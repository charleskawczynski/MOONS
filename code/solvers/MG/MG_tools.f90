       module MG_tools_mod
       use coordinates_mod
       use grid_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       implicit none

       ! Compiler flags: ( _DEBUG_INTERP_ , fopenmp )
       ! 
       ! NOTE: Indexes have not been ordered for speed yet

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: restrict
       public :: prolongate

       interface restrict;     module procedure restrictField1D_RF;    end interface
       interface restrict;     module procedure restrictField_SF;      end interface
       interface restrict;     module procedure restrictField_SF_slow; end interface
       interface restrict;     module procedure restrictField_VF;      end interface

       interface prolongate;   module procedure prolongateField1D_RF;  end interface
       interface prolongate;   module procedure prolongateField_SF;    end interface

       contains

      ! **********************************************************
      ! **********************************************************
      ! ******************* RESTRICT FIELD ***********************
      ! **********************************************************
      ! **********************************************************

      subroutine restrictField1D_RF(r,u,c,dir,s,sr,x,y,z)
        ! This routine restricts the field u {fine grid} to r {coarse grid}
        ! There are 4 possible scenarios
        ! 
        ! Case (1): u {CC}, mod(sc/2,2)=0
        !      (2): u {N},  mod(sc/2,2)=0
        !      (3): u {CC}, mod(sc/2,2)≠0 (bad)
        !      (4): u {N},  mod(sc/2,2)≠0 (bad)
        ! 
        ! These cases are determined internally. This 
        ! restriction corresponds to the same restriction 
        ! as the restrict routine in coordinates.f90.
        ! 
        implicit none
        real(cp),dimension(:,:,:),intent(in) :: u     ! fine field
        real(cp),dimension(:,:,:),intent(inout) :: r  ! restricted field
        type(coordinates),intent(in) :: c            ! fine coordinates
        integer,intent(in) :: dir,x,y,z
        integer,dimension(3),intent(in) :: s,sr
        real(cp) :: alpha
        integer :: i,j,k,t,rCase

            if ((mod(c%sc,2).eq.0).and.(s(dir).eq.c%sc)) then; rCase = 1 ! supported
        elseif ((mod(c%sc,2).eq.0).and.(s(dir).eq.c%sn)) then; rCase = 2 ! supported
        elseif ((mod(c%sc,2).ne.0).and.(s(dir).eq.c%sc)) then; rCase = 3 ! not supported
        elseif ((mod(c%sc,2).ne.0).and.(s(dir).eq.c%sn)) then; rCase = 4 ! not supported
        else; stop 'Error: no rCase exists.'
        endif

        select case (rCase)
        case (1) ! u {CC}, mod(sc/2,2)=0
          !    Linearly interpolate the average of two cells to the cell center:
          ! or Linearly interpolate the two adjecent cells and average:
          ! Ghost values are linearly extrapolated:

          ! write(*,*) 'su,sr = ',s(1),sr(1)

          do k=1+1*z,s(3)-1*z,1+z
           do j=1+1*y,s(2)-1*y,1+y
            do i=1+1*x,s(1)-1*x,1+x
              t = i*x + j*y + k*z
              alpha = c%dhn(t)/(c%dhn(t)+c%dhn(t+1))
              r(i*(1-x)+x*i/2+x,j*(1-y)+y*j/2+y,k*(1-z)+z*k/2+z) = &
              u(i,j,k)*alpha + &
              u(i+x,j+y,k+z)*(real(1.0,cp)-alpha)
            enddo
           enddo
           ! write(*,*) 'ri,ui,ui+x = ',i*(1-x)+x*i/2+x,i,i+x
          enddo
          ! stop 'printed CC restriction'
          
          ! ! Linearly extrapolate to ghost points
          ! ! Is this necessary? Ghost nodes are ALWAYS defined in the smoother
          ! ! so why define them here? BUT this is a restriction operator.
          ! select case (dir)
          ! case (1); r(1,:,:) = real(2.0,cp)*r(2,:,:)-r(3,:,:)
          ! case (2); r(:,1,:) = real(2.0,cp)*r(:,2,:)-r(:,3,:)
          ! case (3); r(:,:,1) = real(2.0,cp)*r(:,:,2)-r(:,:,3)
          ! end select
          ! select case (dir)
          ! case (1); r(sr(1),:,:) = real(2.0,cp)*r(sr(1)-1,:,:)-r(sr(1)-2,:,:)
          ! case (2); r(:,sr(2),:) = real(2.0,cp)*r(:,sr(2)-1,:)-r(:,sr(2)-2,:)
          ! case (3); r(:,:,sr(3)) = real(2.0,cp)*r(:,:,sr(3)-1)-r(:,:,sr(3)-2)
          ! end select

        case (2) ! u {N},  mod(sc/2,2)=0
          ! Starting from the physical boundary,
          ! Every odd becomes the average of the value itself
          ! and its linearly interpolated neighbors:

          ! write(*,*) 'su,sr = ',s(1),sr(1)

          do k=1+1*z,s(3)-1*z,1+z
           do j=1+1*y,s(2)-1*y,1+y
            do i=1+1*x,s(1)-1*x,1+x
              t = i*x + j*y + k*z
              alpha = c%dhn(t)/(c%dhn(t)+c%dhn(t-1))
              r(i*(1-x)+x*i/2+x,j*(1-y)+y*j/2+y,k*(1-z)+z*k/2+z) = 0.5_cp*(u(i,j,k) + &
              u(i-x,j-y,k-z)*alpha + &
              u(i+x,j+y,k+z)*(1.0_cp-alpha))
            enddo
           enddo
           ! write(*,*) 'ri,ui-x,ui+x = ',i*(1-x)+x*i/2+x,i-x,i,i+x
          enddo
          ! stop 'Finished restricting'

          ! Boundary values, along dir, remain the same:
          select case (dir)
          case (1); r(2,:,:) = u(2,:,:); r(sr(1)-1,:,:) = u(s(1)-1,:,:)
          case (2); r(:,2,:) = u(:,2,:); r(:,sr(2)-1,:) = u(:,s(2)-1,:)
          case (3); r(:,:,2) = u(:,:,2); r(:,:,sr(3)-1) = u(:,:,s(3)-1)
          end select

          ! Linearly extrapolate to ghost points
          ! Is this necessary? Ghost nodes are ALWAYS defined in the smoother
          ! so why define them here? BUT this is a restriction operator.
          select case (dir)
          case (1); r(1,:,:) = 2.0_cp*r(2,:,:)-r(3,:,:)
          case (2); r(:,1,:) = 2.0_cp*r(:,2,:)-r(:,3,:)
          case (3); r(:,:,1) = 2.0_cp*r(:,:,2)-r(:,:,3)
          end select
          select case (dir)
          case (1); r(sr(1),:,:) = 2.0_cp*r(sr(1)-1,:,:)-r(sr(1)-2,:,:)
          case (2); r(:,sr(2),:) = 2.0_cp*r(:,sr(2)-1,:)-r(:,sr(2)-2,:)
          case (3); r(:,:,sr(3)) = 2.0_cp*r(:,:,sr(3)-1)-r(:,:,sr(3)-2)
          end select
          ! write(*,*) 'r(s) = ',(s(1)-1)/2+1
          ! stop 'Restricted indexes printed'
        case (3) ! u {CC}, mod(sc/2,2)≠0 (bad)
        stop 'Error: not yet supported'
        case (4) ! u {N},  mod(sc/2,2)≠0 (bad)
        stop 'Error: not yet supported'
        end select
      end subroutine

      subroutine restrictField_SF(r,u,m,temp1,temp2)
        ! This routine restricts the field u {fine grid} to r {coarse grid}
        ! There are 4 possible scenarios
        ! 
        ! Case (1): u {CC}, mod(sc/2,2)=0
        !      (2): u {N},  mod(sc/2,2)=0
        !      (3): u {CC}, mod(sc/2,2)≠0 (bad)
        !      (4): u {N},  mod(sc/2,2)≠0 (bad)
        ! 
        ! These cases are determined internally. This 
        ! restriction corresponds to the same restriction 
        ! as the restrict routine in coordinates.f90.
        ! 
        implicit none
        type(SF),intent(in) :: u     ! fine field
        type(SF),intent(inout) :: r  ! restricted field
        type(mesh),intent(in) :: m   ! fine coordinates
        type(SF),intent(inout) :: temp1,temp2
        integer :: i
        do i=1,u%s
          call restrict(temp1%RF(i)%f,  u%RF(i)%f  ,m%g(i)%c(1),1,u%RF(i)%s    ,temp1%RF(i)%s,1,0,0)
          call restrict(temp2%RF(i)%f,temp1%RF(i)%f,m%g(i)%c(2),2,temp1%RF(i)%s,temp2%RF(i)%s,0,1,0)
          call restrict(  r%RF(i)%f,  temp2%RF(i)%f,m%g(i)%c(3),3,temp2%RF(i)%s,r%RF(i)%s    ,0,0,1)
        enddo
      end subroutine

      subroutine restrictField_SF_slow(r,u,m,m_rx,m_rxy)
        ! This routine is specifically designed 
        ! for the coefficient, sigma.
        implicit none
        type(SF),intent(in) :: u
        type(SF),intent(inout) :: r
        type(mesh),intent(in) :: m,m_rx,m_rxy
        type(SF) :: temp1,temp2
        ! if (u%is_Face) call init_Face(temp1,g_rx,u%FaceDir)
        if (u%is_CC)   call init_CC(temp1,m_rx)
        if (u%is_Node) call init_Node(temp1,m_rx)
        ! if (u%is_Edge) call init_Edge(temp1,g_rx,u%EdgeDir)

        ! if (u%is_Face) call init_Face(temp2,g_rxy,u%FaceDir)
        if (u%is_CC)   call init_CC(temp2,m_rxy)
        if (u%is_Node) call init_Node(temp2,m_rxy)
        ! if (u%is_Edge) call init_Edge(temp2,g_rxy,u%EdgeDir)
        call restrict(r,u,m,temp1,temp2)
        call delete(temp1)
        call delete(temp2)
      end subroutine

      subroutine restrictField_VF(r,u,m,m_rx,m_rxy)
        implicit none
        type(VF),intent(in) :: u               ! fine field
        type(VF),intent(inout) :: r            ! restricted field
        type(mesh),intent(in) :: m,m_rx,m_rxy  ! fine coordinates
        call restrict(r%x,u%x,m,m_rx,m_rxy)
        call restrict(r%y,u%y,m,m_rx,m_rxy)
        call restrict(r%z,u%z,m,m_rx,m_rxy)
      end subroutine

      ! **********************************************************
      ! **********************************************************
      ! ****************** PROLONGATE FIELD **********************
      ! **********************************************************
      ! **********************************************************

      subroutine prolongateField1D_RF(p,u,c,dir,s,sp,x,y,z)
        ! This routine prolongates the field u {coarse grid} to p {fine grid}
        ! There are 4 possible scenarios
        ! 
        ! Case (1): u {CC}, mod(sc/2,2)=0
        !      (2): u {N},  mod(sc/2,2)=0
        !      (3): u {CC}, mod(sc/2,2)≠0 (bad)
        !      (4): u {N},  mod(sc/2,2)≠0 (bad)
        ! 
        implicit none
        real(cp),dimension(:,:,:),intent(in) :: u    ! size = s
        real(cp),dimension(:,:,:),intent(inout) :: p ! size = 2*s-1
        type(coordinates),intent(in) :: c            ! coordinates of fine grid
        integer,intent(in) :: dir,x,y,z
        integer,dimension(3),intent(in) :: s,sp
        real(cp) :: alpha
        integer :: i,j,k,t,pCase

            if ((mod(c%sc,2).eq.0).and.(sp(dir).eq.c%sc)) then; pCase = 1 ! supported
        elseif ((mod(c%sc,2).eq.0).and.(sp(dir).eq.c%sn)) then; pCase = 2 ! supported
        elseif ((mod(c%sc,2).ne.0).and.(sp(dir).eq.c%sc)) then; pCase = 3 ! not supported
        elseif ((mod(c%sc,2).ne.0).and.(sp(dir).eq.c%sn)) then; pCase = 4 ! not supported
        else; stop 'Error: no pCase exists.'
        endif

        ! write(*,*) 'su,sp = ',s(1),sp(1)
        select case (pCase)
        case (1) ! u {CC}, mod(sc/2,2)=0

          ! Starting from the physical boundaries,
          ! odd locations have weighting factors according
          ! to their cell size:
          do k=1+z,s(3)-z
           do j=1+y,s(2)-y
            do i=1+x,s(1)-x
              t = i*x + j*y + k*z
              alpha = c%dhn(t)/(c%dhn(t)+c%dhn(t+1))
              p((1+x)*i-2*x,(1+y)*j-2*y,(1+z)*k-2*z) = u(i,j,k)*alpha
            enddo
           enddo
          ! write(*,*) 'pi,ui,ui+x,t,alpha = ',(1+x)*i-2*x,i,t,alpha
          enddo
          ! write(*,*) '----- odd locations -----'

          ! Starting from the physical boundaries,
          ! even locations have weighting factors according
          ! to their cell size:
          do k=1+z,s(3)-z
           do j=1+y,s(2)-y
            do i=1+x,s(1)-x
              t = i*x + j*y + k*z
              alpha = c%dhn(t+1)/(c%dhn(t)+c%dhn(t+1))
              p((1+x)*i-x,(1+y)*j-y,(1+z)*k-z) = u(i,j,k)*alpha
            enddo
           enddo
          ! write(*,*) 'pi,ui,ui+x,t,alpha = ',(1+x)*i-x,i,t,alpha
          enddo
          ! stop 'Printed prolongation'

        case (2) ! u {N},  mod(sc/2,2)=0
          ! Starting from the physical boundaries,
          ! odd locations have coincident values:
          ! Index for p must be even: 2n-2
          ! write(*,*) 'su,sp = ',s(1),sp(1)
          do k=1+z,s(3)-2*z
           do j=1+y,s(2)-2*y
            do i=1+x,s(1)-2*x
              p((1+x)*i-2*x,(1+y)*j-2*y,(1+z)*k-2*z) = u(i,j,k)
            enddo
           enddo
          ! write(*,*) 'pi,ui = ',(1+x)*i-2*x,i
          enddo
          ! write(*,*) '----- odd locations -----'

          ! Starting from the physical boundaries,
          ! even locations are interpolated:
          ! Index for p must be even: 2n-1
          do k=1,s(3)-z
           do j=1,s(2)-y
            do i=1,s(1)-x
              t = i*x + j*y + k*z
              ! Alpha needs to be fixed
              alpha = c%dhn(t+1)/(c%dhn(t+1)+c%dhn(t))
              p((1+x)*i-x,(1+y)*j-y,(1+z)*k-z) = u(i,j,k)*alpha + &
                                   u(i+x,j+y,k+z)*(1.0_cp-alpha)
            enddo
           enddo
          ! write(*,*) 'pi,ui-x,ui = ',(1+x)*i-1*x,i,i+x
          enddo
          ! stop 'Printed prolongating'

        case (3) ! u {CC}, mod(sc/2,2)≠0 (bad)
        stop 'Error: not yet supported'
        case (4) ! u {N},  mod(sc/2,2)≠0 (bad)
        stop 'Error: not yet supported'
        end select
      end subroutine

      subroutine prolongateField_SF(p,u,mf,temp1,temp2)
        ! This routine prolongates the field u {coarse grid} to p {fine grid}
        ! There are 4 possible scenarios
        ! 
        ! Case (1): u {CC}, mod(sc/2,2)=0
        !      (2): u {N},  mod(sc/2,2)=0
        !      (3): u {CC}, mod(sc/2,2)≠0 (bad)
        !      (4): u {N},  mod(sc/2,2)≠0 (bad)
        ! 
        implicit none
        type(SF),intent(in) :: u    ! size = s
        type(SF),intent(inout) :: p ! size = 2*s-1
        type(mesh),intent(in) :: mf ! fine grid
        type(SF),intent(inout) :: temp1,temp2
        integer :: i
        do i=1,u%s
          call prolongate(temp1%RF(i)%f,  u%RF(i)%f  ,mf%g(i)%c(1),1,u%RF(i)%s    ,temp1%RF(i)%s,1,0,0)
          call prolongate(temp2%RF(i)%f,temp1%RF(i)%f,mf%g(i)%c(2),2,temp1%RF(i)%s,temp2%RF(i)%s,0,1,0)
          call prolongate(  p%RF(i)%f,  temp2%RF(i)%f,mf%g(i)%c(3),3,temp2%RF(i)%s,p%RF(i)%s    ,0,0,1)
        enddo
      end subroutine

       end module