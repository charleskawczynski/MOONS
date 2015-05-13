       module MG_tools_mod
       use coordinates_mod
       use grid_mod
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

       interface restrict;     module procedure restrictField1D;    end interface
       interface restrict;     module procedure restrictField;      end interface

       interface prolongate;   module procedure prolongateField1D;  end interface
       interface prolongate;   module procedure prolongateField;    end interface

       contains

      ! **********************************************************
      ! **********************************************************
      ! ******************* RESTRICT FIELD ***********************
      ! **********************************************************
      ! **********************************************************

      subroutine restrictField1D(r,u,c,dir)
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
        integer,intent(in) :: dir
        integer,dimension(3) :: s,sr
        real(cp) :: alpha
        integer :: i,j,k,t,x,y,z,rCase
        s = shape(u); sr = shape(r)

        select case (dir)
        case (1); x=1;y=0;z=0
        case (2); x=0;y=1;z=0
        case (3); x=0;y=0;z=1
        case default
        stop 'Error: dir must = 1,2,3'
        end select

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
              r(i*(1-x)+x*i/2+x,j*(1-y)+y*j/2+y,k*(1-z)+z*k/2+z) = real(0.5,cp)*(u(i,j,k) + &
              u(i-x,j-y,k-z)*alpha + &
              u(i+x,j+y,k+z)*(real(1.0,cp)-alpha))
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
          case (1); r(1,:,:) = real(2.0,cp)*r(2,:,:)-r(3,:,:)
          case (2); r(:,1,:) = real(2.0,cp)*r(:,2,:)-r(:,3,:)
          case (3); r(:,:,1) = real(2.0,cp)*r(:,:,2)-r(:,:,3)
          end select
          select case (dir)
          case (1); r(sr(1),:,:) = real(2.0,cp)*r(sr(1)-1,:,:)-r(sr(1)-2,:,:)
          case (2); r(:,sr(2),:) = real(2.0,cp)*r(:,sr(2)-1,:)-r(:,sr(2)-2,:)
          case (3); r(:,:,sr(3)) = real(2.0,cp)*r(:,:,sr(3)-1)-r(:,:,sr(3)-2)
          end select
          ! write(*,*) 'r(s) = ',(s(1)-1)/2+1
          ! stop 'Restricted indexes printed'
        case (3) ! u {CC}, mod(sc/2,2)≠0 (bad)
        stop 'Error: not yet supported'
        case (4) ! u {N},  mod(sc/2,2)≠0 (bad)
        stop 'Error: not yet supported'
        end select
      end subroutine

      subroutine restrictField(r,u,g)
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
        type(grid),intent(in) :: g                   ! fine coordinates
        integer,dimension(3) :: su,sr
        real(cp),dimension(:,:,:),allocatable :: temp1,temp2

        su = shape(u); sr = shape(r)

        allocate(temp1(sr(1),su(2),su(3))); temp1 = real(0.0,cp)
        allocate(temp2(sr(1),sr(2),su(3))); temp2 = real(0.0,cp)
        r = real(0.0,cp)

        call restrict(temp1,  u  ,g%c(1),1)
        call restrict(temp2,temp1,g%c(2),2)
        call restrict(  r,  temp2,g%c(3),3)

        deallocate(temp1,temp2)
      end subroutine


      ! **********************************************************
      ! **********************************************************
      ! ****************** PROLONGATE FIELD **********************
      ! **********************************************************
      ! **********************************************************

      subroutine prolongateField1D(p,u,c,dir)
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
        integer,intent(in) :: dir
        integer,dimension(3) :: s,sp
        real(cp) :: alpha
        integer :: i,j,k,t,x,y,z,pCase
        s = shape(u); sp = shape(p)

        select case (dir)
        case (1); x=1;y=0;z=0
        case (2); x=0;y=1;z=0
        case (3); x=0;y=0;z=1
        case default
        stop 'Error: dir must = 1,2,3'
        end select

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
                                   u(i+x,j+y,k+z)*(real(1.0,cp)-alpha)
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

      subroutine prolongateField(p,u,fg)
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
        type(grid),intent(in) :: fg                  ! fine grid
        real(cp),dimension(:,:,:),allocatable :: temp1,temp2
        integer,dimension(3) :: su,sp
        su = shape(u); sp = shape(p)

        allocate(temp1(sp(1),su(2),su(3))); temp1 = real(0.0,cp)
        allocate(temp2(sp(1),sp(2),su(3))); temp2 = real(0.0,cp)
        p = real(0.0,cp)

        call prolongate(temp1,  u  ,fg%c(1),1)
        call prolongate(temp2,temp1,fg%c(2),2)
        call prolongate(  p  ,temp2,fg%c(3),3)

        deallocate(temp1,temp2)
      end subroutine

       end module