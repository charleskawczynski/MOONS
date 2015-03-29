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


       ! Is this necessary?? Or is the residual BCs = dirichlet/zero

       ! interface restrict;     module procedure restrictBCs;        end interface

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

            if ((mod(c%sc,2).eq.0).and.(s(dir).eq.c%sc)) then; rCase = 1
        elseif ((mod(c%sc,2).eq.0).and.(s(dir).eq.c%sn)) then; rCase = 2
        elseif ((mod(c%sc,2).ne.0).and.(s(dir).eq.c%sc)) then; rCase = 3
        elseif ((mod(c%sc,2).ne.0).and.(s(dir).eq.c%sn)) then; rCase = 4
        else; stop 'Error: no rCase exists.'
        endif

        select case (rCase)
        case (1) ! u {CC}, mod(sc/2,2)=0
          !    Linearly interpolate the average of two cells to the cell center:
          ! or Linearly interpolate the two adjecent cells and average:
          ! Ghost values are linearly extrapolated:
          do i=1+1*x,s(1)-1*x,1+x
           do j=1+1*y,s(2)-1*y,1+y
            do k=1+1*z,s(3)-1*z,1+z
              t = i*x + j*y + k*z
              ! This idea might work BUT
              ! Indexing needs to be checked (since t might need to
              ! be 2*t or 2*t-1 or something)
              ! alpha = (c%hn(t+1)-c%hn(t))/(c%hn(t)-c%hn(t-1))
              ! alpha = c%dhn(t)/c%dhn(t-1)
              alpha = real(0.5,cp)
              r(i*(1-x)+x*i/2+x,j*(1-y)+y*j/2+y,k*(1-z)+z*k/2+z) = real(0.5,cp)*(u(i,j,k) + &
              u(i-x,j-y,k-z)*alpha + &
              u(i+x,j+y,k+z)*(real(1.0,cp)-alpha))
            enddo
           enddo
           ! write(*,*) 'ri,ui-x,ui+x = ',i*(1-x)+x*i/2+x,i-x,i,i+x
          enddo
        stop 'Error: not yet supported'
        case (2) ! u {N},  mod(sc/2,2)=0
          ! Every even becomes the average of the value itself
          ! and its linearly interpolated neighbors:

          ! write(*,*) 'su,sr = ',s(1),sr(1)

          do i=1+1*x,s(1)-1*x,1+x
           do j=1+1*y,s(2)-1*y,1+y
            do k=1+1*z,s(3)-1*z,1+z
              t = i*x + j*y + k*z
              ! This idea might work BUT
              ! Indexing needs to be checked (since t might need to
              ! be 2*t or 2*t-1 or something)
              ! alpha = (c%hn(t+1)-c%hn(t))/(c%hn(t)-c%hn(t-1))
              ! alpha = c%dhn(t)/c%dhn(t-1)
              alpha = real(0.5,cp)
              r(i*(1-x)+x*i/2+x,j*(1-y)+y*j/2+y,k*(1-z)+z*k/2+z) = real(0.5,cp)*(u(i,j,k) + &
              u(i-x,j-y,k-z)*alpha + &
              u(i+x,j+y,k+z)*(real(1.0,cp)-alpha))
            enddo
           enddo
           ! write(*,*) 'ri,ui-x,ui+x = ',i*(1-x)+x*i/2+x,i-x,i,i+x
          enddo
          ! stop 'Finished restricting'

          ! Boundary values, normal to dir, remain the same:
          select case (dir)
          case (1); r(2,:,:) = u(2,:,:); r(sr(1)-1,:,:) = u(s(1)-1,:,:)
          case (2); r(:,2,:) = u(:,2,:); r(:,sr(2)-1,:) = u(:,s(2)-1,:)
          case (3); r(:,:,2) = u(:,:,2); r(:,:,sr(3)-1) = u(:,:,s(3)-1)
          end select
          ! Linearly extrapolate to ghost points
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

        allocate(temp1(sr(1),su(2),su(3)))
        allocate(temp2(sr(1),sr(2),su(3)))

        call restrict(temp1,  u  ,g%c(1),1)
        call restrict(temp2,temp1,g%c(2),2)
        call restrict(  r,  temp2,g%c(2),3)

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

            if ((mod(c%sc,2).eq.0).and.(sp(dir).eq.c%sc)) then; pCase = 1
        elseif ((mod(c%sc,2).eq.0).and.(sp(dir).eq.c%sn)) then; pCase = 2
        elseif ((mod(c%sc,2).ne.0).and.(sp(dir).eq.c%sc)) then; pCase = 3
        elseif ((mod(c%sc,2).ne.0).and.(sp(dir).eq.c%sn)) then; pCase = 4
        else; stop 'Error: no pCase exists.'
        endif

        ! write(*,*) 'shape(u) = ',shape(u)
        ! write(*,*) 'shape(p) = ',shape(p)
        select case (pCase)
        case (1) ! u {CC}, mod(sc/2,2)=0
        stop 'Error: not yet supported'
        case (2) ! u {N},  mod(sc/2,2)=0
          ! Starting from the physical boundaries,
          ! odd locations have coincident values:
          ! Index for p must be even: 2n-2
          ! write(*,*) 'su,sp = ',s(1),sp(1)
          do i=1+x,s(1)-2*x
           do j=1+y,s(2)-2*y
            do k=1+z,s(3)-2*z
              p((1+x)*i-2*x,(1+y)*j-2*y,(1+z)*k-2*z) = u(i,j,k)
            enddo
           enddo
          ! write(*,*) 'pi,ui = ',(1+x)*i-2*x,i
          enddo

          ! Starting from the physical boundaries,
          ! even locations are interpolated:
          ! Index for p must be even: 2n-1
          do i=1,s(1)-x
           do j=1,s(2)-y
            do k=1,s(3)-z
              t = i*x + j*y + k*z
              ! Alpha needs to be fixed
              ! alpha = (c%hn(t)-c%hn(t+1))/(c%hn(t-1)-c%hn(t+1))
              alpha = 0.5
              p((1+x)*i-x,(1+y)*j-y,(1+z)*k-z) = u(i,j,k)*alpha + &
                                   u(i+x,j+y,k+z)*(real(1.0,cp)-alpha)
            enddo
           enddo
          ! write(*,*) 'pi,ui-x,ui = ',(1+x)*i-1*x,i,i+x
          enddo
          ! stop 'Finished prolongating'

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
        allocate(temp1(sp(1),su(2),su(3)))
        allocate(temp2(sp(1),sp(2),su(3)))
        temp1 = 0.0; temp2 = 0.0; p = 0.0

        ! write(*,*) 'maxval(u) = ',maxval(u)
        call prolongate(temp1,  u  ,fg%c(1),1)
        ! write(*,*) 'maxval(temp1) = ',maxval(temp1)
        call prolongate(temp2,temp1,fg%c(2),2)
        ! write(*,*) 'maxval(temp2) = ',maxval(temp2)
        call prolongate(  p  ,temp2,fg%c(3),3)
        ! write(*,*) 'maxval(p) = ',maxval(p)

        deallocate(temp1,temp2)
      end subroutine


      ! **********************************************************
      ! **********************************************************
      ! ******************** RESTRICT BCs ************************
      ! **********************************************************
      ! **********************************************************

!       subroutine setAllBCs(mg,u_bcs,nLevels)
!         implicit none
!         type(multiGrid),dimension(nLevels),intent(inout) :: mg
!         type(BCs),intent(in) :: u_bcs
!         integer :: i
!         mg(1)%u_bcs = u_bcs
!         do i = 1,nLevels
!           call restrict(mg(i+1)%u_bcs , mg(i)%u_bcs, mg(i+1)%g, mg(i)%g)
!         enddo
!       end subroutine

!       subroutine setBCs(g,u_bcs,r_bcs)
!         implicit none
!         type(grid),intent(inout) :: g       ! course grid
!         type(BCs),intent(in) :: u_bcs       ! BCs from finer grid
!         type(coordinates),intent(in) :: c   ! coordinates from finer grid
!         ! Set shape
!         call setAllZero(g%r_bcs,g%s(1),g%s(2),g%s(3),1)
!         ! Set types
!         call setXminType(g%u_bcs,getXminType(u_bcs))
!         call setXmaxType(g%u_bcs,getXmaxType(u_bcs))
!         call setYminType(g%u_bcs,getYminType(u_bcs))
!         call setYmaxType(g%u_bcs,getYmaxType(u_bcs))
!         call setZminType(g%u_bcs,getZminType(u_bcs))
!         call setZmaxType(g%u_bcs,getZmaxType(u_bcs))

!         ! Restrict To next level
!         call restrictBCs(g%u_bcs%XminVals,u_bcs%XminVals,g%c,c,1)
!         call restrictBCs(g%u_bcs%XmaxVals,u_bcs%XmaxVals,g%c,c,1)
!         call restrictBCs(g%u_bcs%YminVals,u_bcs%YminVals,g%c,c,2)
!         call restrictBCs(g%u_bcs%YmaxVals,u_bcs%YmaxVals,g%c,c,2)
!         call restrictBCs(g%u_bcs%ZminVals,u_bcs%ZminVals,g%c,c,3)
!         call restrictBCs(g%u_bcs%ZmaxVals,u_bcs%ZmaxVals,g%c,c,3)
!       end subroutine

!       subroutine restrictBCs(r,u,cr,c,sr,s,dir)
!         implicit none
!         real(cp),dimension(:,:),intent(in) :: u    ! 
!         real(cp),dimension(:,:),intent(inout) :: r ! coarse BCs
!         type(coordinates),intent(in) :: cr,c        ! coarse and fine grid

!         integer,dimension(3) :: su,sr
!         real(cp),dimension(:,:,:) :: temp
!         integer :: s
!         s = shape(r)

!         allocate(temp)

!         su = shape(u); sr = shape(r)
!         allocate(temp1(sr(1),su(2),su(3)))
!         ! Restrict in x if necessary
!         select case (mod(s(1),2))
!           case (0); temp1 = u
!           case (1); call restrictBCs(temp1,u,gd,1)
!         end select

!         allocate(temp2(sr(1),sr(2),su(3)))
!         ! Restrict in y if necessary
!         select case (mod(s(2),2))
!           case (0); temp2 = temp1
!           case (1); call restrict(temp3,temp2,gd,1)
!         end select

!         allocate(temp2(sr(1),sr(2),sr(3)))
!         ! Restrict in z if necessary
!         select case (mod(s(3),2))
!           case (0); r = temp3
!           case (1); call restrict(r,temp3,gd,1)
!         end select
!         deallocate(temp1,temp2,temp3)
!       end subroutine

!       subroutine restrictBCsDirection(r,u,c,dir)
!         implicit none
!         real(cp),dimension(:,:),intent(in) :: u     ! size = s
!         real(cp),dimension(:,:),intent(inout) :: r  ! size = (s-1)/2+1
!         type(coordinates),intent(in) :: c
!         integer,intent(in) :: dir

!         integer,dimension(2) :: s
!         real(cp),dimension(:) :: hn
!         real(cp) :: alpha,t
!         integer :: i,j,k,x,y,z
!         s = shape(u)

!         select case (dir)
!         case (1); x=1;y=0;z=0; allocate(hn(s(1))); hn = c%x
!         case (2); x=0;y=1;z=0; allocate(hn(s(2))); hn = c%y
!         case (3); x=0;y=0;z=1; allocate(hn(s(3))); hn = c%z
!         end select

!         ! Every odd becomes the average of the value itself
!         ! and its linearly interpolated neighbors:
!         select case (dir)
!         case (1);    do j=1+2*y,s(1)-2*y,1+y
!                       do k=1+2*z,s(2)-2*z,1+z
!                         t = j*y + k*z
!                         alpha = (hn(t)-hn(t+1))/(hn(t-1)-hn(t+1))
!                         r((j+1)/2,(k+1)/2) = 0.5*(u(j,k) + &
!                         u(j-y,k-z)*alpha + &
!                         u(j+y,k+z)*(1.0-alpha))
!                       enddo
!                      enddo
!         case (2);    do i=1+2*x,s(1)-2*x,1+x
!                       do k=1+2*z,s(2)-2*z,1+z
!                         t = i*x + k*z
!                         alpha = (hn(t)-hn(t+1))/(hn(t-1)-hn(t+1))
!                         r((i+1)/2,(k+1)/2) = 0.5*(u(i,k) + &
!                         u(i-x,k-z)*alpha + &
!                         u(i+x,k+z)*(1.0-alpha))
!                       enddo
!                      enddo
!         case (3);    do i=1+2*x,s(1)-2*x,1+x
!                       do j=1+2*y,s(2)-2*y,1+y
!                         t = i*x + j*y
!                         alpha = (hn(t)-hn(t+1))/(hn(t-1)-hn(t+1))
!                         r((i+1)/2,(j+1)/2) = 0.5*(u(i,j) + &
!                         u(i-x,j-y)*alpha + &
!                         u(i+x,j+y)*(1.0-alpha))
!                       enddo
!                      enddo
!         end select
!         ! Boundary values, normal to dir, remain the same:
!         select case (dir)
!         case (1); r(1,:,:) = u(1,:,:); r((s(1)-1)/2+1,:,:) = u(s(1),:,:)
!         case (2); r(:,1,:) = u(:,1,:); r(:,(s(2)-1)/2+1,:) = u(:,s(2),:)
!         case (3); r(:,:,1) = u(:,:,1); r(:,:,(s(3)-1)/2+1) = u(:,:,s(3))
!         end select
!         select case (dir)
!         case (1); r(1,:,:) = u(1,:,:); r((s(1)-1)/2+1,:,:) = u(s(1),:,:)
!         case (2); r(:,1,:) = u(:,1,:); r(:,(s(2)-1)/2+1,:) = u(:,s(2),:)
!         case (3); r(:,:,1) = u(:,:,1); r(:,:,(s(3)-1)/2+1) = u(:,:,s(3))
!         end select
!       end subroutine

       end module