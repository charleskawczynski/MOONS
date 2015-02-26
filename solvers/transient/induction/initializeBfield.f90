       module initializeBfield_mod
       use constants_mod
       use myIO_mod
       use grid_mod
       use simParams_mod
       use applyBCs_mod
       implicit none

       private
       public :: initBfield

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       contains

       subroutine initBfield(Bx,By,Bz,Bx0,By0,Bz0,g,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0
         if (restartB) then
           call initRestartBfield(Bx,By,Bz,Bx0,By0,Bz0,g,dir)
         elseif (preDefinedB_ICs.ne.0) then
           call initPreDefinedBfield(Bx,By,Bz,Bx0,By0,Bz0,g)
         else
           call initUserBfield(Bx,By,Bz,Bx0,By0,Bz0,g)
         endif
       end subroutine

       subroutine initRestartBfield(Bx,By,Bz,Bx0,By0,Bz0,g,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0
         real(cp),dimension(:),allocatable :: xc,yc,zc
         real(cp),dimension(:),allocatable :: xn,yn,zn

         allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
         allocate(xn(g%c(1)%sn),yn(g%c(2)%sn),zn(g%c(3)%sn))
         xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc
         xn = g%c(1)%hn; yn = g%c(2)%hn; zn = g%c(3)%hn

         ! Need to write B0 to file then change this to a read statement!
         call uniformBfield(Bx,By,Bz,Bx0,By0,Bz0,applied_B_dir)

         call readFromFile(xc,yc,zc,Bx,By,Bz,dir//'Bfield/','Bxct','Byct','Bzct')
         ! Compute the induced magnetic field (B_total is exprted)
         Bx = Bx - Bx0; By = By - By0; Bz = Bz - Bz0

         deallocate(xn,yn,zn)
         deallocate(xc,yc,zc)
       end subroutine

       subroutine uniformBfield(Bx,By,Bz,Bx0,By0,Bz0,dir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0
         integer,intent(in) :: dir
         Bx = real(0.0,cp); By = real(0.0,cp); Bz = real(0.0,cp)
         select case (dir)
         case (0); Bx0 = real(0.0,cp); By0 = real(0.0,cp); Bz0 = real(0.0,cp)
         case (1); Bx0 = real(1.0,cp); By0 = real(0.0,cp); Bz0 = real(0.0,cp)
         case (2); Bx0 = real(0.0,cp); By0 = real(1.0,cp); Bz0 = real(0.0,cp)
         case (3); Bx0 = real(0.0,cp); By0 = real(0.0,cp); Bz0 = real(1.0,cp)
         end select
       end subroutine

       subroutine initPreDefinedBfield(Bx,By,Bz,Bx0,By0,Bz0,g)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0

         select case (preDefinedB_ICs)
         case (1); call uniformBfield(Bx,By,Bz,Bx0,By0,Bz0,applied_B_dir)
         case (2); call initFringingField(Bx,By,Bz,Bx0,By0,Bz0,g)
         case (3); call initFringingField(Bx,By,Bz,Bx0,By0,Bz0,g)
         case default
           write(*,*) 'Incorrect preDefinedB_ICs case in initBfield.'; stop
         end select

       end subroutine

       subroutine initFringingField(Bx,By,Bz,Bx0,By0,Bz0,g)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0
         real(cp),dimension(:),allocatable :: xc,yc,zc
         real(cp),dimension(:),allocatable :: xn,yn,zn
         integer,dimension(3) :: s
         integer :: i2,i
         real(cp) :: Bstretch,Bshift

         allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
         allocate(xn(g%c(1)%sn),yn(g%c(2)%sn),zn(g%c(3)%sn))
         xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc
         xn = g%c(1)%hn; yn = g%c(2)%hn; zn = g%c(3)%hn

         s = shape(Bz0)
         Bx = real(0.0,cp); By = real(0.0,cp); Bz = real(0.0,cp)
         Bx0 = real(0.0,cp)
         By0 = real(0.0,cp)
         
         ! Sergey's fringe:
         Bstretch = 1.d0   ! stretching parameter
         Bshift = 10.d0    ! shift parameter

         do i=1,s(1)
           Bz0(i,:,:) = (real(1.0,cp)+Dtanh((xc(i)-Bshift)/Bstretch))/2.d0
         enddo
         i2 = 0
         do i=1+(s(1)-1)/2,s(1)
           Bz0(i,:,:) = Bz0(1+(s(1)-1)/2-i2,:,:)
           i2 = i2+1
         enddo

         deallocate(xn,yn,zn)
         deallocate(xc,yc,zc)
       end subroutine

       subroutine initUserBfield(Bx,By,Bz,Bx0,By0,Bz0,g)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0
         real(cp),dimension(:),allocatable :: xc,yc,zc
         real(cp),dimension(:),allocatable :: xn,yn,zn

         allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
         allocate(xn(g%c(1)%sn),yn(g%c(2)%sn),zn(g%c(3)%sn))
         xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc
         xn = g%c(1)%hn; yn = g%c(2)%hn; zn = g%c(3)%hn

         call uniformBfield(Bx,By,Bz,Bx0,By0,Bz0,3)

         deallocate(xn,yn,zn)
         deallocate(xc,yc,zc)
       end subroutine

       end module
