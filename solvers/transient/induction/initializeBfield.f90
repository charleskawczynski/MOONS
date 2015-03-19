       module initializeBfield_mod
       use myIO_mod
       use grid_mod
       implicit none

       private
       public :: initBfield

       logical,parameter :: restartB = .false. ! (induced field)
       ! NOTE: - The applied field cannot (and probably should not) be restarted
       !       - By default, preDefinedB_ICs is used to define the applied field

       integer,parameter :: preDefinedB_ICs = 1 ! NOTE: All cases use B_induced = 0
       !                                      0 : User-defined case (no override)
       !                                      1 : Uniform applied (set applied_B_dir)
       !                                      2 : Fringing Magnetic field (Sergey's fringe, up, const, down)
       !                                      3 : Fringing Magnetic field (ALEX experiments)
       !                                      4 : Fringing Magnetic field (consistent, not yet implemented)

       integer,parameter :: fringe_dir = 1 ! Direction along which to fringe
       !                                 1 : B0(x,:,:)
       !                                 2 : B0(:,y,:)
       !                                 3 : B0(:,:,z)

       integer,parameter :: applied_B_dir = 3
       !                                    0 : No applied field:      B0 = (0,0,0)
       !                                    1 : Uniform applied field: B0 = (1,0,0)
       !                                    2 : Uniform applied field: B0 = (0,1,0)
       !                                    3 : Uniform applied field: B0 = (0,0,1)
       !                                    4 : Uniform applied field: B0 = (1,1,1)


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
           call initRestartB(Bx,By,Bz,g,dir)
           call initPreDefinedB0(Bx0,By0,Bz0,g)
         elseif (preDefinedB_ICs.ne.0) then
           call initZeroField(Bx,By,Bz)
           call initPreDefinedB0(Bx0,By0,Bz0,g)
         else
           call initUserBfield(Bx,By,Bz,Bx0,By0,Bz0,g)
         endif
       end subroutine

       subroutine initRestartB(Bx,By,Bz,g,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz
         real(cp),dimension(:),allocatable :: xc,yc,zc
         real(cp),dimension(:),allocatable :: xn,yn,zn
         allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
         allocate(xn(g%c(1)%sn),yn(g%c(2)%sn),zn(g%c(3)%sn))
         xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc
         xn = g%c(1)%hn; yn = g%c(2)%hn; zn = g%c(3)%hn
         call readFromFile(xc,yc,zc,Bx,By,Bz,dir//'Bfield/','Bxct','Byct','Bzct')
         deallocate(xn,yn,zn)
         deallocate(xc,yc,zc)
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
         ! 
         ! This set of routines need to be organized to be more clear.
         ! Right now, the total B-field is exported, which means
         ! when it is imported for restart, the applied must be subtracted,
         ! which is confusion and dangerous due to roundoff errors.

         call initZeroField(Bx,By,Bz)
         ! call uniformBfield(Bx0,By0,Bz0,applied_B_dir)
         call initPreDefinedB0(Bx0,By0,Bz0,g)

         call readFromFile(xc,yc,zc,Bx,By,Bz,dir//'Bfield/','Bxct','Byct','Bzct')
         ! call readFromFile(xc,yc,zc,Bx0,By0,Bz0,dir//'Bfield/','B0xct','B0yct','B0zct')

         ! Compute the induced magnetic field (B_total is exprted)
         Bx = Bx - Bx0; By = By - By0; Bz = Bz - Bz0

         deallocate(xn,yn,zn)
         deallocate(xc,yc,zc)
       end subroutine

       subroutine initPreDefinedB0(Bx,By,Bz,g)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz
         select case (preDefinedB_ICs)
         case (1); call uniformBfield(Bx,By,Bz,applied_B_dir)
         case (2); call initFringingField_Sergey(Bx,By,Bz,g,applied_B_dir,fringe_dir)
         case (3); call initFringingField_ALEX(Bx,By,Bz,g,applied_B_dir,fringe_dir)
         case default
           write(*,*) 'Incorrect preDefinedB_ICs case in initBfield.'; stop
         end select
       end subroutine

       subroutine uniformBfield(Bx,By,Bz,dir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz
         integer,intent(in) :: dir
         select case (dir)
         case (0); Bx = real(0.0,cp); By = real(0.0,cp); Bz = real(0.0,cp)
         case (1); Bx = real(1.0,cp); By = real(0.0,cp); Bz = real(0.0,cp)
         case (2); Bx = real(0.0,cp); By = real(1.0,cp); Bz = real(0.0,cp)
         case (3); Bx = real(0.0,cp); By = real(0.0,cp); Bz = real(1.0,cp)
         case (4); Bx = real(1.0,cp); By = real(1.0,cp); Bz = real(1.0,cp)
         case default
         stop 'Error: dir must = 1,2,3 in uniformBfield.'
         end select
       end subroutine

       subroutine initZeroField(Bx,By,Bz)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz
         Bx = real(0.0,cp); By = real(0.0,cp); Bz = real(0.0,cp)
       end subroutine

       subroutine initFringingField_Sergey(Bx,By,Bz,g,dir,fringeDir)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz
         integer,intent(in) :: dir,fringeDir
         select case (dir)
         case (1); call initFringe_Sergey(Bx,g,fringeDir)
         case (2); call initFringe_Sergey(By,g,fringeDir)
         case (3); call initFringe_Sergey(Bz,g,fringeDir)
         case default
         stop 'Error: dir must = 1,2,3 in initFringingField_Sergey.'
         end select
       end subroutine

       subroutine initFringe_Sergey(B,g,dir)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: B
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: Btemp
         integer,dimension(3) :: s
         integer :: i2,i
         real(cp) :: Bstretch,Bshift

         s = shape(B)
         allocate(Btemp(s(dir)))
         ! Sergey's fringe:
         Bstretch = real(1.0,cp)   ! stretching parameter
         Bshift = real(10.0,cp)    ! shift parameter

         do i=1,s(dir)
           Btemp(i) = (real(1.0,cp)+dtanh((g%c(dir)%hc(i)-Bshift)/Bstretch))/real(2.0,cp)
         enddo; i2 = 0
         do i=1+(s(dir)-1)/2,s(dir)
           Btemp(i) = Btemp(1+(s(dir)-1)/2-i2); i2 = i2+1
         enddo

         select case (dir)
         case (1); do i=1,s(dir); B(i,:,:) = Btemp(i); enddo
         case (2); do i=1,s(dir); B(:,i,:) = Btemp(i); enddo
         case (3); do i=1,s(dir); B(:,:,i) = Btemp(i); enddo
         case default
         stop 'Error: dir must = 1,2,3 in initFringe_Sergey.'
         end select

         deallocate(Btemp)
       end subroutine

       subroutine initFringingField_ALEX(Bx,By,Bz,g,dir,fringeDir)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz
         integer,intent(in) :: dir,fringeDir
         select case (dir)
         case (1); call initFringe_ALEX(Bx,g,fringeDir)
         case (2); call initFringe_ALEX(By,g,fringeDir)
         case (3); call initFringe_ALEX(Bz,g,fringeDir)
         case default
         stop 'Error: dir must = 1,2,3 in initFringingField_ALEX.'
         end select
       end subroutine

       subroutine initFringe_ALEX(B0,g,dir)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: B0
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: Btemp
         integer,dimension(3) :: s
         integer :: i
         real(cp) :: Bstretch,Bshift

         s = shape(B0)
         allocate(Btemp(s(dir)))
         ! Sergey's fringe:
         Bstretch = real(0.45,cp)   ! stretching parameter
         Bshift = real(12.5,cp)     ! shift parameter

         do i=1,s(dir)
           Btemp(i) = real(0.5,cp)*(real(1.0,cp)-dtanh((g%c(dir)%hc(i)-Bshift)*Bstretch))
         enddo

         select case (dir)
         case (1); do i=1,s(dir); B0(i,:,:) = Btemp(i); enddo
         case (2); do i=1,s(dir); B0(:,i,:) = Btemp(i); enddo
         case (3); do i=1,s(dir); B0(:,:,i) = Btemp(i); enddo
         case default
         stop 'Error: dir must = 1,2,3 in initFringe.'
         end select
         write(*,*) 'Hello!',maxval(Btemp)
         deallocate(Btemp)
       end subroutine

       subroutine initUserBfield(Bx,By,Bz,Bx0,By0,Bz0,g)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0
         real(cp),dimension(:),allocatable :: xc,yc,zc

         allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
         xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc

         call initZeroField(Bx,By,Bz)
         call uniformBfield(Bx0,By0,Bz0,3)

         deallocate(xc,yc,zc)
       end subroutine

       end module
