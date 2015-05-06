       module initializeBfield_mod
       use IO_vectorFields_mod
       use IO_vectorBase_mod
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

       integer,parameter :: applied_B_dir = 4
       !                                    0 : No applied field: B0 = (0,0,0)
       !                                    1 :    Applied field: B0 = (B0x,0,0)
       !                                    2 :    Applied field: B0 = (0,B0y,0)
       !                                    3 :    Applied field: B0 = (0,0,B0z)
       !                                    4 :    Applied field: B0 = (B0x,B0y,B0z)


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

       subroutine initBfield(Bx,By,Bz,B0x,B0y,B0z,g,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz,B0x,B0y,B0z
         if (restartB) then
           call initRestartB(Bx,By,Bz,g,dir)
           call initRestartB0(B0x,B0y,B0z,g,dir)
           ! call initPreDefinedB0(B0x,B0y,B0z,g)
         elseif (preDefinedB_ICs.ne.0) then
           call initZeroField(Bx,By,Bz)
           call initPreDefinedB0(B0x,B0y,B0z,g)
         else
           call initUserBfield(Bx,By,Bz,B0x,B0y,B0z,g)
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

       subroutine initRestartB0(Bx,By,Bz,g,dir)
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
         call readFromFile(xc,yc,zc,Bx,By,Bz,dir//'Bfield/','B0xct','B0yct','B0zct')
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

       subroutine initFringingField_Sergey(Bx,By,Bz,g,applied_dir,fringeDir)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz
         integer,intent(in) :: applied_dir,fringeDir
         select case (applied_dir)
         case (1); call initFringe_Sergey(Bx,g,fringeDir)
         case (2); call initFringe_Sergey(By,g,fringeDir)
         case (3); call initFringe_Sergey(Bz,g,fringeDir)
         case default
         stop 'Error: applied_dir must = 1,2,3 in initFringingField_Sergey.'
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
         real(cp) :: d
         real(cp) :: Bstretch,Bshift

         s = shape(B)
         allocate(Btemp(s(dir)))
         ! Sergey's fringe:
         Bstretch = real(0.2,cp)   ! stretching parameter
         Bshift = real(1.5,cp)     ! shift parameter

         do i=1,s(dir)
           d = dble((g%c(dir)%hc(i)-Bshift)/Bstretch)
           Btemp(i) = (real(1.0,cp)+tanh(d))/real(2.0,cp)
         enddo; i2 = 0
         ! write(*,*) 's(dir) = ',s(dir)
         do i=1+(s(dir)-1)/2,s(dir)
           ! write(*,*) 'i = ',i
           ! write(*,*) 'iBtemp = ',1+(s(dir)-1)/2-i2
           Btemp(i) = Btemp(1+(s(dir)+1)/2-i2); i2 = i2+1
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
         real(cp) :: d
         integer :: i
         real(cp) :: Bstretch,Bshift

         s = shape(B0)
         allocate(Btemp(s(dir)))
         ! Sergey's fringe:
         Bstretch = real(0.45,cp)   ! stretching parameter
         Bshift = real(12.5,cp)     ! shift parameter

         do i=1,s(dir)
           d = dble(g%c(dir)%hc(i)-Bshift*Bstretch)
           Btemp(i) = real(0.5,cp)*(real(1.0,cp)-tanh(d))
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

       subroutine initUserBfield(Bx,By,Bz,B0x,B0y,B0z,g)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz,B0x,B0y,B0z
         real(cp),dimension(:),allocatable :: xc,yc,zc

         allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
         xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc

         call initZeroField(Bx,By,Bz)
         call uniformBfield(B0x,B0y,B0z,3)

         deallocate(xc,yc,zc)
       end subroutine

       end module
