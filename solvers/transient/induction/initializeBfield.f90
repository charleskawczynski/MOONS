       module initializeBfield_mod
       use IO_vectorFields_mod
       use IO_vectorBase_mod
       use grid_mod
       use vectorField_mod
       implicit none

       private
       public :: initBfield
       public :: restartB

       logical,parameter :: restartB = .false. ! (induced field)
       ! NOTE: - The applied field cannot (and probably should not) be restarted
       !       - By default, preDefinedB_ICs is used to define the applied field

       integer,parameter :: preDefinedB_ICs = 4 ! NOTE: All cases use B_induced = 0
       !                                      0 : User-defined case (no override)
       !                                      1 : Uniform applied (set applied_B_dir)
       !                                      2 : Fringing Magnetic field (Sergey's fringe, up, const, down)
       !                                      3 : Fringing Magnetic field (ALEX experiments)
       !                                      4 : Periodic (Bandaru)

       integer,parameter :: fringe_dir = 1 ! Direction along which to fringe
       !                                 1 : B0(x,:,:)
       !                                 2 : B0(:,y,:)
       !                                 3 : B0(:,:,z)

       integer,parameter :: applied_B_dir = 3
       !                                    0 : No applied field: B0 = (0,0,0)
       !                                    1 :    Applied field: B0 = (B0x,0,0)
       !                                    2 :    Applied field: B0 = (0,B0y,0)
       !                                    3 :    Applied field: B0 = (0,0,B0z)
       !                                    4 :    Applied field: B0 = (B0x,B0y,B0z)

       integer,parameter :: current_B_dir = 2
       !                                    1 : Applied field: B0 = (0,B0y,B0z)
       !                                    2 : Applied field: B0 = (B0x,0,B0z)
       !                                    3 : Applied field: B0 = (B0x,B0y,0)


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

       subroutine initBfield(B,B0,g,dir)
         implicit none
         type(vectorField),intent(inout) :: B,B0
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         if (restartB) then
           call initRestartB(B,g,dir)
           call initRestartB0(B0,g,dir)
           ! call initPreDefinedB0(B0,g)
         elseif (preDefinedB_ICs.ne.0) then
           call initZeroField(B)
           call initPreDefinedB0(B0,g)
         else
           call initUserBfield(B,B0,g)
         endif
       end subroutine

       subroutine initRestartB(B,g,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         type(vectorField),intent(inout) :: B
         real(cp),dimension(:),allocatable :: xc,yc,zc
         real(cp),dimension(:),allocatable :: xn,yn,zn
         allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
         allocate(xn(g%c(1)%sn),yn(g%c(2)%sn),zn(g%c(3)%sn))
         xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc
         xn = g%c(1)%hn; yn = g%c(2)%hn; zn = g%c(3)%hn
         call readFromFile(xc,yc,zc,B%x,B%y,B%z,dir//'Bfield/','Bxct','Byct','Bzct')
         deallocate(xn,yn,zn)
         deallocate(xc,yc,zc)
       end subroutine

       subroutine initRestartB0(B,g,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         type(vectorField),intent(inout) :: B
         real(cp),dimension(:),allocatable :: xc,yc,zc
         real(cp),dimension(:),allocatable :: xn,yn,zn
         allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
         allocate(xn(g%c(1)%sn),yn(g%c(2)%sn),zn(g%c(3)%sn))
         xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc
         xn = g%c(1)%hn; yn = g%c(2)%hn; zn = g%c(3)%hn
         call readFromFile(xc,yc,zc,B%x,B%y,B%z,dir//'Bfield/','B0xct','B0yct','B0zct')
         deallocate(xn,yn,zn)
         deallocate(xc,yc,zc)
       end subroutine

       subroutine initPreDefinedB0(B,g)
         implicit none
         type(grid),intent(in) :: g
         type(vectorField),intent(inout) :: B
         select case (preDefinedB_ICs)
         case (1); call uniformBfield(B,applied_B_dir)
         case (2); call initFringingField_Sergey(B,g,applied_B_dir,fringe_dir)
         case (3); call initFringingField_ALEX(B,g,applied_B_dir,fringe_dir)
         case (4); call initField_Bandaru(B,g,current_B_dir)
         case default
           write(*,*) 'Incorrect preDefinedB_ICs case in initBfield.'; stop
         end select
       end subroutine

       subroutine uniformBfield(B,dir)
         implicit none
         type(vectorField),intent(inout) :: B
         integer,intent(in) :: dir
         select case (dir)
         case (0); B%x = real(0.0,cp); B%y = real(0.0,cp); B%z = real(0.0,cp)
         case (1); B%x = real(1.0,cp); B%y = real(0.0,cp); B%z = real(0.0,cp)
         case (2); B%x = real(0.0,cp); B%y = real(1.0,cp); B%z = real(0.0,cp)
         case (3); B%x = real(0.0,cp); B%y = real(0.0,cp); B%z = real(1.0,cp)
         case (4); B%x = real(1.0,cp); B%y = real(1.0,cp); B%z = real(1.0,cp)
         case default
         stop 'Error: dir must = 1,2,3 in uniformBfield.'
         end select
       end subroutine

       subroutine initZeroField(B)
         implicit none
         type(vectorField),intent(inout) :: B
         B%x = real(0.0,cp); B%y = real(0.0,cp); B%z = real(0.0,cp)
       end subroutine

       subroutine initFringingField_Sergey(B,g,applied_dir,fringeDir)
         implicit none
         type(grid),intent(in) :: g
         type(vectorField),intent(inout) :: B
         integer,intent(in) :: applied_dir,fringeDir
         select case (applied_dir)
         case (1); call initFringe_Sergey(B%x,g,fringeDir)
         case (2); call initFringe_Sergey(B%y,g,fringeDir)
         case (3); call initFringe_Sergey(B%z,g,fringeDir)
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

       subroutine initField_Bandaru(B,g,currentDir)
         implicit none
         type(grid),intent(in) :: g
         type(vectorField),intent(inout) :: B
         integer,intent(in) :: currentDir
         integer :: i,j,k
         real(cp) :: ka ! kappa
         ka = real(1.0,cp)

         select case (currentDir)
         case (1); 
           do i=1,B%sy(1);do j=1,B%sy(2);do k=1,B%sy(3)
             B%y(i,j,k) = cos(ka*g%c(3)%hc(k)) * &
                         cosh(ka*g%c(2)%hc(j))/cosh(ka)
           enddo;enddo;enddo
           do i=1,B%sz(1);do j=1,B%sz(2);do k=1,B%sz(3)
             B%z(i,j,k) =-sin(ka*g%c(3)%hc(k)) * &
                         sinh(ka*g%c(2)%hc(j))/cosh(ka)
           enddo;enddo;enddo
         case (2); 
           do i=1,B%sx(1);do j=1,B%sx(2);do k=1,B%sx(3)
             B%x(i,j,k) =-sin(ka*g%c(1)%hc(i)) * &
                         sinh(ka*g%c(3)%hc(k))/cosh(ka)
           enddo;enddo;enddo
           do i=1,B%sz(1);do j=1,B%sz(2);do k=1,B%sz(3)
             B%z(i,j,k) = cos(ka*g%c(1)%hc(i)) * &
                         cosh(ka*g%c(3)%hc(k))/cosh(ka)
           enddo;enddo;enddo
         case (3); 
           do i=1,B%sx(1);do j=1,B%sx(2);do k=1,B%sx(3)
             B%x(i,j,k) = cos(ka*g%c(2)%hc(j)) * &
                         cosh(ka*g%c(1)%hc(i))/cosh(ka)
           enddo;enddo;enddo
           do i=1,B%sy(1);do j=1,B%sy(2);do k=1,B%sy(3)
             B%y(i,j,k) =-sin(ka*g%c(2)%hc(j)) * &
                         sinh(ka*g%c(1)%hc(i))/cosh(ka)
           enddo;enddo;enddo
         case default
         stop 'Error: applied_dir must = 1,2,3 in initField_Bandaru.'
         end select
       end subroutine

       subroutine initFringingField_ALEX(B,g,dir,fringeDir)
         implicit none
         type(grid),intent(in) :: g
         type(vectorField),intent(inout) :: B
         integer,intent(in) :: dir,fringeDir
         select case (dir)
         case (1); call initFringe_ALEX(B%x,g,fringeDir)
         case (2); call initFringe_ALEX(B%y,g,fringeDir)
         case (3); call initFringe_ALEX(B%z,g,fringeDir)
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

       subroutine initUserBfield(B,B0,g)
         implicit none
         type(grid),intent(in) :: g
         type(vectorField),intent(inout) :: B,B0
         call initZeroField(B)
         call uniformBfield(B0,3)
       end subroutine

       end module
