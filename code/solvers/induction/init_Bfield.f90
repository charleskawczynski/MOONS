       module init_Bfield_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use VF_mod
       use IO_VF_mod
       use IO_SF_mod
       implicit none

       private
       public :: initBfield

       ! NOTE: - The applied field cannot (and probably should not) be restarted
       !       - By default, preDefinedB0_ICs is used to define the applied field

       integer :: preDefinedB0_ICs = 1 ! NOTE: All cases use B_induced = 0
       !                                       0 : User-defined case (no override)
       !                                       1 : Uniform applied (set applied_B_dir)
       !                                       2 : Fringing Magnetic field (Sergey's fringe, up, const, down)
       !                                       3 : Fringing Magnetic field (ALEX experiments)
       !                                       4 : Fringing in (x,z) (Bandaru)

       integer :: fringe_dir = 1 ! Direction along which to fringe
       !                                 1 : B0(x,:,:)
       !                                 2 : B0(:,y,:)
       !                                 3 : B0(:,:,z)

       integer :: applied_B_dir = 3
       !                                    0 : No applied field: B0 = (0,0,0)
       !                                    1 :    Applied field: B0 = (B0x,0,0)
       !                                    2 :    Applied field: B0 = (0,B0y,0)
       !                                    3 :    Applied field: B0 = (0,0,B0z)
       !                                    4 :    Applied field: B0 = (B0x,B0y,B0z)

       integer :: current_B_dir = 2 ! (For Bandaru)
       !                                    1 : Applied field: B0 = (0,B0y,B0z)
       !                                    2 : Applied field: B0 = (B0x,0,B0z)
       !                                    3 : Applied field: B0 = (B0x,B0y,0)

       integer :: Bsign = 1 ! Change sign of B0 for predefined cases
       !                            1 : B0 = B0
       !                           -1 : B0 = -B0

       contains

       subroutine initBfield(B,B0,m,restartB,restartB0,dir)
         implicit none
         type(VF),intent(inout) :: B,B0
         logical,intent(in) :: restartB,restartB0
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         if (restartB) then
               call initRestartB(B,m,dir)
         else; call initZeroField(B)
         endif
         if (restartB0) then
               call initRestartB0(B0,m,dir)
         elseif (preDefinedB0_ICs.ne.0) then
               call initPreDefinedB0(B0,m)
         else; call initUserBfield(B,B0)
         endif
       end subroutine

       subroutine initRestartB(B,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         type(mesh) :: temp
         call init(temp,m)
         call import_3D_1C(temp,B%x,dir,'Bf_x',0)
         call import_3D_1C(temp,B%y,dir,'Bf_y',0)
         call import_3D_1C(temp,B%z,dir,'Bf_z',0)
         call delete(temp)
       end subroutine

       subroutine initRestartB0(B,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         type(mesh) :: temp
         call init(temp,m)
         call import_3D_3C(temp,B,dir,'B0ct',0)
         call delete(temp)
       end subroutine

       subroutine initPreDefinedB0(B,m)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         select case (preDefinedB0_ICs)
         case (1); call uniformBfield(B,applied_B_dir)
         case (2); call initFringingField_Sergey(B,m,applied_B_dir,fringe_dir)
         case (3); call initFringingField_ALEX(B,m,applied_B_dir,fringe_dir)
         case (4); call initField_Bandaru(B,m,current_B_dir)
         case default
           write(*,*) 'Erro: Incorrect preDefinedB0_ICs case in initBfield.'; stop
         end select
         select case (Bsign)
         case (1)
         case (-1)
         call multiply(B,-1.0_cp)
         case default
         stop 'Error: Bsign must = -1,1 in initPreDefinedB0 in initializeBfield.f90'
         end select
       end subroutine

       subroutine uniformBfield(B,dir)
         implicit none
         type(VF),intent(inout) :: B
         integer,intent(in) :: dir
         call assign(B,0.0_cp)
         select case (dir)
         case (0)
         case (1); call assign(B%x,1.0_cp)
         case (2); call assign(B%y,1.0_cp)
         case (3); call assign(B%z,1.0_cp)
         case (4); call assign(B,1.0_cp)
         case default
         stop 'Error: dir must = 1,2,3 in uniformBfield.'
         end select
       end subroutine

       subroutine initZeroField(B)
         implicit none
         type(VF),intent(inout) :: B
         call assign(B,0.0_cp)
       end subroutine

       subroutine initUserBfield(B,B0)
         implicit none
         type(VF),intent(inout) :: B,B0
         call initZeroField(B)
         call uniformBfield(B0,3)
       end subroutine

       subroutine initFringingField_Sergey(B,m,applied_dir,fringeDir)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         integer,intent(in) :: applied_dir,fringeDir
         select case (applied_dir)
         case (1); call initFringe_Sergey(B%x%RF(1)%f,m%g(1),fringeDir)
         case (2); call initFringe_Sergey(B%y%RF(1)%f,m%g(1),fringeDir)
         case (3); call initFringe_Sergey(B%z%RF(1)%f,m%g(1),fringeDir)
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
         Bstretch = 0.2_cp   ! Fringe slope
         Bshift = 10.0_cp    ! Fringe location

         do i=1,s(dir)
           d = dble((g%c(dir)%hc(i)-Bshift)/Bstretch)
           Btemp(i) = (1.0_cp+tanh(d))/2.0_cp
         enddo; i2 = 0
         do i=1+(s(dir)-1)/2,s(dir)
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

       subroutine initField_Bandaru(B,m,currentDir)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         integer,intent(in) :: currentDir
         call initField_Bandaru_RF(B%x%RF(1)%f,&
                                   B%y%RF(1)%f,&
                                   B%z%RF(1)%f,m%g(1),currentDir)
       end subroutine

       subroutine initField_Bandaru_RF(Bx,By,Bz,g,currentDir)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: Bx,By,Bz
         integer,intent(in) :: currentDir
         integer,dimension(3) :: sx,sy,sz
         integer :: i,j,k
         real(cp) :: ka ! kappa
         ka = 1.0_cp
         sx = shape(Bx); sy = shape(By); sz = shape(Bz)
         select case (currentDir)
         case (1); 
           do i=1,sy(1);do j=1,sy(2);do k=1,sy(3)
             By(i,j,k) = cos(ka*g%c(3)%hc(k)) * &
                         cosh(ka*g%c(2)%hc(j))/cosh(ka)
           enddo;enddo;enddo
           do i=1,sz(1);do j=1,sz(2);do k=1,sz(3)
             Bz(i,j,k) =-sin(ka*g%c(3)%hc(k)) * &
                         sinh(ka*g%c(2)%hc(j))/cosh(ka)
           enddo;enddo;enddo
         case (2); 
           do i=1,sx(1);do j=1,sx(2);do k=1,sx(3)
             Bx(i,j,k) =-sin(ka*g%c(1)%hc(i)) * &
                         sinh(ka*g%c(3)%hc(k))/cosh(ka)
           enddo;enddo;enddo
           do i=1,sz(1);do j=1,sz(2);do k=1,sz(3)
             Bz(i,j,k) = cos(ka*g%c(1)%hc(i)) * &
                         cosh(ka*g%c(3)%hc(k))/cosh(ka)
           enddo;enddo;enddo
         case (3); 
           do i=1,sx(1);do j=1,sx(2);do k=1,sx(3)
             Bx(i,j,k) = cos(ka*g%c(2)%hc(j)) * &
                         cosh(ka*g%c(1)%hc(i))/cosh(ka)
           enddo;enddo;enddo
           do i=1,sy(1);do j=1,sy(2);do k=1,sy(3)
             By(i,j,k) =-sin(ka*g%c(2)%hc(j)) * &
                         sinh(ka*g%c(1)%hc(i))/cosh(ka)
           enddo;enddo;enddo
         case default
         stop 'Error: applied_dir must = 1,2,3 in initField_Bandaru_RF in init_Bfield.'
         end select
       end subroutine

       subroutine initFringingField_ALEX(B,m,dir,fringeDir)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         integer,intent(in) :: dir,fringeDir
         select case (dir)
         case (1); call initFringe_ALEX(B%x%RF(1)%f,m%g(1),fringeDir)
         case (2); call initFringe_ALEX(B%y%RF(1)%f,m%g(1),fringeDir)
         case (3); call initFringe_ALEX(B%z%RF(1)%f,m%g(1),fringeDir)
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
         Bstretch = 0.45_cp   ! stretching parameter
         Bshift = 12.5_cp     ! shift parameter

         do i=1,s(dir)
           d = dble(g%c(dir)%hc(i)-Bshift*Bstretch)
           Btemp(i) = 0.5_cp*(1.0_cp-tanh(d))
         enddo

         select case (dir)
         case (1); do i=1,s(dir); B0(i,:,:) = Btemp(i); enddo
         case (2); do i=1,s(dir); B0(:,i,:) = Btemp(i); enddo
         case (3); do i=1,s(dir); B0(:,:,i) = Btemp(i); enddo
         case default
         stop 'Error: dir must = 1,2,3 in initFringe.'
         end select
         deallocate(Btemp)
       end subroutine

       end module
