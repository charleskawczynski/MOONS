       module init_B0_field_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use VF_mod
       use IO_import_mod
       use benchmark_case_mod
       implicit none

       private
       public :: init_B0_field

       contains

       subroutine init_B0_field(B0,m,BMC,dir)
         implicit none
         type(VF),intent(inout) :: B0
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(benchmark_case),intent(in) :: BMC
         integer :: preset_ID

         call assign(B0,0.0_cp)
         preset_ID = BMC%VS%B0%IC
         if (BMC%VS%B0%SS%restart) then
               call restart_B(B0,m,dir)
         else
           select case(preset_ID)
           case (0)
           case (1); call uniform_B_field(B0,3)
           case (2); call initFringingField_Sergey(B0,m,3,1)
           case (3); call initFringingField_ALEX(B0,m,3,1)
           case (4); call initField_Bandaru(B0,m,2)
           case default; stop 'Error: bad preset_ID in init_B0_field.f90'
           end select
         endif
       end subroutine

       subroutine restart_B(B,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         call import_3D_1C(m,B%x,dir,'Bf_x',0)
         call import_3D_1C(m,B%y,dir,'Bf_y',0)
         call import_3D_1C(m,B%z,dir,'Bf_z',0)
       end subroutine

       subroutine uniform_B_field(B,dir)
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
         stop 'Error: dir must = 1,2,3 in uniform_B_field.'
         end select
       end subroutine

       subroutine initFringingField_Sergey(B,m,applied_dir,fringeDir)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         integer,intent(in) :: applied_dir,fringeDir
         select case (applied_dir)
         case (1); call initFringe_Sergey(B%x%BF(1)%GF%f,m%B(1)%g,fringeDir)
         case (2); call initFringe_Sergey(B%y%BF(1)%GF%f,m%B(1)%g,fringeDir)
         case (3); call initFringe_Sergey(B%z%BF(1)%GF%f,m%B(1)%g,fringeDir)
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
           d = dble((g%c(dir)%hc%f(i)-Bshift)/Bstretch)
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
         call initField_Bandaru_GF(B%x%BF(1)%GF%f,&
                                   B%y%BF(1)%GF%f,&
                                   B%z%BF(1)%GF%f,m%B(1)%g,currentDir)
       end subroutine

       subroutine initField_Bandaru_GF(Bx,By,Bz,g,currentDir)
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
             By(i,j,k) = cos(ka*g%c(3)%hc%f(k)) * &
                         cosh(ka*g%c(2)%hc%f(j))/cosh(ka)
           enddo;enddo;enddo
           do i=1,sz(1);do j=1,sz(2);do k=1,sz(3)
             Bz(i,j,k) =-sin(ka*g%c(3)%hc%f(k)) * &
                         sinh(ka*g%c(2)%hc%f(j))/cosh(ka)
           enddo;enddo;enddo
         case (2);
           do i=1,sx(1);do j=1,sx(2);do k=1,sx(3)
             Bx(i,j,k) =-sin(ka*g%c(1)%hc%f(i)) * &
                         sinh(ka*g%c(3)%hc%f(k))/cosh(ka)
           enddo;enddo;enddo
           do i=1,sz(1);do j=1,sz(2);do k=1,sz(3)
             Bz(i,j,k) = cos(ka*g%c(1)%hc%f(i)) * &
                         cosh(ka*g%c(3)%hc%f(k))/cosh(ka)
           enddo;enddo;enddo
         case (3);
           do i=1,sx(1);do j=1,sx(2);do k=1,sx(3)
             Bx(i,j,k) = cos(ka*g%c(2)%hc%f(j)) * &
                         cosh(ka*g%c(1)%hc%f(i))/cosh(ka)
           enddo;enddo;enddo
           do i=1,sy(1);do j=1,sy(2);do k=1,sy(3)
             By(i,j,k) =-sin(ka*g%c(2)%hc%f(j)) * &
                         sinh(ka*g%c(1)%hc%f(i))/cosh(ka)
           enddo;enddo;enddo
         case default
         stop 'Error: applied_dir must = 1,2,3 in initField_Bandaru_GF in init_B0_field.'
         end select
       end subroutine

       subroutine initFringingField_ALEX(B,m,dir,fringeDir)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         integer,intent(in) :: dir,fringeDir
         select case (dir)
         case (1); call initFringe_ALEX(B%x%BF(1)%GF%f,m%B(1)%g,fringeDir)
         case (2); call initFringe_ALEX(B%y%BF(1)%GF%f,m%B(1)%g,fringeDir)
         case (3); call initFringe_ALEX(B%z%BF(1)%GF%f,m%B(1)%g,fringeDir)
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
           d = dble(g%c(dir)%hc%f(i)-Bshift*Bstretch)
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
