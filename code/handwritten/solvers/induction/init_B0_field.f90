       module init_B0_field_mod
       use current_precision_mod
       use grid_mod
       use GF_mod
       use mesh_extend_mod
       use VF_mod
       use IO_import_mod
       use sim_params_mod
       implicit none

       private
       public :: init_B0_field

       contains

       subroutine init_B0_field(B0,m,SP)
         implicit none
         type(VF),intent(inout) :: B0
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         integer :: preset_ID

         call assign(B0,0.0_cp)
         preset_ID = SP%VS%B0%IC
         ! preset_ID = 0 ! manual override

         select case(preset_ID)
         case (0)
         case (1); call uniform_B_field(B0,SP%SCP%uniform_B0_dir)
         case (2); call initFringingField_Sergey(B0,m,3,1)
         case (3); call initFringingField_ALEX(B0,m,3,1)
         case (4); call init_Field_Bandaru(B0,m,2)
         case default; stop 'Error: bad preset_ID in init_B0_field.f90'
         end select
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
         case (1); call init_Fringe_Sergey(B%x%BF(1)%GF,m%B(1)%g,fringeDir)
         case (2); call init_Fringe_Sergey(B%y%BF(1)%GF,m%B(1)%g,fringeDir)
         case (3); call init_Fringe_Sergey(B%z%BF(1)%GF,m%B(1)%g,fringeDir)
         case default
         stop 'Error: applied_dir must = 1,2,3 in initFringingField_Sergey.'
         end select
       end subroutine

       subroutine init_Fringe_Sergey(B,g,dir)
         implicit none
         type(grid),intent(in) :: g
         type(grid_field),intent(inout) :: B
         integer,intent(in) :: dir
         type(grid_field) :: Btemp
         integer :: i2,i
         real(cp) :: d,Bstretch,Bshift,temp
         call init(Btemp,B)
         Bstretch = 0.2_cp; Bshift = 10.0_cp
         do i=1,B%s(dir)
           d = dble((g%c(dir)%hc%f(i)-Bshift)/Bstretch)
           temp = (1.0_cp+tanh(d))/2.0_cp
           call assign_plane(Btemp,temp,i,dir)
         enddo; i2 = 0
         call assign(B,Btemp)
         do i=1+(B%s(dir)-1)/2,B%s(dir)
          call assign_plane(B,Btemp,i,1+(B%s(dir)+1)/2-i2,dir); i2 = i2+1
         enddo
         call delete(Btemp)
       end subroutine

       subroutine init_Field_Bandaru(B,m,currentDir)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         integer,intent(in) :: currentDir
         call init_Field_Bandaru_GF(B%x%BF(1)%GF,&
                                    B%y%BF(1)%GF,&
                                    B%z%BF(1)%GF,m%B(1)%g,currentDir)
       end subroutine

       subroutine init_Field_Bandaru_GF(Bx,By,Bz,g,currentDir)
         implicit none
         type(grid_field),intent(inout) :: Bx,By,Bz
         type(grid),intent(in) :: g
         integer,intent(in) :: currentDir
         integer :: i,j,k
         real(cp) :: ka,cosh_ka ! kappa
         ka = 1.0_cp
         cosh_ka = cosh(ka)
         select case (currentDir)
         case (1);
           do i=1,By%s(1);do j=1,By%s(2);do k=1,By%s(3)
             By%f(i,j,k) = cos( ka*g%c(3)%hc%f(k)) * &
                           cosh(ka*g%c(2)%hn%f(j))/cosh_ka
           enddo;enddo;enddo
           do i=1,Bz%s(1);do j=1,Bz%s(2);do k=1,Bz%s(3)
             Bz%f(i,j,k) =-sin( ka*g%c(3)%hn%f(k)) * &
                           sinh(ka*g%c(2)%hc%f(j))/cosh_ka
           enddo;enddo;enddo
         case (2);
           do i=1,Bx%s(1);do j=1,Bx%s(2);do k=1,Bx%s(3)
             Bx%f(i,j,k) =-sin( ka*g%c(1)%hn%f(i)) * &
                           sinh(ka*g%c(3)%hc%f(k))/cosh_ka
           enddo;enddo;enddo
           do i=1,Bz%s(1);do j=1,Bz%s(2);do k=1,Bz%s(3)
             Bz%f(i,j,k) = cos( ka*g%c(1)%hc%f(i)) * &
                           cosh(ka*g%c(3)%hn%f(k))/cosh_ka
           enddo;enddo;enddo
         case (3);
           do i=1,Bx%s(1);do j=1,Bx%s(2);do k=1,Bx%s(3)
             Bx%f(i,j,k) = cos( ka*g%c(2)%hc%f(j)) * &
                           cosh(ka*g%c(1)%hn%f(i))/cosh_ka
           enddo;enddo;enddo
           do i=1,By%s(1);do j=1,By%s(2);do k=1,By%s(3)
             By%f(i,j,k) =-sin( ka*g%c(2)%hn%f(j)) * &
                           sinh(ka*g%c(1)%hc%f(i))/cosh_ka
           enddo;enddo;enddo
         case default
         stop 'Error: currentDir must = 1,2,3 in init_Field_Bandaru_GF in init_B0_field.'
         end select
       end subroutine

       subroutine initFringingField_ALEX(B,m,dir,fringeDir)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         integer,intent(in) :: dir,fringeDir
         select case (dir)
         case (1); call init_ALEX_Fringe(B%x%BF(1)%GF,m%B(1)%g,fringeDir)
         case (2); call init_ALEX_Fringe(B%y%BF(1)%GF,m%B(1)%g,fringeDir)
         case (3); call init_ALEX_Fringe(B%z%BF(1)%GF,m%B(1)%g,fringeDir)
         case default
         stop 'Error: dir must = 1,2,3 in initFringingField_ALEX.'
         end select
       end subroutine

       subroutine init_ALEX_Fringe(B0,g,dir)
         implicit none
         type(grid),intent(in) :: g
         type(grid_field),intent(inout) :: B0
         integer,intent(in) :: dir
         real(cp) :: d,temp,Bstretch,Bshift
         integer :: i
         Bstretch = 0.45_cp; Bshift = 12.5_cp
         do i=1,B0%s(dir)
           d = dble(g%c(dir)%hc%f(i)-Bshift*Bstretch)
           temp = 0.5_cp*(1.0_cp-tanh(d))
           call assign_plane(B0,temp,i,dir)
         enddo
       end subroutine

       end module
