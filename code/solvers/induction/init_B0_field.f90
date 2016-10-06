       module init_B0_field_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use VF_mod
       use IO_VF_mod
       use IO_SF_mod
       implicit none

       private
       public :: init_B0_field

       integer :: preDefinedB0_ICs = 1
       !                             0 : No field, B = 0
       !                             1 : Uniform applied (set applied_B_dir)
       !                             2 : Sergey's fringing B (up, const, down, set fringe_dir)
       !                             3 : ALEX experiment fringing B (set fringe_dir)
       !                             4 : Bandaru fringing B (in x,z, set current_B_dir)

       integer :: fringe_dir = 1 ! Direction along which to fringe
       !                                 1 : B0(x,:,:)
       !                                 2 : B0(:,y,:)
       !                                 3 : B0(:,:,z)

       integer :: applied_B_dir = 3
       !                                    0 : No applied field: B0 = (0,0,0)
       !                                    1 :    Applied field: B0 = (B0x,0,0)
       !                                    2 :    Applied field: B0 = (0,B0y,0)
       !                                    3 :    Applied field: B0 = (0,0,B0z)

       integer :: current_B_dir = 2 ! Plane normal to fringe
       !                                    1 : Applied field: B0 = (0,B0y,B0z)
       !                                    2 : Applied field: B0 = (B0x,0,B0z)
       !                                    3 : Applied field: B0 = (B0x,B0y,0)

       integer :: Bsign = 1 ! Change sign of B0 for predefined cases
       !                            1 : B0 = B0
       !                           -1 : B0 = -B0

       contains

       subroutine init_B0_field(B,m,restart,dir)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         logical,intent(in) :: restart
         character(len=*),intent(in) :: dir

         call zero_field(B)

         select case (preDefinedB0_ICs)
         case (0); ! zero_field
         case (1); call uniform_B_field(B,applied_B_dir)
         case (2); call initFringingField_Sergey(B,m,applied_B_dir,fringe_dir)
         case (3); call initFringingField_ALEX(B,m,applied_B_dir,fringe_dir)
         case (4); call initField_Bandaru(B,m,current_B_dir)
         case default; write(*,*) 'Erro: Incorrect preDefinedB0_ICs case in initBfield.'; stop 'Done'
         end select

         if (Bsign.eq.-1) call multiply(B,-1.0_cp)

         call make_periodic(B,periodic_dir)

         if (restart) call init_Restart_B(B,m,dir)
       end subroutine

       subroutine init_Restart_B(B,m,dir)
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

       subroutine zero_field(B)
         implicit none
         type(VF),intent(inout) :: B
         call assign(B,0.0_cp)
       end subroutine

       subroutine uniform_B_field(B,dir)
         implicit none
         type(VF),intent(inout) :: B
         integer,intent(in) :: dir
         select case (dir)
         case (1); call assign(B%x,1.0_cp)
         case (2); call assign(B%y,1.0_cp)
         case (3); call assign(B%z,1.0_cp)
         case default; stop 'Error: dir must = 0:4 in uniform_B_field in init_B0_field.f90.'
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

       subroutine initFringe_ALEX(B,m,fringe_dir,applied_dir)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         integer,intent(in) :: fringe_dir,applied_dir
         if (fringe_dir.eq.applied_dir) then
           write(*,*) 'Error: fringe_dir=applied_dir -> violates div(B)=0 in initFringe_ALEX in init_B0_field.f90'
           stop 'Done'
         endif
         select case (applied_dir)
         case (1); call initFringe_ALEX_SF(B%x,m,fringe_dir)
         case (2); call initFringe_ALEX_SF(B%y,m,fringe_dir)
         case (3); call initFringe_ALEX_SF(B%z,m,fringe_dir)
         case default; stop 'Error: dir must = 1,2,3 in initFringe_ALEX in init_B0_field.f90.'
         end select
       end subroutine

       subroutine initFringe_ALEX_SF(B,m,fringe_dir)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: B
         integer,intent(in) :: fringe_dir
         type(SF) :: temp
         type(coordinates) :: temp
         real(cp) :: d,Bstretch,Bshift
         integer :: i
         call init(temp,B)
         ! Sergey's fringe:
         Bstretch = 0.45_cp   ! stretching parameter
         Bshift = 12.5_cp     ! shift parameter
         do i=1,s(dir)
           d = m%B(t)%g%c(dir)%hc(i)-Bshift*Bstretch
           Btemp(i) = 0.5_cp*(1.0_cp-tanh(d))
         enddo

         call init_CC(temp,B%BF(t)%GF%s(fringe_dir))
         do i=1,B%BF(t)%GF%s(fringe_dir)
           temp%hc(i) = 0.5_cp*(1.0_cp-tanh(m%B(t)%g%c(dir)%hc(i)-Bshift*Bstretch))
         enddo

         select case (fringe_dir)
         case (1); do t=1,m%s;do i=1,B%BF(t)%GF%s(fringe_dir); B%BF(t)%GF%f(i,:,:)=temp%hc(i);enddo;enddo
         case (2); do t=1,m%s;do i=1,B%BF(t)%GF%s(fringe_dir); B%BF(t)%GF%f(:,i,:)=temp%hc(i);enddo;enddo
         case (3); do t=1,m%s;do i=1,B%BF(t)%GF%s(fringe_dir); B%BF(t)%GF%f(:,:,i)=temp%hc(i);enddo;enddo
         case default; stop 'Error: dir must = 1,2,3 in initFringe.'
         end select

         select case (fringe_dir)
         case (1); do i=1,B%BF(t)%GF%s(dir); B%BF(t)%GF%f(i,:,:) = Btemp(i); enddo
         case (2); do i=1,B%BF(t)%GF%s(dir); B%BF(t)%GF%f(:,i,:) = Btemp(i); enddo
         case (3); do i=1,B%BF(t)%GF%s(dir); B%BF(t)%GF%f(:,:,i) = Btemp(i); enddo
         case default; stop 'Error: dir must = 1,2,3 in initFringe.'
         end select
         call delete(temp)
       end subroutine

       subroutine initFringe_ALEX_SF(B,m,fringe_dir)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: B
         integer,intent(in) :: fringe_dir
         type(coordinates) :: temp
         real(cp) :: d,Bstretch,Bshift
         integer :: i
         ! Sergey's fringe:
         Bstretch = 0.45_cp   ! stretching parameter
         Bshift = 12.5_cp     ! shift parameter
         do t=1,m%s
           call init(temp,m%B(t)%g%c(fringe_dir)%hn,m%B(t)%g%c(fringe_dir)%sn)
           do i=1,B%BF(t)%GF%s(fringe_dir)
             temp%hc(i) = 0.5_cp*(1.0_cp-tanh(m%B(t)%g%c(dir)%hc(i)-Bshift*Bstretch))
           enddo
           select case (fringe_dir)
           case (1); do i=1,B%BF(t)%GF%s(fringe_dir); B%BF(t)%GF%f(i,:,:)=temp%hc(i);enddo
           case (2); do i=1,B%BF(t)%GF%s(fringe_dir); B%BF(t)%GF%f(:,i,:)=temp%hc(i);enddo
           case (3); do i=1,B%BF(t)%GF%s(fringe_dir); B%BF(t)%GF%f(:,:,i)=temp%hc(i);enddo
           case default; stop 'Error: dir must = 1:3 in initFringe_ALEX_SF in init_B0_field.f90.'
           end select
           call delete(temp)
         enddo
       end subroutine

       subroutine initField_Bandaru(B,m,currentDir)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         integer,intent(in) :: currentDir
         integer :: t,i,j,k
         real(cp) :: ka ! kappa
         ka = 1.0_cp
         select case (currentDir)
         case (1)
           do t=1,m%s; do i=1,B%y%BF(t)%GF%s(1);do j=1,B%y%BF(t)%GF%s(2);do k=1,B%y%BF(t)%GF%s(3)
             B%y%BF(1)%GF%f(i,j,k) = cos(ka*m%B(t)%g%c(3)%hc(k))*cosh(ka*m%B(t)%g%c(2)%hc(j))/cosh(ka)
           enddo;enddo;enddo; enddo
           do t=1,m%s; do i=1,B%z%BF(t)%GF%s(1);do j=1,B%z%BF(t)%GF%s(2);do k=1,B%z%BF(t)%GF%s(3)
             B%z%BF(1)%GF%f(i,j,k) =-sin(ka*m%B(t)%g%c(3)%hc(k))*sinh(ka*m%B(t)%g%c(2)%hc(j))/cosh(ka)
           enddo;enddo;enddo; enddo
         case (2)
           do t=1,m%s; do i=1,B%x%BF(t)%GF%s(1);do j=1,B%x%BF(t)%GF%s(2);do k=1,B%x%BF(t)%GF%s(3)
             B%x%BF(1)%GF%f(i,j,k) =-sin(ka*m%B(t)%g%c(1)%hc(i))*sinh(ka*m%B(t)%g%c(3)%hc(k))/cosh(ka)
           enddo;enddo;enddo; enddo
           do t=1,m%s; do i=1,B%z%BF(t)%GF%s(1);do j=1,B%z%BF(t)%GF%s(2);do k=1,B%z%BF(t)%GF%s(3)
             B%z%BF(1)%GF%f(i,j,k) = cos(ka*m%B(t)%g%c(1)%hc(i))*cosh(ka*m%B(t)%g%c(3)%hc(k))/cosh(ka)
           enddo;enddo;enddo; enddo
         case (3)
           do t=1,m%s; do i=1,B%x%BF(t)%GF%s(1);do j=1,B%x%BF(t)%GF%s(2);do k=1,B%x%BF(t)%GF%s(3)
             B%x%BF(1)%GF%f(i,j,k) = cos(ka*m%B(t)%g%c(2)%hc(j))*cosh(ka*m%B(t)%g%c(1)%hc(i))/cosh(ka)
           enddo;enddo;enddo; enddo
           do t=1,m%s; do i=1,B%y%BF(t)%GF%s(1);do j=1,B%y%BF(t)%GF%s(2);do k=1,B%y%BF(t)%GF%s(3)
             B%y%BF(1)%GF%f(i,j,k) =-sin(ka*m%B(t)%g%c(2)%hc(j))*sinh(ka*m%B(t)%g%c(1)%hc(i))/cosh(ka)
           enddo;enddo;enddo; enddo
         case default; stop 'Error: applied_dir must = 1:3 in initField_Bandaru in init_Bfield.f90.'
         end select
       end subroutine

       end module