       module init_Ufield_mod
       use IO_SF_mod
       use IO_VF_mod
       use grid_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use BCs_mod
       implicit none

       private
       public :: init_Ufield,restartU

       logical,parameter :: restartU        = .false.
       integer :: preDefinedU_ICs = 1
       !                            0 : User-defined case (no override)
       !                            1 : Rest (u,v,w = 0)
       !                            2 : Parabolic Duct Flow (in x)
       !                            3 : Vortex
       !                            4 : Isolated Eddy (Weiss)
       !                            5 : Single Eddy (Weiss)
       !                            6 : Cylinder (Parker)
       !                            7 : Parabolic (1D) (Bandaru)
       ! 
       integer :: ductDirection   = 1 ! (1,2,3) = (x,y,z)
       integer :: ductSign        = 1 ! (-1,1) = {(-x,-y,-z),(x,y,z)}
       ! 
       ! integer :: vortexDirection = 1 ! (1,2,3) = (x,y,z)
       ! integer :: vortexSign      = 1 ! (-1,1) = {clockwise from +, clockwise from -}


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif
       real(cp),parameter :: PI = 3.14159265358979_cp

       contains

       subroutine init_Ufield(U,m,dir)
         implicit none
         type(VF),intent(inout) :: U
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         if (restartU) then
               call initRestartUfield(U,m,dir)
         elseif (preDefinedU_ICs.ne.0) then
               call initPreDefinedUfield(U%x%RF(1)%f,&
                                         U%y%RF(1)%f,&
                                         U%z%RF(1)%f,m%g(1))
         else; call initUserUfield(U%x%RF(1)%f,&
                                   U%y%RF(1)%f,&
                                   U%z%RF(1)%f,m%g(1))
         endif
       end subroutine

       subroutine initRestartUfield(U,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: U
         type(mesh) :: temp
         call init(temp,m)
         call import_3D_1C(temp,U%x,dir//'Ufield/','ufi',0)
         call import_3D_1C(temp,U%y,dir//'Ufield/','vfi',0)
         call import_3D_1C(temp,U%z,dir//'Ufield/','wfi',0)

         ! call export_1C_SF(temp,U%x,dir//'Ufield/','ufi_imported',0)
         ! call export_1C_SF(temp,U%y,dir//'Ufield/','vfi_imported',0)
         ! call export_1C_SF(temp,U%z,dir//'Ufield/','wfi_imported',0)
         ! stop 'Done'
         call delete(temp)
       end subroutine
       
       subroutine initPreDefinedUfield(u,v,w,g)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w

         call initRest(u,v,w)
         select case (preDefinedU_ICs)
         case (1) ! Default
         case (2); call initFullyDevelopedDuctFlow(u,v,w,g,ductDirection,ductSign)
         case (3); ! call vortex2D(u,v,w,g,3,1)     ! Vortex
         case (4); call isolatedEddy2D(u,v,w,g,3,1) ! Isolated Eddy (Weiss)
         case (5); call singleEddy2D(u,v,w,g,3,1)   ! Single Eddy (Weiss)
         case (6); call cylinder2D(u,v,w,g,3,1)     ! Cylinder
         case (7); call parabolic1D(u,v,w,g)        ! Bandaru (SS of Ha=0)
         end select
       end subroutine

       ! **************************************************************************
       ! **************************************************************************
       ! ************************ REAL FIELD FUNCTIONS ****************************
       ! **************************************************************************
       ! **************************************************************************

       subroutine initUserUfield(u,v,w,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w
         type(grid),intent(in) :: g
         integer :: j,k
         v = 0.0d0; w = 0.0d0
         do j=1,g%c(2)%sc
          do k=1,g%c(3)%sc
            u(:,j,k) = (2.0_cp - g%c(2)%hc(j)**2.0_cp - &
                                       g%c(3)%hc(k)**2.0_cp)/2.0_cp
          enddo
         enddo
       end subroutine

       subroutine initRest(u,v,w)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w
         u = 0.0_cp; v = 0.0_cp; w = 0.0_cp
       end subroutine

       subroutine initFullyDevelopedDuctFlow(u,v,w,g,dir,posNeg)
         ! This routine initializes a fully developed duct flow
         ! profile along direction dir.
         ! 
         ! There is a copy of this routine inside initUBCs.
         ! THE MASTER COPY OF THIS ROUTINE SHOULD
         ! RESIDE IN INITIALIZE UFIELD, NOT
         ! INITIALIZE UBCs
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,posNeg

         integer :: i,j,imax,jmax
         real(cp),dimension(:),allocatable :: hx,hy
         real(cp) :: alpha,height,width,F,A,A1,A2,A3
         real(cp),dimension(:,:),allocatable :: u_temp
         real(cp),dimension(3) :: hmin,hmax
         integer :: n,m,nMax,mMax
         integer,dimension(3) :: s,Ni

         hmin = (/g%c(1)%hmin,g%c(2)%hmin,g%c(3)%hmin/)
         hmax = (/g%c(1)%hmax,g%c(2)%hmax,g%c(3)%hmax/)
         Ni = (/g%c(1)%sc,g%c(2)%sc,g%c(3)%sc/)

         ! For max number of iterations in 
         ! infinite series solution:
         nMax = 100; mMax = 100
         F = 1.0_cp

         select case (dir)
         case (1) ! u(y,z)
           s = g%c(1)%sn
           width = (hmax(2) - hmin(2))/2.0_cp
           height = (hmax(3) - hmin(3))/2.0_cp
           imax = Ni(2); jmax = Ni(3)
           allocate(hx(imax)); hx = g%c(2)%hc
           allocate(hy(jmax)); hy = g%c(3)%hc
           allocate(u_temp(s(2),s(3)))
         case (2) ! v(x,z)
           s = g%c(2)%sn
           width = (hmax(1) - hmin(1))/2.0_cp
           height = (hmax(3) - hmin(3))/2.0_cp
           imax = Ni(1); jmax = Ni(3)
           allocate(hx(imax)); hx = g%c(1)%hc
           allocate(hy(jmax)); hy = g%c(3)%hc
           allocate(u_temp(s(1),s(3)))
         case (3) ! w(x,y)
           s = g%c(3)%sn
           width = (hmax(1) - hmin(1))/2.0_cp
           height = (hmax(2) - hmin(2))/2.0_cp
           imax = Ni(1); jmax = Ni(2)
           allocate(hx(imax)); hx = g%c(1)%hc
           allocate(hy(jmax)); hy = g%c(2)%hc
           allocate(u_temp(s(1),s(2)))
         case default
         stop 'Error: dir must = 1,2,3 in initFullyDevelopedDuctFlow.'
         end select
         alpha = width/height

         do i=1,imax
           do j=1,jmax
             do m=1,mMax
               do n=1,nMax
               A1 = 16.0_cp*F*alpha**2.0_cp*height**2.0_cp/&
               ((real(m,cp)*PI)**2.0_cp+(alpha*real(n,cp)*PI)**2.0_cp)
               A2 = 1.0_cp/(real(m,cp)*PI)*1.0_cp/(real(n,cp)*PI)
               A3 = (1.0_cp-cos(real(m,cp)*PI))*(1.0_cp-cos(real(n,cp)*PI))
               A = A1*A2*A3
               u_temp(i,j) = u_temp(i,j) + A*sin(real(m,cp)*PI*(hx(i)-hmin(1))/(2.0_cp*width))*&
                                             sin(real(n,cp)*PI*(hy(j)-hmin(2))/(2.0_cp*height))
               enddo
             enddo
           enddo
         enddo

         select case (dir)
         case (1); do i=1,s(1); u(i,:,:) = sign(1.0_cp,real(posNeg,cp))*u_temp; enddo
         case (2); do i=1,s(2); v(:,i,:) = sign(1.0_cp,real(posNeg,cp))*u_temp; enddo
         case (3); do i=1,s(3); w(:,:,i) = sign(1.0_cp,real(posNeg,cp))*u_temp; enddo
         end select

         deallocate(u_temp)
         deallocate(hx,hy)
       end subroutine

       subroutine parabolic1D(u,v,w,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w
         type(grid),intent(in) :: g
         integer :: k
         real(cp) :: Re
         Re = real(200.0,cp)
         v = 0.0d0; w = 0.0d0
         do k=1,g%c(3)%sc
           u(:,:,k) = real(0.5,cp)*Re*(1.0_cp - g%c(3)%hc(k)**2.0_cp)
         enddo
       end subroutine
       
       subroutine isolatedEddy2D(u,v,w,g,dir,vsign)
         ! From
         !      Weiss, N. O. The Expulsion of Magnetic Flux 
         !      by Eddies. Proc. R. Soc. A Math. Phys. Eng.
         !      Sci. 293, 310–328 (1966).
         ! 
         ! Computes
         !           U = curl(psi)
         ! Where
         !           psi = (-1/(2 pi)) cos(2 pi x) cos(2 pi y)
         !           u = dpsi/dy
         !           v =-dpsi/dx
         !           w = 0
         ! Computes
         ! dir==1
         !       u = 0
         !       v = dpsi/dz =   cos(2 pi y) sin(2 pi z)
         !       w =-dpsi/dy = - sin(2 pi y) cos(2 pi z)
         ! dir==2
         !       u =-dpsi/dz = - cos(2 pi x) sin(2 pi z)
         !       v = 0
         !       w = dpsi/dx =   sin(2 pi x) cos(2 pi z)
         ! dir==3
         !       u = dpsi/dy =   cos(2 pi x) sin(2 pi y)
         !       v =-dpsi/dx = - sin(2 pi x) cos(2 pi y)
         !       w = 0
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,vsign
         integer :: i,j,k
         integer,dimension(3) :: sx,sy,sz
         real(cp) :: two
         two = 2.0_cp

         sx = shape(u); sy = shape(v); sz = shape(w)
         select case (dir)
         case (1)
           u = 0.0_cp
           do k=1,sy(3);do j=1,sy(2);do i=1,sy(1)
                v(i,j,k) =   cos(two*PI*g%c(2)%hn(j)) * &
                             sin(two*PI*g%c(3)%hc(k))
           enddo;enddo;enddo
           do k=1,sz(3);do j=1,sz(2);do i=1,sz(1)
                w(i,j,k) = - sin(two*PI*g%c(2)%hc(j)) * &
                             cos(two*PI*g%c(3)%hn(k))
           enddo;enddo;enddo
         case (2)
           do k=1,sx(3);do j=1,sx(2);do i=1,sx(1)
                u(i,j,k) = - cos(two*PI*g%c(1)%hn(i)) * &
                             sin(two*PI*g%c(3)%hc(k))
           enddo;enddo;enddo
           v = 0.0_cp
           do k=1,sz(3);do j=1,sz(2);do i=1,sz(1)
                w(i,j,k) =   sin(two*PI*g%c(1)%hc(i)) * &
                             cos(two*PI*g%c(3)%hn(k))
           enddo;enddo;enddo
         case (3)
           do k=1,sx(3);do j=1,sx(2);do i=1,sx(1)
                u(i,j,k) =   cos(two*PI*g%c(1)%hn(i)) * &
                             sin(two*PI*g%c(2)%hc(j))
           enddo;enddo;enddo
           do k=1,sy(3);do j=1,sy(2);do i=1,sy(1)
                v(i,j,k) = - sin(two*PI*g%c(1)%hc(i)) * &
                             cos(two*PI*g%c(2)%hn(j))
           enddo;enddo;enddo
           w = 0.0_cp
         case default
         stop 'Error: dir must = 1,2,3 in isolatedEddy2D in initializeUfield.f90'
         end select
         select case (vsign)
         case (-1,1); u = u*real(vsign,cp)
                      v = v*real(vsign,cp)
                      w = w*real(vsign,cp)
         case default
         stop 'Error: vsign must = -1,1 in isolatedEddy2D in initializeUfield.f90'
         end select
       end subroutine

       subroutine singleEddy2D(u,v,w,g,dir,vsign)
         ! From
         !      Weiss, N. O. The Expulsion of Magnetic Flux 
         !      by Eddies. Proc. R. Soc. A Math. Phys. Eng.
         !      Sci. 293, 310–328 (1966).
         ! 
         ! Computes
         !           U = curl(psi)
         ! Where
         !           psi = (-1/pi) (1-4 y^2)^4 cos(pi x)
         !           u = dpsi/dy = (-1/pi) cos(pi x) 4 (1-4 y^2)^3(-8y)
         !                       = (32y/pi) cos(pi x) (1-4 y^2)^3
         !           v =-dpsi/dx = (1-4 y^2)^4 sin(pi x)
         !           w = 0
         ! Computes
         ! dir==1
         !       u = 0
         !       v = dpsi/dz =   (32z/pi) cos(pi y) (1-4 z^2)^3
         !       w =-dpsi/dy = - (1-4 z^2)^4 sin(pi y)
         ! dir==2
         !       u =-dpsi/dz = - (1-4 x^2)^4 sin(pi z)
         !       v = 0
         !       w = dpsi/dx =   (32x/pi) cos(pi z) (1-4 x^2)^3
         ! dir==3
         !       u = dpsi/dy =   (32y/pi) cos(pi x) (1-4 y^2)^3
         !       v =-dpsi/dx = - (1-4 y^2)^4 sin(pi x)
         !       w = 0
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,vsign
         integer :: i,j,k
         integer,dimension(3) :: sx,sy,sz
         real(cp) :: one,two,three,four
         one = 1.0_cp; two = 2.0_cp
         three = 3.0_cp; four = 4.0_cp

         sx = shape(u); sy = shape(v); sz = shape(w)
         select case (dir)
         case (1)
           u = 0.0_cp
           do k=1,sy(3);do j=1,sy(2);do i=1,sy(1)
                v(i,j,k) =   (real(32,cp)*g%c(3)%hc(k)/PI)*((one-four*g%c(3)%hc(k)**two)**three) * &
                cos(PI*g%c(2)%hn(j))
           enddo;enddo;enddo
           do k=1,sz(3);do j=1,sz(2);do i=1,sz(1)
                w(i,j,k) = - ((one-four*g%c(3)%hn(k)**two)**four)*sin(PI*g%c(2)%hc(j))
           enddo;enddo;enddo
         case (2)
           do k=1,sx(3);do j=1,sx(2);do i=1,sx(1)
                u(i,j,k) = - ((one-four*g%c(1)%hn(i)**two)**four)*sin(PI*g%c(3)%hc(k))
           enddo;enddo;enddo
           v = 0.0_cp
           do k=1,sz(3);do j=1,sz(2);do i=1,sz(1)
                w(i,j,k) =   (real(32,cp)*g%c(1)%hc(i)/PI)*((one-four*g%c(1)%hc(i)**two)**three) * &
                cos(PI*g%c(3)%hn(k))
           enddo;enddo;enddo
         case (3)
           do k=1,sx(3);do j=1,sx(2);do i=1,sx(1)
                u(i,j,k) =   (real(32,cp)*g%c(2)%hc(j)/PI)*((one-four*g%c(2)%hc(j)**two)**three) * &
                cos(PI*g%c(1)%hn(i))
           enddo;enddo;enddo
           do k=1,sy(3);do j=1,sy(2);do i=1,sy(1)
                v(i,j,k) = - ((one-four*g%c(2)%hn(j)**two)**four)*sin(PI*g%c(1)%hc(i))
           enddo;enddo;enddo
           w = 0.0_cp
         case default
         stop 'Error: dir must = 1,2,3 in singleEddy2D in initializeUfield.f90'
         end select
         select case (vsign)
         case (-1,1); u = u*real(vsign,cp)
                      v = v*real(vsign,cp)
                      w = w*real(vsign,cp)
         case default
         stop 'Error: vsign must = -1,1 in singleEddy2D in initializeUfield.f90'
         end select
       end subroutine

       subroutine cylinder2D(u,v,w,g,dir,vsign)
         ! From
         !      Moffatt
         ! 
         ! Computes
         !           U(r) = omega0*r
         ! for
         !           0 < r < r0
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,vsign
         integer :: i,j,k
         integer,dimension(3) :: sx,sy,sz
         real(cp),dimension(3) :: hc
         real(cp) :: omega0,r0,two,r,theta
         two = 2.0_cp

         omega0 = 1.0_cp
         r0 = 1.0_cp
         u = 0.0_cp; v = 0.0_cp; w = 0.0_cp

         sx = shape(u); sy = shape(v); sz = shape(w)
         select case (dir)
         case (1)
           hc = (/((g%c(i)%hmax+g%c(i)%hmin)/2.0_cp,i=1,3)/)
           u = 0.0_cp
           do k=1,sy(3);do j=1,sy(2);do i=1,sy(1)
                r = sqrt((g%c(2)%hn(j)-hc(2))**two + (g%c(3)%hc(k)-hc(3))**two)
                if (r.lt.r0) v(i,j,k) =-omega0*r
           enddo;enddo;enddo
           do k=1,sz(3);do j=1,sz(2);do i=1,sz(1)
                r = sqrt((g%c(2)%hc(j)-hc(2))**two + (g%c(3)%hn(k)-hc(3))**two)
                if (r.lt.r0) w(i,j,k) = omega0*r
           enddo;enddo;enddo
         case (2)
           hc = (/((g%c(i)%hmax+g%c(i)%hmin)/2.0_cp,i=1,3)/)
           do k=1,sx(3);do j=1,sx(2);do i=1,sx(1)
                r = sqrt((g%c(1)%hn(i)-hc(1))**two + (g%c(3)%hn(k)-hc(3))**two)
                if (r.lt.r0) u(i,j,k) = omega0*r
           enddo;enddo;enddo
           v = 0.0_cp
           do k=1,sz(3);do j=1,sz(2);do i=1,sz(1)
                r = sqrt((g%c(1)%hn(i)-hc(1))**two + (g%c(3)%hn(k)-hc(3))**two)
                if (r.lt.r0) w(i,j,k) =-omega0*r
           enddo;enddo;enddo
         case (3)
           hc = (/((g%c(i)%hmax+g%c(i)%hmin)/2.0_cp,i=1,3)/)
           do k=1,sx(3);do j=1,sx(2);do i=1,sx(1)
                r = sqrt((g%c(1)%hn(i)-hc(1))**two + (g%c(2)%hc(j)-hc(2))**two)
                theta = atan2(g%c(2)%hc(j),g%c(1)%hn(i))
                if (r.lt.r0) u(i,j,k) =-omega0*r*sin(theta)
           enddo;enddo;enddo
           do k=1,sy(3);do j=1,sy(2);do i=1,sy(1)
                r = sqrt((g%c(1)%hc(i)-hc(1))**two + (g%c(2)%hn(j)-hc(2))**two)
                theta = atan2(g%c(2)%hn(j),g%c(1)%hc(i))
                if (r.lt.r0) v(i,j,k) = omega0*r*cos(theta)
           enddo;enddo;enddo
           w = 0.0_cp
         case default
         stop 'Error: dir must = 1,2,3 in singleEddy2D in initializeUfield.f90'
         end select
         select case (vsign)
         case (-1,1); u = u*real(vsign,cp)
                      v = v*real(vsign,cp)
                      w = w*real(vsign,cp)
         case default
         stop 'Error: vsign must = -1,1 in singleEddy2D in initializeUfield.f90'
         end select
       end subroutine

       end module
