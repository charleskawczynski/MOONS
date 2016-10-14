       module profile_funcs_mod
       use current_precision_mod
       use grid_mod
       use coordinates_mod
       implicit none

       private
       ! None of these have been tested yet..
       public :: rotatingCylinder   ! Done
       public :: init_FD_DuctFlow   ! Done
       public :: parabolic1D        ! Done
       public :: shercliff_profile  ! Done
       public :: hunt_profile       ! Done
       public :: isolatedEddy2D     ! Need to change to func
       public :: singleEddy2D       ! Need to change to func
       public :: cylinder2D         ! Need to change to func
       public :: SH_profile

       real(cp),parameter :: PI = 3.141592653589793238462643383279502884197169399375105820974_cp
       
       contains

       function rotatingCylinder(cx,cy,sx,sy,r0,omega0,component) result(f)
         ! Computes velocities fx,fy due to rigid body motion
         ! with coordinates cx,cy. The radius of rotation, r0,
         ! and angular velocity, omega0, are inputs.
         ! NOTE: omega0 may be zero to allow for reverse rotation.
         ! component = (1,2) = (x,y)
         implicit none
         integer,intent(in) :: sx,sy
         real(cp),dimension(sx,sy) :: f
         type(coordinates),intent(in) :: cx,cy
         real(cp),intent(in) :: r0,omega0
         integer,intent(in) :: component
         real(cp) :: x0,y0,r,theta
         integer :: i,j
         real(cp),dimension(:),allocatable :: x,y
         real(cp),dimension(2) :: hmin,hmax
         allocate(x(sx)); allocate(y(sy))
             if (sx.eq.cx%sn) then; x = cx%hn
         elseif (sx.eq.cx%sc) then; x = cx%hc
           else; stop 'Error: bad size in rotatingCylinder in profile_funcs.f90'
         endif
             if (sy.eq.cy%sn) then; y = cy%hn
         elseif (sy.eq.cy%sc) then; y = cy%hc
           else; stop 'Error: bad size in rotatingCylinder in profile_funcs.f90'
         endif

         hmin = (/cx%hmin,cy%hmin/); x0 = (hmax(1) + hmin(1))/2.0_cp
         hmax = (/cx%hmax,cy%hmax/); y0 = (hmax(2) + hmin(2))/2.0_cp
         do i=1,sx; do j=1,sy
           r = sqrt((x(i)-x0)**2.0_cp + (y(j)-y0)**2.0_cp)
           if (r.lt.r0) then
             theta = atan2(y(j),x(i))
             select case (component)
             case (1); f(i,j) = -omega0*r*sin(theta)
             case (2); f(i,j) =  omega0*r*cos(theta)
             end select
           endif
         enddo; enddo
         deallocate(x); deallocate(y)
       end function

       function shercliff_profile(cx,cy,sx,sy,Ha,mu,dpdz) result(w)
         ! Computes the Shercliff profile, w(x,y) for Hartmann number,Ha
         ! Reference:
         !      "Planas, R., Badia, S. & Codina, R. Approximation of 
         !      the inductionless MHD problem using a stabilized finite 
         !      element method. J. Comput. Phys. 230, 2977–2996 (2011)."
         implicit none
         integer,intent(in) :: sx,sy
         real(cp),dimension(sx,sy) :: w
         type(coordinates),intent(in) :: cx,cy
         real(cp),intent(in) :: Ha,mu,dpdz
         real(cp) :: r1k,r2k,V2,V3,coeff,N,alpha_k,d_B,L,term,a,b
         integer :: i,j,k,iter
         real(cp),dimension(:),allocatable :: x,y

         allocate(x(sx)); allocate(y(sy))
             if (sx.eq.cx%sn) then; x = cx%hn
         elseif (sx.eq.cx%sc) then; x = cx%hc
           else; stop 'Error: bad size in shercliff_profile in profile_funcs.f90'
         endif
             if (sy.eq.cy%sn) then; y = cy%hn
         elseif (sy.eq.cy%sc) then; y = cy%hc
           else; stop 'Error: bad size in shercliff_profile in profile_funcs.f90'
         endif

         a = (cx%hmax - cx%hmin)/2.0_cp
         b = (cy%hmax - cy%hmin)/2.0_cp
         L = b/a

         d_B = 0.0_cp
         iter = 1000
         do k=0,iter
           do i=1,sx; do j=1,sy
             alpha_k = (k+0.5_cp)*PI/L
             N = (Ha**2.0_cp + 4.0_cp*alpha_k**2.0_cp)**(0.5_cp)
             r1k = 0.5_cp*(Ha + N)
             r2k = 0.5_cp*(-Ha + N)
             V2 = shercliff_coeff(r1k,r2k,y(j)/a,d_B,N)
             V3 = shercliff_coeff(r2k,r1k,y(j)/a,d_B,N)
             
             coeff = 2.0_cp*(-1.0_cp)**k*cos(alpha_k*x(i)/a)/(L*alpha_k**3.0_cp)
             term = coeff*(1.0_cp - V2 - V3)
             w(i,j) = w(i,j) + term
           enddo; enddo
         enddo
         w = w/mu*(-dpdz)*a**2.0_cp
         deallocate(x); deallocate(y)
       end function

       function shercliff_coeff(r1k,r2k,eta,d_B,N) result(V)
         ! Computes the coefficient (V2,V3) in the Shercliff profile
         ! expressions given in reference:
         ! 
         !      "Planas, R., Badia, S. & Codina, R. Approximation of 
         !      the inductionless MHD problem using a stabilized finite 
         !      element method. J. Comput. Phys. 230, 2977–2996 (2011)."
         implicit none
         real(cp),intent(in) :: r1k,r2k,eta,d_B,N
         real(cp) :: A,B,C,D,V
         A = d_B*r2k+(1.0_cp-exp(-2.0_cp*r2k))/(1.0_cp+exp(-2.0_cp*r2k))
         B = (exp(-r1k*(1.0_cp-eta))+exp(-r1k*(1.0_cp+eta)))/2.0_cp
         C = (1.0_cp+exp(-2.0_cp*r1k))/2.0_cp*d_B*N
         D = (1.0_cp-exp(-2.0_cp*(r1k+r2k)))/(1.0_cp+exp(-2.0_cp*r2k))
         V = A*B/(C+D)
       end function

       function hunt_coeff(r1k,r2k,eta,N) result(V)
         ! Computes the coefficient (V2,V3) in the Hunt profile
         ! expressions given in reference:
         ! 
         !      "Planas, R., Badia, S. & Codina, R. Approximation of 
         !      the inductionless MHD problem using a stabilized finite 
         !      element method. J. Comput. Phys. 230, 2977–2996 (2011)."
         implicit none
         real(cp),intent(in) :: r1k,r2k,eta,N
         real(cp) :: A,B,C,V
         A = r2k/N
         B = exp(-r1k*(1.0_cp-eta))+exp(-r1k*(1.0_cp+eta))
         C = 1.0_cp+exp(-2.0_cp*r1k)
         V = A*B/C
       end function

       function hunt_profile(cx,cy,sx,sy,Ha,mu,dpdz) result(w)
         ! Computes the Hunt profile, w(x,y) for Hartmann number,Ha
         ! Reference:
         !      "Planas, R., Badia, S. & Codina, R. Approximation of 
         !      the inductionless MHD problem using a stabilized finite 
         !      element method. J. Comput. Phys. 230, 2977–2996 (2011)."
         implicit none
         integer,intent(in) :: sx,sy
         real(cp),dimension(sx,sy) :: w
         type(coordinates),intent(in) :: cx,cy
         real(cp),intent(in) :: Ha,mu,dpdz
         real(cp) :: r1k,r2k,V2,V3,coeff,N,alpha_k,L,term,a,b
         integer :: i,j,k,iter
         real(cp),dimension(:),allocatable :: x,y

         allocate(x(sx)); allocate(y(sy))
             if (sx.eq.cx%sn) then; x = cx%hn
         elseif (sx.eq.cx%sc) then; x = cx%hc
           else; stop 'Error: bad size in hunt_profile in profile_funcs.f90'
         endif
             if (sy.eq.cy%sn) then; y = cy%hn
         elseif (sy.eq.cy%sc) then; y = cy%hc
           else; stop 'Error: bad size in hunt_profile in profile_funcs.f90'
         endif

         a = (cx%hmax - cx%hmin)/2.0_cp
         b = (cy%hmax - cy%hmin)/2.0_cp
         L = b/a

         iter = 1000
         do k=0,iter
           do i=1,sx; do j=1,sy
             alpha_k = (k+0.5_cp)*PI/L
             N = (Ha**2.0_cp + 4.0_cp*alpha_k**2.0_cp)**(0.5_cp)
             r1k = 0.5_cp*(Ha + N)
             r2k = 0.5_cp*(-Ha + N)
             V2 = hunt_coeff(r1k,r2k,y(j)/a,N)
             V3 = hunt_coeff(r2k,r1k,y(j)/a,N)
             
             coeff = 2.0_cp*(-1.0_cp)**k*cos(alpha_k*x(i)/a)/(L*alpha_k**3.0_cp)
             term = coeff*(1.0_cp - V2 - V3)
             w(i,j) = w(i,j) + term
           enddo; enddo
         enddo
         w = w/mu*(-dpdz)*a**2.0_cp
         deallocate(x); deallocate(y)
       end function

       function SH_coeff(r1k,r2k,eta,d_B,N) result(V)
         ! Computes the coefficient (V2,V3) in the Shercliff profile
         ! expressions given in reference:
         ! 
         !      "Planas, R., Badia, S. & Codina, R. Approximation of 
         !      the inductionless MHD problem using a stabilized finite 
         !      element method. J. Comput. Phys. 230, 2977–2996 (2011)."
         implicit none
         real(cp),intent(in) :: r1k,r2k,eta,d_B,N
         real(cp) :: A,B,C,D,V
         A = d_B*r2k+(1.0_cp-exp(-2.0_cp*r2k))/(1.0_cp+exp(-2.0_cp*r2k))
         B = (exp(-r1k*(1.0_cp-eta))+exp(-r1k*(1.0_cp+eta)))/2.0_cp
         C = (1.0_cp+exp(-2.0_cp*r1k))/2.0_cp*d_B*N
         D = (1.0_cp-exp(-2.0_cp*(r1k+r2k)))/(1.0_cp+exp(-2.0_cp*r2k))
         V = A*B/(C+D)
       end function

       function SH_profile(cx,cy,sx,sy,d_B,Ha,mu,dpdz) result(w)
         ! Computes the Hunt profile, w(x,y) for Hartmann number,Ha
         ! Reference:
         !      "Planas, R., Badia, S. & Codina, R. Approximation of 
         !      the inductionless MHD problem using a stabilized finite 
         !      element method. J. Comput. Phys. 230, 2977–2996 (2011)."
         ! 
         ! 
         ! Hunt flow configuration
         ! 
         !         conducting
         !   B⁰   |----------|
         !   ^    |          |
         !   |    |          | - non-conducting
         !   |    |          |
         !        |----------|
         !         conducting
         ! 
         implicit none
         integer,intent(in) :: sx,sy
         real(cp),dimension(sx,sy) :: w
         type(coordinates),intent(in) :: cx,cy
         real(cp),intent(in) :: Ha,mu,dpdz,d_B
         real(cp) :: r1k,r2k,V2,V3,coeff,N,alpha_k,L,term,a,b
         integer :: i,j,k,iter
         real(cp),dimension(:),allocatable :: x,y,xi,eta

         allocate(x(sx)); allocate(y(sy)); allocate(xi(sx)); allocate(eta(sy))
             if (sx.eq.cx%sn) then; x = cx%hn
         elseif (sx.eq.cx%sc) then; x = cx%hc
           else; stop 'Error: bad size in hunt_profile in profile_funcs.f90'
         endif
             if (sy.eq.cy%sn) then; y = cy%hn
         elseif (sy.eq.cy%sc) then; y = cy%hc
           else; stop 'Error: bad size in hunt_profile in profile_funcs.f90'
         endif

         a = (cx%hmax - cx%hmin)/2.0_cp
         b = (cy%hmax - cy%hmin)/2.0_cp
         L = b/a
         xi = x/a
         eta = y/a

         iter = 1000
         w = 0.0_cp
         do k=0,iter
           do i=1,sx; do j=1,sy
             alpha_k = (k+0.5_cp)*PI/L
             N = (Ha**2.0_cp + 4.0_cp*alpha_k**2.0_cp)**(0.5_cp)
             r1k = 0.5_cp*(Ha + N)
             r2k = 0.5_cp*(-Ha + N)
             V2 = SH_coeff(r1k,r2k,eta(j),d_B,N)
             V3 = SH_coeff(r2k,r1k,eta(j),d_B,N)
             
             coeff = 2.0_cp*(-1.0_cp)**k*cos(alpha_k*xi(i))/(L*alpha_k**3.0_cp)
             term = coeff*(1.0_cp - V2 - V3)
             w(i,j) = w(i,j) + term
           enddo; enddo
         enddo
         w = w/mu*(-dpdz)*a**2.0_cp
         deallocate(x,y,xi,eta)
       end function

       function init_FD_DuctFlow(cx,cy,sx,sy) result(w)
         implicit none
         integer,intent(in) :: sx,sy
         real(cp),dimension(sx,sy) :: w
         type(coordinates),intent(in) :: cx,cy
         real(cp),dimension(:),allocatable :: hx,hy
         real(cp) :: alpha,height,width,F,A,A1,A2,A3
         real(cp),dimension(2) :: hmin,hmax
         integer :: i,j,n,m,nMax,mMax
         hmin = (/cx%hmin,cy%hmin/); allocate(hx(sx)); width  = (hmax(1) - hmin(1))/2.0_cp
         hmax = (/cx%hmax,cy%hmax/); allocate(hy(sy)); height = (hmax(2) - hmin(2))/2.0_cp
         
             if (sx.eq.cx%sn) then; hx = cx%hn
         elseif (sx.eq.cx%sc) then; hx = cx%hc
           else; stop 'Error: bad size in init_FD_DuctFlow in profile_funcs.f90'
         endif
             if (sy.eq.cy%sn) then; hy = cy%hn
         elseif (sy.eq.cy%sc) then; hy = cy%hc
           else; stop 'Error: bad size in init_FD_DuctFlow in profile_funcs.f90'
         endif
         ! Iterations in infinite series:
         nMax = 100; mMax = 100
         F = 1.0_cp ! This is dynamic viscosity or proportional to it or something...
         alpha = width/height
         do i=1,sx
           do j=1,sy
             do m=1,mMax
               do n=1,nMax
               A1 = 16.0_cp*F*alpha**2.0_cp*height**2.0_cp/&
               ((real(m,cp)*PI)**2.0_cp+(alpha*real(n,cp)*PI)**2.0_cp)
               A2 = 1.0_cp/(real(m,cp)*PI)*1.0_cp/(real(n,cp)*PI)
               A3 = (1.0_cp-cos(real(m,cp)*PI))*(1.0_cp-cos(real(n,cp)*PI))
               A = A1*A2*A3
               w(i,j) = w(i,j) + A*sin(real(m,cp)*PI*(hx(i)-hmin(1))/(2.0_cp*width))*&
                                             sin(real(n,cp)*PI*(hy(j)-hmin(2))/(2.0_cp*height))
               enddo
             enddo
           enddo
         enddo
         deallocate(hx,hy)
       end function

       function parabolic1D(c,s) result(u)
         implicit none
         type(coordinates),intent(in) :: c
         integer,intent(in) :: s
         real(cp),dimension(s) :: u
         real(cp),dimension(:),allocatable :: h
         integer :: i
         real(cp) :: Re
         Re = 200.0_cp
         allocate(h(s))
             if (s.eq.c%sn) then; h = c%hn
         elseif (s.eq.c%sc) then; h = c%hc
         else; stop 'Error: bad size in parabolic1D in profile_funcs.f90'
         endif
         do i=1,s
           u(i) = 0.5_cp*Re*(1.0_cp - h(i)**2.0_cp)
         enddo
         deallocate(h)
       end function

       function isolatedEddy2D(cx,cy,sx,sy,component) result(f)
         ! Computes velocities fx,fy from reference:
         !      "Weiss, N. O. The Expulsion of Magnetic Flux 
         !      by Eddies. Proc. R. Soc. A Math. Phys. Eng.
         !      Sci. 293, 310–328 (1966).""
         ! 
         ! Computes
         !           U = curl(psi)
         ! Where
         !           psi = (-1/(2 pi)) cos(2 pi x) cos(2 pi y)
         !           u = dpsi/dy
         !           v =-dpsi/dx
         !           w = 0
         ! Computes
         !       u = dpsi/dy =   cos(2 pi x) sin(2 pi y)
         !       v =-dpsi/dx = - sin(2 pi x) cos(2 pi y)
         ! 
         ! Component = (1,2) = (x,y)
         implicit none
         integer,intent(in) :: sx,sy
         real(cp),dimension(sx,sy) :: f
         type(coordinates),intent(in) :: cx,cy
         integer,intent(in) :: component
         integer :: i,j
         real(cp) :: wavenum
         real(cp),dimension(:),allocatable :: x,y
         if (sx.lt.1) stop 'Error: sx < 1 in isolatedEddy2D in profile_funcs.f90'
         if (sy.lt.1) stop 'Error: sy < 1 in isolatedEddy2D in profile_funcs.f90'
         allocate(x(sx)); allocate(y(sy))
             if (sx.eq.cx%sn) then; x = cx%hn
         elseif (sx.eq.cx%sc) then; x = cx%hc
           else; stop 'Error: bad size in isolatedEddy2D in profile_funcs.f90'
         endif
             if (sy.eq.cy%sn) then; y = cy%hn
         elseif (sy.eq.cy%sc) then; y = cy%hc
           else; stop 'Error: bad size in isolatedEddy2D in profile_funcs.f90'
         endif

         wavenum = 2.0_cp
         select case (component)
         case (1); do j=1,sy;do i=1,sx
                     f(i,j) =   cos(wavenum*PI*x(i)) * &
                                sin(wavenum*PI*y(j))
                   enddo;enddo
         case (2); do j=1,sy;do i=1,sx
                     f(i,j) = - sin(wavenum*PI*x(i)) * &
                                cos(wavenum*PI*y(j))
                   enddo;enddo
         case default; stop 'Error: bad component input in isolatedEddy2D in profile_funcs.f90'
         end select
         deallocate(x,y)
       end function

       ! subroutine isolatedEddy2D(u,v,g)
       !   ! From
       !   !      Weiss, N. O. The Expulsion of Magnetic Flux 
       !   !      by Eddies. Proc. R. Soc. A Math. Phys. Eng.
       !   !      Sci. 293, 310–328 (1966).
       !   ! 
       !   ! Computes
       !   !           U = curl(psi)
       !   ! Where
       !   !           psi = (-1/(2 pi)) cos(2 pi x) cos(2 pi y)
       !   !           u = dpsi/dy
       !   !           v =-dpsi/dx
       !   !           w = 0
       !   ! Computes
       !   !       u = dpsi/dy =   cos(2 pi x) sin(2 pi y)
       !   !       v =-dpsi/dx = - sin(2 pi x) cos(2 pi y)
       !   implicit none
       !   real(cp),dimension(:,:),intent(inout) :: u,v
       !   type(grid),intent(in) :: g
       !   integer :: i,j
       !   integer,dimension(2) :: sx,sy
       !   real(cp) :: wavenum
       !   wavenum = 2.0_cp
       !   sx = shape(u); sy = shape(v)
       !   do j=1,sx(2);do i=1,sx(1)
       !        u(i,j) =   cos(wavenum*PI*g%c(1)%hn(i)) * &
       !                   sin(wavenum*PI*g%c(2)%hc(j))
       !   enddo;enddo
       !   do j=1,sy(2);do i=1,sy(1)
       !        v(i,j) = - sin(wavenum*PI*g%c(1)%hc(i)) * &
       !                   cos(wavenum*PI*g%c(2)%hn(j))
       !   enddo;enddo
       ! end subroutine

       subroutine singleEddy2D(u,v,g)
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
         !       u = dpsi/dy =   (32y/pi) cos(pi x) (1-4 y^2)^3
         !       v =-dpsi/dx = - (1-4 y^2)^4 sin(pi x)
         implicit none
         real(cp),dimension(:,:),intent(inout) :: u,v
         type(grid),intent(in) :: g
         integer :: i,j
         integer,dimension(2) :: sx,sy
         real(cp) :: one,two,three,four
         one = 1.0_cp; two = 2.0_cp
         three = 3.0_cp; four = 4.0_cp
         sx = shape(u); sy = shape(v)
         do j=1,sx(2);do i=1,sx(1)
              u(i,j) =   (32.0_cp*g%c(2)%hc(j)/PI)*((one-four*g%c(2)%hc(j)**two)**three) * &
              cos(PI*g%c(1)%hn(i))
         enddo;enddo
         do j=1,sy(2);do i=1,sy(1)
              v(i,j) = - ((one-four*g%c(2)%hn(j)**two)**four)*sin(PI*g%c(1)%hc(i))
         enddo;enddo
       end subroutine

       subroutine cylinder2D(u,v,g)
         ! From
         !      Moffatt
         ! 
         ! Computes
         !           U(r) = omega0*r
         ! for
         !           0 < r < r0
         ! 
         implicit none
         real(cp),dimension(:,:),intent(inout) :: u,v
         type(grid),intent(in) :: g
         integer :: i,j
         integer,dimension(2) :: sx,sy
         real(cp),dimension(3) :: hc
         real(cp) :: omega0,r0,two,r,theta
         two = 2.0_cp
         omega0 = 1.0_cp; r0 = 1.0_cp
         u = 0.0_cp; v = 0.0_cp

         sx = shape(u); sy = shape(v)
         hc = (/((g%c(i)%hmax+g%c(i)%hmin)/2.0_cp,i=1,3)/)

         do j=1,sx(2);do i=1,sx(1)
              r = sqrt((g%c(1)%hn(i)-hc(1))**two + (g%c(2)%hc(j)-hc(2))**two)
              theta = atan2(g%c(2)%hc(j),g%c(1)%hn(i))
              if (r.lt.r0) u(i,j) =-omega0*r*sin(theta)
         enddo;enddo
         do j=1,sy(2);do i=1,sy(1)
              r = sqrt((g%c(1)%hc(i)-hc(1))**two + (g%c(2)%hn(j)-hc(2))**two)
              theta = atan2(g%c(2)%hn(j),g%c(1)%hc(i))
              if (r.lt.r0) v(i,j) = omega0*r*cos(theta)
         enddo;enddo
       end subroutine

       ! subroutine vortex2D(U,g,s,dir,vsign,directory)
       !   implicit none
       !   type(VF),intent(inout) :: U
       !   type(grid),intent(in) :: g
       !   integer,dimension(3),intent(in) :: s
       !   integer,intent(in) :: dir,vsign
       !   character(len=*),intent(in) :: directory
       !   integer :: Nx,Ny,Nz
       !   integer :: i,j,k
       !   ! Vortex variables
       !   real(cp) :: omega0,r,alpha,r0
       !   type(scalarField) :: omega,psi
       !   type(BCs) :: psi_bcs
       !   type(solverSettings) :: ss_psi
       !   type(VF) :: tempVF,temp
       !   type(norms) :: norm
       !   type(mySOR) :: SOR
       !   real(cp) :: two,one
       !   integer,dimension(2) :: d
       !   two = 2.0_cp; one = 1.0_cp
       !   d = orthogonalDirections(dir)
       !   call setAllZero(psi_bcs,s,5)
       !   call checkBCs(psi_bcs)
       !   call writeAllBoundaries(psi_bcs,directory//'parameters/','psi')
       !   omega0 = 1000.0
       !   alpha = 10000.0
       !   call init(omega,s)
       !   do k = 1,s(3)
       !     do j = 1,s(2)
       !       do i = 1,s(1)
       !         r0 = sqrt( (g%c(d(1))%hmax-g%c(d(1))%hmin)**two + &
       !                    (g%c(d(2))%hmax-g%c(d(2))%hmin)**two )
       !         r = sqrt( g%c(d(1))%hc(i)**two + & 
       !                   g%c(d(2))%hc(j)**two )/r0
       !         omega(i,j,k) = omega0*(one - r**two)*exp(-alpha*r**two)
       !       enddo
       !     enddo
       !   enddo
       !   call init(psi,s)
       !   call init(ss_psi)
       !   call setMaxIterations(ss_psi,100)
       !   call poisson(SOR,psi%phi,omega%phi,psi_bcs,g,ss_psi,norm,.true.)
       !   call writeToFile(g,omega,directory//'Ufield/','omega')
       !   call writeToFile(g,psi,directory//'Ufield/','psi')
       !   call delete(omega)
       !   call allocateX(tempVF,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
       !   call allocateY(tempVF,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
       !   call allocateZ(tempVF,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)
       !   call curl(U,tempVF,g)
       !   call delete(psi)
       !   call delete(tempVF)
       ! end subroutine
       

       ! subroutine initVortex(u,v,w,g,vdir,vsign,dir)
       !   implicit none
       !   real(cp),dimension(:,:,:),intent(inout) :: u,v,w
       !   type(grid),intent(in) :: g
       !   integer,intent(in) :: vdir,vsign
       !   character(len=*),intent(in) :: dir
       !   integer :: Nx,Ny,Nz
       !   real(cp) :: x_0,x_N
       !   real(cp) :: y_0,y_N
       !   real(cp) :: z_0,z_N
       !   integer :: i,j,k
       !   real(cp),dimension(:),allocatable :: xn,yn,zn
       !   real(cp),dimension(:),allocatable :: xc,yc,zc
       !   ! Vortex variables
       !   real(cp) :: omega0,r,alpha,r0
       !   real(cp),dimension(:,:,:),allocatable :: omega,psi
       !   type(BCs) :: psi_bcs
       !   type(solverSettings) :: ss_psi
       !   real(cp),dimension(:,:,:),allocatable :: tempx,tempy,tempz,temp
       !   type(myError) :: err
       !   real(cp),dimension(3) :: hmin,hmax
       !   type(mySOR) :: SOR
       !   hmin = (/g%c(1)%hmin,g%c(2)%hmin,g%c(3)%hmin/)
       !   hmax = (/g%c(1)%hmax,g%c(2)%hmax,g%c(3)%hmax/)
       !   x_0 = g%c(1)%hmin; y_0 = g%c(2)%hmin; z_0 = g%c(3)%hmin
       !   x_N = g%c(1)%hmax; y_N = g%c(2)%hmax; z_N = g%c(3)%hmax
       !   allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
       !   allocate(xn(g%c(1)%sn),yn(g%c(2)%sn),zn(g%c(3)%sn))
       !   xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc
       !   xn = g%c(1)%hn; yn = g%c(2)%hn; zn = g%c(3)%hn
       !   call setAllZero(psi_bcs,Nx+2,Ny+2,Nz+2,5)
       !   call checkBCs(psi_bcs)
       !   call writeAllBoundaries(psi_bcs,dir//'parameters/','psi')
       !   omega0 = 1000.0
       !   alpha = 10000.0
       !   allocate(omega(Nx+2,Ny+2,Nz+2))
       !   do i=1,Nx+2
       !     do j = 1,Ny+2
       !       do k = 1,Nz+2
       !          r0 = sqrt( (x_N-x_0)**2.0 + (y_N-y_0)**2.0 + (z_N-z_0)**2.0 )
       !          r = sqrt( xc(i)**2.0 + yc(j)**2.0 + zc(k)**2.0 )/r0
       !          omega(i,j,k) = omega0*(1.0 - r**2.0)*exp(-alpha*r**2.0)
       !       enddo
       !     enddo
       !   enddo
       !   allocate(psi(Nx+2,Ny+2,Nz+2))
       !   call init(ss_psi)
       !   call setMaxIterations(ss_psi,100)
       !   omega = -omega
       !   call myPoisson(SOR,psi,omega,psi_bcs,g,ss_psi,err,1,.true.)
       !   call writeToFile(xc,yc,zc,omega,dir//'Ufield/','omega')
       !   call writeToFile(xc,yc,zc,psi,dir//'Ufield/','psi')
       !   deallocate(omega)
       !   allocate(tempx(Nx+2,Ny+2,Nz+2))
       !   allocate(tempy(Nx+2,Ny+2,Nz+2))
       !   allocate(tempz(Nx+2,Ny+2,Nz+2))
       !   allocate( temp(Nx+2,Ny+2,Nz+2))
       !   call myCC2FaceGrad(tempx,tempy,tempz,psi,g)
       !   call myFace2CellCenter(temp,tempy(:,1:Ny+1,:),g,2)
       !   call myCellCenter2Face(u,temp,g,1)
       !   call myFace2CellCenter(temp,tempx(1:Nx+1,:,:),g,1)
       !   call myCellCenter2Face(v,temp,g,2)
       !   v = -v
       !   deallocate(psi)
       !   w = 0.0_cp
       !   deallocate(tempx,tempy,tempz,temp)
       !   deallocate(xn,yn,zn)
       !   deallocate(xc,yc,zc)
       ! end subroutine

       end module