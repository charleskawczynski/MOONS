       module initializeUfield_mod
       use IO_scalarFields_mod
       use IO_scalarBase_mod
       use grid_mod
       use vectorField_mod
       use BCs_mod
       implicit none

       private
       public :: initUfield
       public :: restartU

       logical,parameter :: restartU      = .false.
       integer,parameter :: preDefinedU_ICs = 1
       !                                      0 : User-defined case (no override)
       !                                      1 : Rest (u,v,w = 0)
       !                                      2 : Parabolic Duct Flow (in x)
       !                                      3 : Vortex
       !                                      4 : Isolated Eddy (Weiss)
       !                                      5 : Single Eddy (Weiss)
       !                                      6 : Cylinder (Parker)
       !                                      7 : Parabolic (1D) (Bandaru)
       ! 
       integer,parameter :: ductDirection   = 1 ! (1,2,3) = (x,y,z)
       integer,parameter :: ductSign        = 1 ! (-1,1) = {(-x,-y,-z),(x,y,z)}
       ! 
       integer,parameter :: vortexDirection = 1 ! (1,2,3) = (x,y,z)
       integer,parameter :: vortexSign =      1 ! (-1,1) = {clockwise from +, clockwise from -}


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif
       real(cp),parameter :: PI = real(3.14159265358979,cp)

       contains

       subroutine initUfield(U,p,g,dir)
         implicit none
         type(vectorField),intent(inout) :: U
         real(cp),dimension(:,:,:),intent(inout) :: p
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         if (restartU) then
           call initRestartUfield(U%x,U%y,U%z,p,g,dir)
         elseif (preDefinedU_ICs.ne.0) then
           call initPreDefinedUfield(U%x,U%y,U%z,p,g)
         else
           call initUserUfield(U%x,U%y,U%z,p,g)
         endif
       end subroutine
       
       subroutine initRestartUfield(u,v,w,p,g,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p
         type(grid) :: gtemp
         call init(gtemp,g)
         call readFromFile(gtemp,u,dir//'Ufield/','ufi')
         call readFromFile(gtemp,v,dir//'Ufield/','vfi')
         call readFromFile(gtemp,w,dir//'Ufield/','wfi')
         call readFromFile(gtemp,p,dir//'Ufield/','pci')
         call delete(gtemp)
       end subroutine
       
       subroutine initPreDefinedUfield(u,v,w,p,g)
         implicit none
         ! Auxiliary data types
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p

         call initRest(u,v,w,p)
         select case (preDefinedU_ICs)
         case (1) ! Default
         case (2); call initFullyDevelopedDuctFlow(u,v,w,p,g,ductDirection,ductSign)
         case (3); ! call vortex2D(u,v,w,g,3,1)     ! Vortex
         case (4); call isolatedEddy2D(u,v,w,g,3,1) ! Isolated Eddy (Weiss)
         case (5); call singleEddy2D(u,v,w,g,3,1)   ! Single Eddy (Weiss)
         case (6); call cylinder2D(u,v,w,g,3,1)     ! Cylinder
         case (7); call parabolic1D(u,v,w,p,g)        ! Bandaru (SS of Ha=0)
         end select
       end subroutine

       subroutine initRest(u,v,w,p)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p
         u = real(0.0,cp); v = real(0.0,cp); w = real(0.0,cp); p = real(0.0,cp)
       end subroutine

       subroutine initFullyDevelopedDuctFlow(u,v,w,p,g,dir,posNeg)
         ! This routine initializes a fully developed duct flow
         ! profile along direction dir.
         ! 
         ! There is a copy of this routine inside initUBCs.
         ! THE MASTER COPY OF THIS ROUTINE SHOULD
         ! RESIDE IN INITIALIZE UFIELD, NOT
         ! INITIALIZE UBCs
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p
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
         F = real(1.0,cp)

         select case (dir)
         case (1) ! u(y,z)
           s = g%c(1)%sn
           width = (hmax(2) - hmin(2))/real(2.0,cp)
           height = (hmax(3) - hmin(3))/real(2.0,cp)
           imax = Ni(2); jmax = Ni(3)
           allocate(hx(imax)); hx = g%c(2)%hc
           allocate(hy(jmax)); hy = g%c(3)%hc
           allocate(u_temp(s(2),s(3)))
         case (2) ! v(x,z)
           s = g%c(2)%sn
           width = (hmax(1) - hmin(1))/real(2.0,cp)
           height = (hmax(3) - hmin(3))/real(2.0,cp)
           imax = Ni(1); jmax = Ni(3)
           allocate(hx(imax)); hx = g%c(1)%hc
           allocate(hy(jmax)); hy = g%c(3)%hc
           allocate(u_temp(s(1),s(3)))
         case (3) ! w(x,y)
           s = g%c(3)%sn
           width = (hmax(1) - hmin(1))/real(2.0,cp)
           height = (hmax(2) - hmin(2))/real(2.0,cp)
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
               A1 = real(16.0,cp)*F*alpha**real(2.0,cp)*height**real(2.0,cp)/&
               ((real(m,cp)*PI)**real(2.0,cp)+(alpha*real(n,cp)*PI)**real(2.0,cp))
               A2 = real(1.0,cp)/(real(m,cp)*PI)*real(1.0,cp)/(real(n,cp)*PI)
               A3 = (real(1.0,cp)-cos(real(m,cp)*PI))*(real(1.0,cp)-cos(real(n,cp)*PI))
               A = A1*A2*A3
               u_temp(i,j) = u_temp(i,j) + A*sin(real(m,cp)*PI*(hx(i)-hmin(1))/(real(2.0,cp)*width))*&
                                             sin(real(n,cp)*PI*(hy(j)-hmin(2))/(real(2.0,cp)*height))
               enddo
             enddo
           enddo
         enddo

         select case (dir)
         case (1); do i=1,s(1); u(i,:,:) = sign(real(1.0,cp),real(posNeg,cp))*u_temp; enddo
         case (2); do i=1,s(2); v(:,i,:) = sign(real(1.0,cp),real(posNeg,cp))*u_temp; enddo
         case (3); do i=1,s(3); w(:,:,i) = sign(real(1.0,cp),real(posNeg,cp))*u_temp; enddo
         end select
         p = real(0.0,cp)

         deallocate(u_temp)
         deallocate(hx,hy)
       end subroutine

       subroutine initUserUfield(u,v,w,p,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p
         type(grid),intent(in) :: g
         integer :: j,k
         v = 0.0d0; w = 0.0d0
         p = 0.0d0
         do j=1,g%c(2)%sc
          do k=1,g%c(3)%sc
            u(:,j,k) = (real(2.0,cp) - g%c(2)%hc(j)**real(2.0,cp) - &
                                       g%c(3)%hc(k)**real(2.0,cp))/real(2.0,cp)
          enddo
         enddo
       end subroutine

       subroutine parabolic1D(u,v,w,p,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p
         type(grid),intent(in) :: g
         integer :: k
         real(cp) :: Re
         Re = real(200.0,cp)
         v = 0.0d0; w = 0.0d0; p = 0.0d0

         do k=1,g%c(3)%sc
           u(:,:,k) = real(0.5,cp)*Re*(real(1.0,cp) - g%c(3)%hc(k)**real(2.0,cp))
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
         two = real(2.0,cp)

         sx = shape(u); sy = shape(v); sz = shape(w)
         select case (dir)
         case (1)
           u = real(0.0,cp)
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
           v = real(0.0,cp)
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
           w = real(0.0,cp)
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
         one = real(1.0,cp); two = real(2.0,cp)
         three = real(3.0,cp); four = real(4.0,cp)

         sx = shape(u); sy = shape(v); sz = shape(w)
         select case (dir)
         case (1)
           u = real(0.0,cp)
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
           v = real(0.0,cp)
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
           w = real(0.0,cp)
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
         two = real(2.0,cp)

         omega0 = real(1.0,cp)
         r0 = real(1.0,cp)
         u = real(0.0,cp); v = real(0.0,cp); w = real(0.0,cp)

         sx = shape(u); sy = shape(v); sz = shape(w)
         select case (dir)
         case (1)
           hc = (/((g%c(i)%hmax+g%c(i)%hmin)/real(2.0,cp),i=1,3)/)
           u = real(0.0,cp)
           do k=1,sy(3);do j=1,sy(2);do i=1,sy(1)
                r = sqrt((g%c(2)%hn(j)-hc(2))**two + (g%c(3)%hc(k)-hc(3))**two)
                if (r.lt.r0) v(i,j,k) =-omega0*r
           enddo;enddo;enddo
           do k=1,sz(3);do j=1,sz(2);do i=1,sz(1)
                r = sqrt((g%c(2)%hc(j)-hc(2))**two + (g%c(3)%hn(k)-hc(3))**two)
                if (r.lt.r0) w(i,j,k) = omega0*r
           enddo;enddo;enddo
         case (2)
           hc = (/((g%c(i)%hmax+g%c(i)%hmin)/real(2.0,cp),i=1,3)/)
           do k=1,sx(3);do j=1,sx(2);do i=1,sx(1)
                r = sqrt((g%c(1)%hn(i)-hc(1))**two + (g%c(3)%hn(k)-hc(3))**two)
                if (r.lt.r0) u(i,j,k) = omega0*r
           enddo;enddo;enddo
           v = real(0.0,cp)
           do k=1,sz(3);do j=1,sz(2);do i=1,sz(1)
                r = sqrt((g%c(1)%hn(i)-hc(1))**two + (g%c(3)%hn(k)-hc(3))**two)
                if (r.lt.r0) w(i,j,k) =-omega0*r
           enddo;enddo;enddo
         case (3)
           hc = (/((g%c(i)%hmax+g%c(i)%hmin)/real(2.0,cp),i=1,3)/)
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
           w = real(0.0,cp)
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


       ! subroutine vortex2D(U,g,s,dir,vsign,directory)
       !   implicit none
       !   type(vectorField),intent(inout) :: U
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
       !   type(vectorField) :: tempVF,temp
       !   type(norms) :: norm
       !   type(mySOR) :: SOR
       !   real(cp) :: two,one
       !   integer,dimension(2) :: d
       !   two = real(2.0,cp); one = real(1.0,cp)
       !   d = orthogonalDirections(dir)
       !   call setAllZero(psi_bcs,s,5)
       !   call checkBCs(psi_bcs)
       !   call writeAllBoundaries(psi_bcs,directory//'parameters/','psi')
       !   omega0 = 1000.0
       !   alpha = 10000.0
       !   call allocateField(omega,s)
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
       !   call allocateField(psi,s)
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
       !   w = 0.0
       !   deallocate(tempx,tempy,tempz,temp)
       !   deallocate(xn,yn,zn)
       !   deallocate(xc,yc,zc)
       ! end subroutine

       end module
