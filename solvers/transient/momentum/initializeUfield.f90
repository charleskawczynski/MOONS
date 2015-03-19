       module initializeUfield_mod
       use myIO_mod
       use grid_mod
       use BCs_mod
       implicit none

       private
       public :: initUfield

       logical,parameter :: restartU      = .false.
       integer,parameter :: preDefinedU_ICs = 1
       !                                      0 : User-defined case (no override)
       !                                      1 : Rest (u,v,w = 0)
       !                                      2 : Parabolic Duct Flow (in x)
       !                                      3 : Vortex
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
       real(cp),parameter :: PI = 3.14159265358979

       contains

       subroutine initUfield(u,v,w,p,g,dir)
         implicit none
         ! Auxiliary data types
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p
         if (restartU) then
           call initRestartUfield(u,v,w,p,g,dir)
         elseif (preDefinedU_ICs.ne.0) then
           call initPreDefinedUfield(u,v,w,p,g)
         else
           call initUserUfield(u,v,w,p)
         endif
       end subroutine
       
       subroutine initRestartUfield(u,v,w,p,g,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p
         call readFromFile(g%c(1)%hn,g%c(2)%hc,g%c(3)%hc,u,dir//'Ufield/','ufi')
         call readFromFile(g%c(1)%hc,g%c(2)%hn,g%c(3)%hc,v,dir//'Ufield/','vfi')
         call readFromFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hn,w,dir//'Ufield/','wfi')
         call readFromFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,p,dir//'Ufield/','pci')
       end subroutine
       
       subroutine initPreDefinedUfield(u,v,w,p,g)
         implicit none
         ! Auxiliary data types
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p

         select case (preDefinedU_ICs)
         case (1) ! Rest
           call initRest(u,v,w,p)
         case (2) ! Fully Developed Duct Flow
           call initRest(u,v,w,p)
           call initFullyDevelopedDuctFlow(u,v,w,p,g,ductDirection,ductSign)
         case (3) ! Vortex
           call initRest(u,v,w,p)
           ! This routine requires too many other modules
           ! think of a better way
           ! call initVortex(u,v,w,g,vortexDirection,vortexSign,dir)
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

       subroutine initUserUfield(u,v,w,p)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p

         u = 0.0d0; v = 0.0d0; w = 0.0d0
         p = 0.0d0
       end subroutine
       
       
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
