       module initializeUfield_mod
       use constants_mod
       use myIO_mod
       use myDebug_mod
       use simParams_mod
       use grid_mod
       use myDel_mod
       use vectorOps_mod
       use BCs_mod
       use applyBCs_mod
       use myError_mod
       use solverSettings_mod
       use mySOR_mod
       use myPoisson_mod
       implicit none

       private
       public :: initUfield

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

       subroutine initUfield(u,v,w,p,g,dir)
         implicit none
         ! Auxiliary data types
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p
         if (restartU) then
           call initRestartUfield(u,v,w,p,g,dir)
         elseif (preDefinedU_ICs.ne.0) then
           call initPreDefinedUfield(u,v,w,p,g,dir)
         else
           call initUserUfield(u,v,w,p)
         endif
       end subroutine
       
       subroutine initRestartUfield(u,v,w,p,g,dir)
         implicit none
         ! Auxiliary data types
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p
         ! Coordinates
         call readFromFile(g%c(1)%hn,g%c(2)%hc,g%c(3)%hc,u,dir//'Ufield/','ufi')
         call readFromFile(g%c(1)%hc,g%c(2)%hn,g%c(3)%hc,v,dir//'Ufield/','vfi')
         call readFromFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hn,w,dir//'Ufield/','wfi')
         call readFromFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,p,dir//'Ufield/','pci')
       end subroutine
       
       subroutine initPreDefinedUfield(u,v,w,p,g,dir)
         implicit none
         ! Auxiliary data types
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p

         ! real(cp),dimension(:,:),allocatable :: bvals
         integer :: Nx,Ny,Nz
         real(cp) :: x_0,x_N
         real(cp) :: y_0,y_N
         real(cp) :: z_0,z_N
         integer :: i,j,k
         ! Coordinates
         real(cp),dimension(:),allocatable :: xn,yn,zn
         real(cp),dimension(:),allocatable :: xc,yc,zc

         ! Vortex variables
         real(cp) :: omega0,r,alpha,r0
         real(cp),dimension(:,:,:),allocatable :: omega,psi
         type(BCs) :: psi_bcs
         type(solverSettings) :: ss_psi
         real(cp),dimension(:,:,:),allocatable :: tempx,tempy,tempz,temp
         type(myError) :: err
         real(cp),dimension(3) :: hmin,hmax
         type(mySOR) :: SOR


         hmin = (/g%c(1)%hmin,g%c(2)%hmin,g%c(3)%hmin/)
         hmax = (/g%c(1)%hmax,g%c(2)%hmax,g%c(3)%hmax/)

         x_0 = g%c(1)%hmin; y_0 = g%c(2)%hmin; z_0 = g%c(3)%hmin
         x_N = g%c(1)%hmax; y_N = g%c(2)%hmax; z_N = g%c(3)%hmax

         allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
         allocate(xn(g%c(1)%sn),yn(g%c(2)%sn),zn(g%c(3)%sn))

         xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc
         xn = g%c(1)%hn; yn = g%c(2)%hn; zn = g%c(3)%hn


         u = 0.0; v = 0.0; v = 0.0
         p = 0.0

         select case (preDefinedU_ICs)

         case (1) ! Rest

           u = 0.0; v = 0.0; v = 0.0
           p = 0.0

         case (2) ! Fully Developed Duct Flow (in x)

           u = 0.0; v = 0.0; w = 0.0

           do j = 1,Ny
             do k = 1,Nz
               u(:,j,k) = 10.0*(1.0 - yn(j)**2.0)*(1.0 - zn(k)**2.0)
               v(:,j,k) = 0.0
               w(:,j,k) = 0.0
             enddo
           enddo
           call initFullyDevelopedDuctFlow(u,v,w,p,g,1)

         case (3) ! Vortex

           call setAllZero(psi_bcs,Nx+2,Ny+2,Nz+2,5)
           call checkBCs(psi_bcs)
           call writeAllBoundaries(psi_bcs,dir//'parameters/','psi')

           omega0 = 1000.0
           alpha = 10000.0
           allocate(omega(Nx+2,Ny+2,Nz+2))
           do i=1,Nx+2
             do j = 1,Ny+2
               do k = 1,Nz+2
                  r0 = sqrt( (x_N-x_0)**2.0 + (y_N-y_0)**2.0 + (z_N-z_0)**2.0 )
                  r = sqrt( xc(i)**2.0 + yc(j)**2.0 + zc(k)**2.0 )/r0
                  omega(i,j,k) = omega0*(1.0 - r**2.0)*exp(-alpha*r**2.0)
               enddo
             enddo
           enddo
           allocate(psi(Nx+2,Ny+2,Nz+2))
           call init(ss_psi)
           call setMaxIterations(ss_psi,100)
           omega = -omega
           call myPoisson(SOR,psi,omega,psi_bcs,g,ss_psi,err,1,.true.)
!            call myPoisson(psi,omega,psi_bcs,g,ss_psi,err,1,.true.)

           call writeToFile(xc,yc,zc,omega,dir//'Ufield/','omega')
           call writeToFile(xc,yc,zc,psi,dir//'Ufield/','psi')
           deallocate(omega)

           allocate(tempx(Nx+2,Ny+2,Nz+2))
           allocate(tempy(Nx+2,Ny+2,Nz+2))
           allocate(tempz(Nx+2,Ny+2,Nz+2))
           allocate( temp(Nx+2,Ny+2,Nz+2))

           call myCC2FaceGrad(tempx,tempy,tempz,psi,g)

           call myFace2CellCenter(temp,tempy(:,1:Ny+1,:),g,2)
           call myCellCenter2Face(u,temp,g,1)

           call myFace2CellCenter(temp,tempx(1:Nx+1,:,:),g,1)
           call myCellCenter2Face(v,temp,g,2)
           v = -v

           deallocate(psi)
           w = 0.0
           deallocate(tempx,tempy,tempz,temp)

         end select

         deallocate(xn,yn,zn)
         deallocate(xc,yc,zc)
       end subroutine

       subroutine initFullyDevelopedDuctFlow(u,v,w,p,g,dir)
         ! This routine initializes a fully developed duct flow
         ! profile along direction dir.
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p
         type(grid),intent(in) :: g
         integer,intent(in) :: dir

         integer :: i,j,imax,jmax
         ! Coordinates
         real(cp),dimension(:),allocatable :: xc,yc,zc
         real(cp),dimension(:),allocatable :: xn,yn,zn
         real(cp),dimension(:),allocatable :: hx,hy

         ! Vortex variables
         real(cp) :: alpha,height,width,F,A,A1,A2,A3
         real(cp),dimension(:,:),allocatable :: u_temp
         real(cp),dimension(3) :: hmin,hmax
         integer :: n,m,nMax,mMax,x,y,z
         integer,dimension(3) :: s,Ni

         hmin = (/g%c(1)%hmin,g%c(2)%hmin,g%c(3)%hmin/)
         hmax = (/g%c(1)%hmax,g%c(2)%hmax,g%c(3)%hmax/)
         Ni = (/g%c(1)%sc,g%c(2)%sc,g%c(3)%sc/)

         allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
         allocate(xn(g%c(1)%sn),yn(g%c(2)%sn),zn(g%c(3)%sn))
         xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc
         xn = g%c(1)%hn; yn = g%c(2)%hn; zn = g%c(3)%hn

         u = 0.0d0; v = 0.0d0; w = 0.0d0; p = 0.0d0

         ! For max number of iterations in 
         ! infinite series solution:
         nMax = 100; mMax = 100
         F = 1.0d0

         select case (dir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         end select

         select case (dir)
         case (1) ! u(y,z)
           s = shape(u)
           width = (hmax(2) - hmin(2))/2.0d0
           height = (hmax(3) - hmin(3))/2.0d0
           imax = Ni(2); jmax = Ni(3)
           allocate(hx(imax)); hx = yc
           allocate(hy(jmax)); hy = zc
           allocate(u_temp(s(2),s(3)))
         case (2) ! v(x,z)
           s = shape(v)
           width = (hmax(1) - hmin(1))/2.0d0
           height = (hmax(3) - hmin(3))/2.0d0
           imax = Ni(1); jmax = Ni(3)
           allocate(hx(imax)); hx = xc
           allocate(hy(jmax)); hy = zc
           allocate(u_temp(s(1),s(3)))
         case (3) ! w(x,y)
           s = shape(w)
           width = (hmax(1) - hmin(1))/2.0d0
           height = (hmax(2) - hmin(2))/2.0d0
           imax = Ni(1); jmax = Ni(2)
           allocate(hx(imax)); hx = xc
           allocate(hy(jmax)); hy = yc
           allocate(u_temp(s(1),s(2)))
         end select
         alpha = width/height

         do i=1,imax
           do j=1,jmax
             do m=1,mMax
               do n=1,nMax
               A1 = 16.0d0*F*alpha**2.0d0*height**2.0d0/((dble(m)*PI)**2.0d0+(alpha*dble(n)*PI)**2.0d0)
               A2 = 1.0d0/(dble(m)*PI)*1.0d0/(dble(n)*PI)
               A3 = (1.0d0-cos(dble(m)*PI))*(1.0d0-cos(dble(n)*PI))
               A = A1*A2*A3
               u_temp(i,j) = u_temp(i,j) + A*sin(dble(m)*PI*(hx(i)-hmin(1))/(2.0d0*width))*&
                                             sin(dble(n)*PI*(hy(j)-hmin(2))/(2.0d0*height))
               enddo
             enddo
           enddo
         enddo

         select case (dir)
         case (1); do i=1,s(1); u(i,:,:) = u_temp; enddo
         case (2); do i=1,s(2); v(:,i,:) = u_temp; enddo
         case (3); do i=1,s(3); w(:,:,i) = u_temp; enddo
         end select

         deallocate(u_temp)
         deallocate(hx,hy)

         deallocate(xn,yn,zn)
         deallocate(xc,yc,zc)
       end subroutine

       subroutine initUserUfield(u,v,w,p)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u,v,w,p

         u = 0.0d0; v = 0.0d0; w = 0.0d0
         p = 0.0d0
       end subroutine
       
       end module
