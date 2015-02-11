       module initializeUBCs_mod
       use constants_mod
       use myIO_mod
       use griddata_mod
       use myAllocate_mod
       use simParams_mod
       use BCs_mod
       use applyBCs_mod
       implicit none
       ! From applyBCs.f90:
       ! bctype = 1 ! Dirichlet - direct - wall coincident
       ! bctype = 2 ! Dirichlet - interpolated - wall incoincident
       ! bctype = 3 ! Neumann - direct - wall coincident ~O(dh^2)
       ! bctype = 4 ! Neumann - direct - wall coincident ~O(dh)
       ! bctype = 5 ! Neumann - interpolated - wall incoincident ~O(dh)

       private
       public :: initializeUBCs
       
       contains

       subroutine initializeUBCs(u_bcs,v_bcs,w_bcs,p_bcs,gd)
         implicit none
         ! Auxiliary data types
         type(griddata),intent(in) :: gd
         type(BCs),intent(inout) :: p_bcs
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         if (preDefinedU_BCs.ne.0) then
           call initializePredefinedUBCs(u_bcs,v_bcs,w_bcs,p_bcs,gd)
         else
           call initializeUserUBCs(u_bcs,v_bcs,w_bcs,p_bcs,gd)
         endif
         call setGrid(u_bcs,gd)
         call setGrid(v_bcs,gd)
         call setGrid(w_bcs,gd)
         call setGrid(p_bcs,gd)
         call checkBCs(u_bcs)
         call checkBCs(v_bcs)
         call checkBCs(w_bcs)
         call checkBCs(p_bcs)
       end subroutine

       subroutine initializePredefinedUBCs(u_bcs,v_bcs,w_bcs,p_bcs,gd)
         implicit none
         ! Auxiliary data types
         type(griddata),intent(in) :: gd
         type(BCs),intent(inout) :: p_bcs
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         real(dpn),dimension(:,:),allocatable :: bvals
         ! Coordinates
         integer :: Nx,Ny,Nz

         select case (preDefinedU_BCs)
         case (1) ! Lid Driven Cavity

           ! U-field boundary conditions
           call noSlipNoFlowThroughBCs(u_bcs,v_bcs,w_bcs,gd)

           ! call myAllocate(Nx,Ny,Nz,gd,ULoc,1)
           ! allocate(bvals(Nx,Nz)); bvals = 1.0
           ! call setYmaxVals(u_bcs,bvals); deallocate(bvals)

           call initializeLidDrivenBCs(u_bcs,v_bcs,w_bcs,gd,&
            drivenFace,drivenDirection,drivenSign)

         case (2) ! No Slip Cavity
           call noSlipNoFlowThroughBCs(u_bcs,v_bcs,w_bcs,gd)

         case (3) ! Duct Flow (in x)

           ! U-field boundary conditions
           call myAllocate(Nx,Ny,Nz,gd,ULoc,1)
           call setAllZero(u_bcs,Nx,Ny,Nz,2)
           call setXminType(u_bcs,1) ! Dirichlet
           allocate(bvals(Ny,Nz)); bvals = 1.0
           call setXminVals(u_bcs,bvals); deallocate(bvals)
           call setXmaxType(u_bcs,3) ! Neumann

           call myAllocate(Nx,Ny,Nz,gd,ULoc,2)
           call setAllZero(v_bcs,Nx,Ny,Nz,2)
           call setYminType(v_bcs,1)
           call setYmaxType(v_bcs,1)
           call setXmaxType(v_bcs,5) ! Neumann

           call myAllocate(Nx,Ny,Nz,gd,ULoc,3)
           call setAllZero(w_bcs,Nx,Ny,Nz,2)
           call setZminType(w_bcs,1)
           call setZmaxType(w_bcs,1)
           call setXmaxType(w_bcs,5) ! Neumann

           ! P-field boundary conditions
           call myAllocate(Nx,Ny,Nz,gd,PLoc)
           call setAllZero(p_bcs,Nx,Ny,Nz,5)

         case (4) ! Cylinder Driven Cavity Flow (tornado)
           call initializeCylinderDrivenBCs(u_bcs,v_bcs,w_bcs,gd,1)
         end select

         ! P-field boundary conditions
         call myAllocate(Nx,Ny,Nz,gd,PLoc)
         call setAllZero(p_bcs,Nx,Ny,Nz,5)
       end subroutine

       subroutine initializeUserUBCs(u_bcs,v_bcs,w_bcs,p_bcs,gd)
         implicit none
         ! Auxiliary data types
         type(griddata),intent(in) :: gd
         type(BCs),intent(inout) :: p_bcs
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         real(dpn),dimension(:,:),allocatable :: bvals
         integer :: Nx,Ny,Nz

         ! U-field boundary conditions
         call myAllocate(Nx,Ny,Nz,gd,ULoc,1)
         call setAllZero(u_bcs,Nx,Ny,Nz,2)
         allocate(bvals(Nx,Nz));
         bvals = 1.0
         call setYmaxVals(u_bcs,bvals); deallocate(bvals)
         call setXminType(u_bcs,1)
         call setXmaxType(u_bcs,1)

         call myAllocate(Nx,Ny,Nz,gd,ULoc,2)
         call setAllZero(v_bcs,Nx,Ny,Nz,2)
         call setYminType(v_bcs,1)
         call setYmaxType(v_bcs,1)

         call myAllocate(Nx,Ny,Nz,gd,ULoc,3)
         call setAllZero(w_bcs,Nx,Ny,Nz,2)
         call setZminType(w_bcs,1)
         call setZmaxType(w_bcs,1)

         ! P-field boundary conditions
         call myAllocate(Nx,Ny,Nz,gd,PLoc)
         call setAllZero(p_bcs,Nx,Ny,Nz,5)
       end subroutine

       subroutine initializeLidDrivenBCs(u_bcs,v_bcs,w_bcs,gd,face,dir,posNeg)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         type(griddata),intent(in) :: gd
         integer,intent(in) :: face,dir,posNeg
         real(dpn),dimension(:,:),allocatable :: bvals
         integer :: Nx,Ny,Nz
         call myAllocate(Nx,Ny,Nz,gd,ULoc,dir)
         select case (face)
         case (1) ! xmin
           allocate(bvals(Ny,Nz)); bvals = sign(dble(1.0),dble(posNeg));
           select case (dir)
           case (1); write(*,*) 'Lid driven BCs is violating flow through.';stop
           case (2); call setXminVals(v_bcs,bvals)
           case (3); call setXminVals(w_bcs,bvals)
           end select

         case (2) ! xmax
           allocate(bvals(Ny,Nz)); bvals = sign(dble(1.0),dble(posNeg));
           select case (dir)
           case (1); write(*,*) 'Lid driven BCs is violating flow through.';stop
           case (3); call setXmaxVals(v_bcs,bvals)
           case (2); call setXmaxVals(w_bcs,bvals)
           end select

         case (3) ! ymin
           allocate(bvals(Nx,Nz)); bvals = sign(dble(1.0),dble(posNeg));
           select case (dir)
           case (1); call setYminVals(u_bcs,bvals)
           case (2); write(*,*) 'Lid driven BCs is violating flow through.';stop
           case (3); call setYminVals(w_bcs,bvals)
           end select

         case (4) ! ymax
           allocate(bvals(Nx,Nz)); bvals = sign(dble(1.0),dble(posNeg));
           select case (dir)
           case (1); call setYmaxVals(u_bcs,bvals)
           case (2); write(*,*) 'Lid driven BCs is violating flow through.';stop
           case (3); call setYmaxVals(w_bcs,bvals)
           end select

         case (5) ! zmin
           allocate(bvals(Nx,Ny)); bvals = sign(dble(1.0),dble(posNeg));
           select case (dir)
           case (1); call setZminVals(u_bcs,bvals)
           case (2); call setZminVals(v_bcs,bvals)
           case (3); write(*,*) 'Lid driven BCs is violating flow through.';stop
           end select

         case (6) ! zmax
           allocate(bvals(Nx,Ny)); bvals = sign(dble(1.0),dble(posNeg));
           select case (dir)
           case (1); call setZmaxVals(u_bcs,bvals)
           case (2); call setZmaxVals(v_bcs,bvals)
           case (3); write(*,*) 'Lid driven BCs is violating flow through.';stop
           end select

         end select
         deallocate(bvals)
       end subroutine

       subroutine noSlipNoFlowThroughBCs(u_bcs,v_bcs,w_bcs,gd)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         type(griddata),intent(in) :: gd
         integer :: Nx,Ny,Nz

         call myAllocate(Nx,Ny,Nz,gd,ULoc,1)
         call setAllZero(u_bcs,Nx,Ny,Nz,2)
         call setXminType(u_bcs,1)
         call setXmaxType(u_bcs,1)

         call myAllocate(Nx,Ny,Nz,gd,ULoc,2)
         call setAllZero(v_bcs,Nx,Ny,Nz,2)
         call setYminType(v_bcs,1)
         call setYmaxType(v_bcs,1)

         call myAllocate(Nx,Ny,Nz,gd,ULoc,3)
         call setAllZero(w_bcs,Nx,Ny,Nz,2)
         call setZminType(w_bcs,1)
         call setZmaxType(w_bcs,1)
       end subroutine

       subroutine initializeCylinderDrivenBCs(u_bcs,v_bcs,w_bcs,gd,dir)
         implicit none
         ! Auxiliary data types
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         type(griddata),intent(in) :: gd
         integer,intent(in) :: dir
         real(dpn),dimension(:,:),allocatable :: bvals
         ! Coordinates
         real(dpn),dimension(:),allocatable :: xc,yc,zc
         real(dpn),dimension(:),allocatable :: xn,yn,zn
         real(dpn) :: x_0,y_0,z_0
         real(dpn) :: x_N,y_N,z_N
         real(dpn) :: x0,y0,z0
         real(dpn) :: r,omega0,theta,r0
         integer :: Nx,Ny,Nz,i,j,k
         real(dpn),dimension(3) :: hmin,hmax

         call getRange(gd,hmin,hmax)
         x_0 = hmin(1); y_0 = hmin(2); z_0 = hmin(3)
         x_N = hmax(1); y_N = hmax(2); z_N = hmax(3)

         call myAllocate(Nx,Ny,Nz,gd,interiorCC)
         allocate(xc(Nx),yc(Ny),zc(Nz))
         call myAllocate(Nx,Ny,Nz,gd,interiorN)
         allocate(xn(Nx),yn(Ny),zn(Nz))

         call getXYZcc(gd,xc,yc,zc)
         call getXYZn(gd,xn,yn,zn)

         omega0 = 1.0d0
         r0 = 0.05d0
         x0 = (x_N + x_0)/2.0d0
         y0 = (y_N + y_0)/2.0d0
         z0 = (z_N + z_0)/2.0d0

         select case (dir)
         case (1) ! u = 0
           ! u_bcs
           call myAllocate(Nx,Ny,Nz,gd,ULoc,1)
           call setAllZero(u_bcs,Nx,Ny,Nz,2)
           call setXminType(u_bcs,1)
           call setXmaxType(u_bcs,1)

           ! v_bcs
           call myAllocate(Nx,Ny,Nz,gd,ULoc,2)
           call setAllZero(v_bcs,Nx,Ny,Nz,2)
           allocate(bvals(Ny,Nz));
           do j=1,Ny
             do k=1,Nz
               r = sqrt((yn(j)-y0)**2.0d0 + (zc(k)-z0)**2.0d0)
               if (r.lt.r0) then
                 theta = atan2(zc(k),yn(j))
                 bvals(j,k) = -omega0*r*sin(theta)
               endif
             enddo
           enddo
           call setXmaxVals(v_bcs,bvals); deallocate(bvals)
           call setYminType(v_bcs,1)
           call setYmaxType(v_bcs,1)
           
           ! w_bcs
           call myAllocate(Nx,Ny,Nz,gd,ULoc,3)
           call setAllZero(w_bcs,Nx,Ny,Nz,2)
           allocate(bvals(Nx,Nz));
           do j=1,Ny
             do k=1,Nz
               r = sqrt((yc(j)-y0)**2.0d0 + (zn(k)-z0)**2.0d0)
               if (r.lt.r0) then
                 theta = atan2(zn(k),yc(j))
                 bvals(j,k) = omega0*r*cos(theta)
               endif
             enddo
           enddo
           call setXmaxVals(w_bcs,bvals); deallocate(bvals)
           call setZminType(w_bcs,1)
           call setZmaxType(w_bcs,1)

         case (2) ! v = 0
           ! v_bcs
           call myAllocate(Nx,Ny,Nz,gd,ULoc,2)
           call setAllZero(v_bcs,Nx,Ny,Nz,2)
           call setYminType(v_bcs,1)
           call setYmaxType(v_bcs,1)
           i = 1

         case (3) ! w = 0
           ! v_bcs
           call myAllocate(Nx,Ny,Nz,gd,ULoc,3)
           call setAllZero(w_bcs,Nx,Ny,Nz,2)
           call setYminType(w_bcs,1)
           call setYmaxType(w_bcs,1)

         end select
       end subroutine

       end module