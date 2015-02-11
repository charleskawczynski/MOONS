       module initializeBBCs_mod
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
       public :: initializeBBCs

       contains

       subroutine initializeBBCs(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,gd)
         implicit none
         type(griddata),intent(in) :: gd
         type(BCs),intent(inout) :: phi_bcs
         type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
         if (preDefinedB_BCs.ne.0) then
           call initializePreDefinedBfield(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,gd)
         else
           call initializeUserBBCs(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,gd)
         endif
         call setGrid(Bx_bcs,gd)
         call setGrid(By_bcs,gd)
         call setGrid(Bz_bcs,gd)
         if (cleanB) call setGrid(phi_bcs,gd)
         call checkBCs(Bx_bcs)
         call checkBCs(By_bcs)
         call checkBCs(Bz_bcs)
         if (cleanB) call checkBCs(phi_bcs)
       end subroutine

       subroutine initializePreDefinedBfield(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,gd)
         implicit none
         type(griddata),intent(in) :: gd
         type(BCs),intent(inout) :: phi_bcs
         type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
         integer :: Nx,Ny,Nz,neumann,dirichlet

         ! ************** BOUNDARY CONDITIONS ***************************
         ! --------------------------------------------------------------
         ! B-field boundary conditions
         call myAllocate(Nx,Ny,Nz,gd,BLoc)
         select case (BLoc)
         case (dom_cc_tot); dirichlet = 2; neumann = 5
         case (dom_n_tot);  dirichlet = 1; neumann = 4
         end select
         
         select case (preDefinedB_BCs)
         case (1) ! Pseudo Vaccuum
           call setAllZero(Bx_bcs,Nx,Ny,Nz,dirichlet)
           call setXminType(Bx_bcs,neumann)
           call setXmaxType(Bx_bcs,neumann)

           call setAllZero(By_bcs,Nx,Ny,Nz,dirichlet)
           call setYminType(By_bcs,neumann)
           call setYmaxType(By_bcs,neumann)

           call setAllZero(Bz_bcs,Nx,Ny,Nz,dirichlet)
           call setZminType(Bz_bcs,neumann)
           call setZmaxType(Bz_bcs,neumann)
         case (2) ! B = 0
           call setAllZero(Bx_bcs,Nx,Ny,Nz,dirichlet)
           call setAllZero(By_bcs,Nx,Ny,Nz,dirichlet)
           call setAllZero(Bz_bcs,Nx,Ny,Nz,dirichlet)
         case default
           write(*,*) 'Incorrect preDefinedB_BCs in initializePreDefinedBfield';stop
         end select

         ! --------------------------------------------------------------
         if (cleanB) then
           ! phi boundary conditions
           call myAllocate(Nx,Ny,Nz,gd,phiLoc)
           call setAllZero(phi_bcs,Nx,Ny,Nz,neumann)
         endif
       end subroutine

       subroutine initializeUserBBCs(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,gd)
         implicit none
         type(griddata),intent(in) :: gd
         type(BCs),intent(inout) :: phi_bcs
         type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
         integer :: Nx,Ny,Nz,neumann,dirichlet

         ! ************** BOUNDARY CONDITIONS ***************************
         ! --------------------------------------------------------------
         ! B-field boundary conditions
         call myAllocate(Nx,Ny,Nz,gd,BLoc)
         select case (BLoc)
         case (dom_cc_tot); dirichlet = 2; neumann = 5
         case (dom_n_tot);  dirichlet = 1; neumann = 4
         end select
         
         call setAllZero(Bx_bcs,Nx,Ny,Nz,dirichlet)
         call setXminType(Bx_bcs,neumann)
         call setXmaxType(Bx_bcs,neumann)

         call setAllZero(By_bcs,Nx,Ny,Nz,dirichlet)
         call setYminType(By_bcs,neumann)
         call setYmaxType(By_bcs,neumann)

         call setAllZero(Bz_bcs,Nx,Ny,Nz,dirichlet)
         call setZminType(Bz_bcs,neumann)
         call setZmaxType(Bz_bcs,neumann)

         ! --------------------------------------------------------------
         if (cleanB) then
           ! phi boundary conditions
           call myAllocate(Nx,Ny,Nz,gd,phiLoc)
           call setAllZero(phi_bcs,Nx,Ny,Nz,neumann)
         endif
       end subroutine

       end module