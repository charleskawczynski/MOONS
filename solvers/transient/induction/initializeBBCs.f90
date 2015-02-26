       module initializeBBCs_mod
       use simParams_mod
       use myIO_mod
       use grid_mod
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

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: initBBCs

       contains

       subroutine initBBCs(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,g)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: phi_bcs
         type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
         if (preDefinedB_BCs.ne.0) then
           call initPreDefinedBfield(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,g)
         else
           call initUserBBCs(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,g)
         endif
         call setGrid(Bx_bcs,g)
         call setGrid(By_bcs,g)
         call setGrid(Bz_bcs,g)
         if (cleanB) call setGrid(phi_bcs,g)
         call checkBCs(Bx_bcs)
         call checkBCs(By_bcs)
         call checkBCs(Bz_bcs)
         if (cleanB) call checkBCs(phi_bcs)
       end subroutine

       subroutine initPreDefinedBfield(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,g)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: phi_bcs
         type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
         integer :: Nx,Ny,Nz,neumann,dirichlet

         ! ************** BOUNDARY CONDITIONS ***************************
         ! --------------------------------------------------------------
         ! B-field boundary conditions
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         dirichlet = 2; neumann = 5
         
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
           write(*,*) 'Incorrect preDefinedB_BCs in initPreDefinedBfield';stop
         end select

         ! --------------------------------------------------------------
         if (cleanB) then
           ! phi boundary conditions
           call setAllZero(phi_bcs,Nx,Ny,Nz,neumann)
         endif
       end subroutine

       subroutine initUserBBCs(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,g)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: phi_bcs
         type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
         integer :: Nx,Ny,Nz,neumann,dirichlet

         ! ************** BOUNDARY CONDITIONS ***************************
         ! --------------------------------------------------------------
         ! B-field boundary conditions
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         dirichlet = 2; neumann = 5
         
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
           call setAllZero(phi_bcs,Nx,Ny,Nz,neumann)
         endif
       end subroutine

       end module