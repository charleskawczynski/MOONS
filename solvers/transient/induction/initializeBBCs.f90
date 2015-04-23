       module initializeBBCs_mod
       use grid_mod
       use BCs_mod
       implicit none
       ! From applyBCs.f90:
       ! bctype = 1 ! Dirichlet - direct - wall coincident
       ! bctype = 2 ! Dirichlet - interpolated - wall incoincident
       ! bctype = 3 ! Neumann - direct - wall coincident ~O(dh^2)
       ! bctype = 4 ! Neumann - direct - wall coincident ~O(dh)
       ! bctype = 5 ! Neumann - interpolated - wall incoincident ~O(dh)

       private

       integer,parameter :: preDefinedB_BCs = 0
       !                                      0 : User-defined case (no override)
       !                                      1 : Psuedo-vaccuum BCs (dBn/dn = 0, B_tangential = 0)
       !                                      2 : B = 0

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

       subroutine initBBCs(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,g,cleanB)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: phi_bcs
         type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
         logical,intent(in) :: cleanB
         if (preDefinedB_BCs.ne.0) then
           call initPreDefinedBCs(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,g,cleanB)
         else
           call initUserBBCs(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,g,cleanB)
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

       subroutine initPreDefinedBCs(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,g,cleanB)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: phi_bcs
         type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
         logical,intent(in) :: cleanB
         integer :: Nx,Ny,Nz,neumann

         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         neumann = 5
         
         select case (preDefinedB_BCs)
         case (1) ! Pseudo Vaccuum
           call initPseudoVacuumBCs(Bx_bcs,By_bcs,Bz_bcs,g)
         case (2) ! B = 0
           call initBeqZeroBCs(Bx_bcs,By_bcs,Bz_bcs,g)
         case default
           write(*,*) 'Incorrect preDefinedB_BCs in initPreDefinedBfield';stop
         end select

         if (cleanB) call setAllZero(phi_bcs,Nx,Ny,Nz,neumann)
       end subroutine

       subroutine initPseudoVacuumBCs(Bx_bcs,By_bcs,Bz_bcs,g)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
         integer :: Nx,Ny,Nz,neumann,dirichlet
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
       end subroutine

       subroutine initBeqZeroBCs(Bx_bcs,By_bcs,Bz_bcs,g)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
         integer :: Nx,Ny,Nz,dirichlet
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         dirichlet = 2
         call setAllZero(Bx_bcs,Nx,Ny,Nz,dirichlet)
         call setAllZero(By_bcs,Nx,Ny,Nz,dirichlet)
         call setAllZero(Bz_bcs,Nx,Ny,Nz,dirichlet)
       end subroutine

       subroutine initUserBBCs(Bx_bcs,By_bcs,Bz_bcs,phi_bcs,g,cleanB)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: phi_bcs
         type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
         logical,intent(in) :: cleanB
         integer :: Nx,Ny,Nz,neumann
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         neumann = 5
         call initPseudoVacuumBCs(Bx_bcs,By_bcs,Bz_bcs,g)
         
         call setXminType(By_bcs,neumann)
         call setXmaxType(By_bcs,neumann)
         call setXminType(Bz_bcs,neumann)
         call setXmaxType(Bz_bcs,neumann)

       end subroutine

       end module