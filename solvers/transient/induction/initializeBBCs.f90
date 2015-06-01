       module initializeBBCs_mod
       use grid_mod
       use BCs_mod
       use vectorBCs_mod
       use vectorField_mod
       implicit none
       ! From applyBCs.f90:
       ! bctype = 1 ! Dirichlet - direct - wall coincident
       ! bctype = 2 ! Dirichlet - interpolated - wall incoincident
       ! bctype = 3 ! Neumann - direct - wall coincident ~O(dh^2)
       ! bctype = 4 ! Neumann - direct - wall coincident ~O(dh)
       ! bctype = 5 ! Neumann - interpolated - wall incoincident ~O(dh)

       private

       integer,parameter :: preDefinedB_BCs = 1
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

       subroutine initBBCs(B_bcs,phi_bcs,B,g,cleanB)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         type(BCs),intent(inout) :: phi_bcs
         type(vectorField),intent(in) :: B
         type(grid),intent(in) :: g
         logical,intent(in) :: cleanB

         call init(B_bcs%x,B%sx(1),B%sx(2),B%sx(3))
         call init(B_bcs%y,B%sy(1),B%sy(2),B%sy(3))
         call init(B_bcs%z,B%sz(1),B%sz(2),B%sz(3))

         if (preDefinedB_BCs.ne.0) then
           call initPreDefinedBCs(B_bcs,phi_bcs,g,cleanB)
         else
           call initUserBBCs(B_bcs,phi_bcs,g,cleanB)
         endif
         call setGrid(B_bcs,g)
         if (cleanB) call setGrid(phi_bcs,g)
         call checkVectorBCs(B_bcs)
         if (cleanB) call checkBCs(phi_bcs)
       end subroutine

       subroutine initPreDefinedBCs(B_bcs,phi_bcs,g,cleanB)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         type(BCs),intent(inout) :: phi_bcs
         type(grid),intent(in) :: g
         logical,intent(in) :: cleanB
         integer :: neumann
         
         select case (preDefinedB_BCs)
         case (1); call initPseudoVacuumBCs(B_bcs,g) ! Pseudo Vaccuum
         case (2); call initBeqZeroBCs(B_bcs,g)      ! B = 0
         case default
           write(*,*) 'Incorrect preDefinedB_BCs in initPreDefinedBfield';stop
         end select

         neumann = 5
         call init(phi_bcs,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
         if (cleanB) call setAllZero(phi_bcs,neumann)
       end subroutine

       subroutine initPseudoVacuumBCs(B_bcs,g)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         type(grid),intent(in) :: g
         integer :: dirichlet,dirichlet_c,dirichlet_i
         integer :: neumann,neumann_c,neumann_i
         dirichlet_c = 1; dirichlet_i = 2
         neumann_c = 3; neumann_i = 5;
         neumann = neumann_i
         dirichlet = dirichlet_i

         call setAllZero(B_bcs%x,dirichlet)
         call setXminType(B_bcs%x,neumann)
         call setXmaxType(B_bcs%x,neumann)

         call setAllZero(B_bcs%y,dirichlet)
         call setYminType(B_bcs%y,neumann)
         call setYmaxType(B_bcs%y,neumann)

         call setAllZero(B_bcs%z,dirichlet)
         call setZminType(B_bcs%z,neumann)
         call setZmaxType(B_bcs%z,neumann)
       end subroutine

       subroutine initBeqZeroBCs(B_bcs,g)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         type(grid),intent(in) :: g
         integer :: dirichlet,dirichlet_c,dirichlet_i
         integer :: neumann,neumann_c,neumann_i
         dirichlet_c = 1; dirichlet_i = 2
         neumann_c = 3; neumann_i = 5;
         neumann = neumann_i
         dirichlet = dirichlet_i

         call setAllZero(B_bcs%x,dirichlet)
         call setAllZero(B_bcs%y,dirichlet)
         call setAllZero(B_bcs%z,dirichlet)
       end subroutine

       subroutine initUserBBCs(B_bcs,phi_bcs,g,cleanB)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: phi_bcs
         type(vectorBCs),intent(inout) :: B_bcs
         logical,intent(in) :: cleanB
         integer :: neumann,periodic_i
         neumann = 5
         periodic_i = 7 ! Wall incoincident
         call initPseudoVacuumBCs(B_bcs,g)
         
         call setXminType(B_bcs%x,periodic_i)
         call setXminType(B_bcs%y,periodic_i)
         call setXminType(B_bcs%z,periodic_i)

         call setXmaxType(B_bcs%x,periodic_i)
         call setXmaxType(B_bcs%y,periodic_i)
         call setXmaxType(B_bcs%z,periodic_i)
       end subroutine

       end module