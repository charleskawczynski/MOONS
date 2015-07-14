       module initializeBBCs_mod
       use grid_mod
       use BCs_mod
       use vectorBCs_mod
       use SF_mod
       use VF_mod
       implicit none
       ! Make some of these integers, neumann_i, neumann_c, etc. 
       ! global within this file.
       ! 
       ! 
       ! From applyBCs.f90:
       ! bctype = 1 ! Dirichlet - direct - wall coincident
       ! bctype = 2 ! Dirichlet - interpolated - wall incoincident
       ! bctype = 3 ! Neumann - direct - wall coincident ~O(dh^2)
       ! bctype = 4 ! Neumann - direct - wall coincident ~O(dh)
       ! bctype = 5 ! Neumann - interpolated - wall incoincident ~O(dh)

       private

       integer,dimension(3),parameter :: periodic_dir = (/1,1,0/) ! 1 = true, else false
       integer,parameter :: preDefinedB_BCs = 3
       !                                      0 : User-defined case (no override)
       !                                      1 : Psuedo-vaccuum BCs (dBn/dn = 0, B_tangential = 0)
       !                                      2 : B = 0
       !                                      3 : Bandaru
       !                                      4 : B = 0 AND dBn/dn = 0


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
         type(VF),intent(in) :: B
         type(grid),intent(in) :: g
         logical,intent(in) :: cleanB

         call init(B_bcs%x,B%sx(1),B%sx(2),B%sx(3))
         call init(B_bcs%y,B%sy(1),B%sy(2),B%sy(3))
         call init(B_bcs%z,B%sz(1),B%sz(2),B%sz(3))

         if (preDefinedB_BCs.ne.0) then
           call initPreDefinedBCs(B_bcs,phi_bcs,g,cleanB)
         else
           call initUserBBCs(B_bcs)
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
         integer :: neumann,i
         
         call initPseudoVacuumBCs(B_bcs) ! Pseudo Vaccuum
         select case (preDefinedB_BCs)
         case (1) ! Default
         case (2); call initBeqZeroBCs(B_bcs)      ! B = 0
         case (3); call initBeqZeroBCs(B_bcs)
                   call initBandaru(B_bcs)
         case (4); call initBeqZeroBCs(B_bcs)
                   call initBandaru(B_bcs)
         case default
           write(*,*) 'Incorrect preDefinedB_BCs in initPreDefinedBfield';stop
         end select

         do i=1,3
           select case (periodic_dir(i))
           case (0)
           case (1); call makePeriodic(B_bcs,i)
           case default
           stop 'Error: periodic_dir must = 1,0 in initPreDefinedBCs in initializeBBCs.f90'
           end select
         enddo

         neumann = 5
         call init(phi_bcs,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
         if (cleanB) call setAllZero(phi_bcs,neumann)
       end subroutine

       subroutine initPseudoVacuumBCs(B_bcs)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         integer :: dirichlet,dirichlet_c,dirichlet_i
         integer :: neumann,neumann_c,neumann_i
         dirichlet_c = 1; dirichlet_i = 2
         neumann_c = 3; neumann_i = 5
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

       subroutine initBandaru(B_bcs)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         integer :: neumann,neumann_c,neumann_i
         neumann_c = 3; neumann_i = 5
         neumann = neumann_i
         call setZminType(B_bcs%x,neumann)
         call setZmaxType(B_bcs%x,neumann)
       end subroutine

       subroutine initBeqZeroBCs(B_bcs)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
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

       subroutine initUserBBCs(B_bcs)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         integer :: neumann,periodic_i
         neumann = 5
         periodic_i = 7 ! Wall incoincident
         call initPseudoVacuumBCs(B_bcs)
         
         call setXminType(B_bcs%x,periodic_i)
         call setXminType(B_bcs%y,periodic_i)
         call setXminType(B_bcs%z,periodic_i)

         call setXmaxType(B_bcs%x,periodic_i)
         call setXmaxType(B_bcs%y,periodic_i)
         call setXmaxType(B_bcs%z,periodic_i)
       end subroutine

       subroutine makePeriodic(B_bcs,dir)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         integer,intent(in) :: dir
         integer :: periodic_i
         periodic_i = 7 ! Wall incoincident
         select case (dir)
         case (1);call setXminType(B_bcs%x,periodic_i)
                  call setXminType(B_bcs%y,periodic_i)
                  call setXminType(B_bcs%z,periodic_i)
                  call setXmaxType(B_bcs%x,periodic_i)
                  call setXmaxType(B_bcs%y,periodic_i)
                  call setXmaxType(B_bcs%z,periodic_i)
         case (2);call setYminType(B_bcs%x,periodic_i)
                  call setYminType(B_bcs%y,periodic_i)
                  call setYminType(B_bcs%z,periodic_i)
                  call setYmaxType(B_bcs%x,periodic_i)
                  call setYmaxType(B_bcs%y,periodic_i)
                  call setYmaxType(B_bcs%z,periodic_i)
         case (3);call setZminType(B_bcs%x,periodic_i)
                  call setZminType(B_bcs%y,periodic_i)
                  call setZminType(B_bcs%z,periodic_i)
                  call setZmaxType(B_bcs%x,periodic_i)
                  call setZmaxType(B_bcs%y,periodic_i)
                  call setZmaxType(B_bcs%z,periodic_i)
         case default
         stop 'Error: dir must = 1,2,3 in makePeriodic in initializeBBCs.f90'
         end select
       end subroutine

       end module