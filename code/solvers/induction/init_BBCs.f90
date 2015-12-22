       module init_BBCs_mod
       use grid_mod
       use mesh_mod
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

       integer,dimension(3),parameter :: periodic_dir = (/0,0,0/) ! 1 = true, else false
       integer,parameter :: preDefinedB_BCs = 2
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

       subroutine initBBCs(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         type(vectorBCs) :: B_bcs
         integer :: i

         do i=1,m%s
           call init(B_bcs%x,m%g(i),B%x%RF(i)%s)
           call init(B_bcs%y,m%g(i),B%y%RF(i)%s)
           call init(B_bcs%z,m%g(i),B%z%RF(i)%s)

           if (preDefinedB_BCs.ne.0) then
             call initPreDefinedBCs(B_bcs)
           else
             call initUserBBCs(B_bcs)
           endif

           call init(B%x%RF(i)%b,B_bcs%x) ! Copy vector BCs to field
           call init(B%y%RF(i)%b,B_bcs%y) ! Copy vector BCs to field
           call init(B%z%RF(i)%b,B_bcs%z) ! Copy vector BCs to field
         enddo
         ! call init_Antisymmetry(B%x%RF(1)%b,6)
         ! call init_Antisymmetry(B%y%RF(1)%b,6)
         ! call init_Neumann(B%z%RF(1)%b,6)
         call delete(B_bcs)
       end subroutine

       subroutine initPreDefinedBCs(B_bcs)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         integer :: i
         
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
       end subroutine

       subroutine initPseudoVacuumBCs(B_bcs)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         call init_Dirichlet(B_bcs%x)
         call init_Neumann(B_bcs%x,1)
         call init_Neumann(B_bcs%x,2)

         call init_Dirichlet(B_bcs%y)
         call init_Neumann(B_bcs%y,3)
         call init_Neumann(B_bcs%y,4)

         call init_Dirichlet(B_bcs%z)
         call init_Neumann(B_bcs%z,5)
         call init_Neumann(B_bcs%z,6)

         call init(B_bcs%x,0.0_cp)
         call init(B_bcs%y,0.0_cp)
         call init(B_bcs%z,0.0_cp)
       end subroutine

       subroutine initBandaru(B_bcs)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         call init_Neumann(B_bcs%x,5)
         call init_Neumann(B_bcs%x,6)

         call init(B_bcs%x,0.0_cp,5)
         call init(B_bcs%x,0.0_cp,6)
       end subroutine

       subroutine initBeqZeroBCs(B_bcs)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         call init_Dirichlet(B_bcs%x)
         call init_Dirichlet(B_bcs%y)
         call init_Dirichlet(B_bcs%z)

         call init(B_bcs%x,0.0_cp)
         call init(B_bcs%y,0.0_cp)
         call init(B_bcs%z,0.0_cp)
       end subroutine

       subroutine initUserBBCs(B_bcs)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         call initPseudoVacuumBCs(B_bcs)

         call init_periodic(B_bcs%x,1)
         call init_periodic(B_bcs%y,1)
         call init_periodic(B_bcs%z,1)

         call init_periodic(B_bcs%x,2)
         call init_periodic(B_bcs%y,2)
         call init_periodic(B_bcs%z,2)
       end subroutine

       subroutine makePeriodic(B_bcs,dir)
         implicit none
         type(vectorBCs),intent(inout) :: B_bcs
         integer,intent(in) :: dir
         integer :: periodic_i
         periodic_i = 7 ! Wall incoincident
         select case (dir)
         case (1);call init_periodic(B_bcs%x,1)
                  call init_periodic(B_bcs%y,1)
                  call init_periodic(B_bcs%z,1)
                  call init_periodic(B_bcs%x,2)
                  call init_periodic(B_bcs%y,2)
                  call init_periodic(B_bcs%z,2)
         case (2);call init_periodic(B_bcs%x,3)
                  call init_periodic(B_bcs%y,3)
                  call init_periodic(B_bcs%z,3)
                  call init_periodic(B_bcs%x,4)
                  call init_periodic(B_bcs%y,4)
                  call init_periodic(B_bcs%z,4)
         case (3);call init_periodic(B_bcs%x,5)
                  call init_periodic(B_bcs%y,5)
                  call init_periodic(B_bcs%z,5)
                  call init_periodic(B_bcs%x,6)
                  call init_periodic(B_bcs%y,6)
                  call init_periodic(B_bcs%z,6)
         case default
         stop 'Error: dir must = 1,2,3 in makePeriodic in initializeBBCs.f90'
         end select
       end subroutine

       end module