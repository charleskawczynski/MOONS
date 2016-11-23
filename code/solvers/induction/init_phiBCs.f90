       module init_phiBCs_mod
       use current_precision_mod
       use BC_funcs_mod
       use grid_mod
       use mesh_mod
       use boundary_conditions_mod
       use SF_mod
       implicit none

       private
       public :: init_phiBCs

       integer,dimension(3) :: periodic_dir = (/0,0,0/) ! 1 = true, else false
       ! Default = dirichlet on all sides
       integer :: preDefinedphi_BCs = 0 ! see cases in init_phiBCs

       contains

       subroutine init_phiBCs(phi,m)
         implicit none
         type(SF),intent(inout) :: phi
         type(mesh),intent(in) :: m
         call init_BC_mesh(phi,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         call Dirichlet_BCs(phi,m)

         phi%all_Neumann = .false. ! Needs to be adjusted manually

         select case (preDefinedphi_BCs)
         case (0)
         case (1); call periodic_duct_flow(phi)
         case (2); call symmetric_zmax(phi)
         case default; stop 'Error: preDefinedphi_BCs must = 1:5 in init_phiBCs in init_phiBCs.f90.'
         end select
         call make_periodic(phi,m,periodic_dir)
         call init_BC_props(phi)
       end subroutine

       subroutine periodic_duct_flow(phi)
         implicit none
         type(SF),intent(inout) :: phi
         call init_periodic(phi%BF(1)%BCs,1)
         call init_periodic(phi%BF(1)%BCs,2)
       end subroutine

       subroutine symmetric_zmax(phi)
         implicit none
         type(SF),intent(inout) :: phi
         call init_Symmetric(phi%BF(1)%BCs,6)
       end subroutine

       end module