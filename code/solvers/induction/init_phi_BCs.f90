       module init_phi_BCs_mod
       use current_precision_mod
       use BC_funcs_mod
       use grid_mod
       use mesh_mod
       use boundary_conditions_mod
       use SF_mod
       use benchmark_case_mod
       implicit none

       private
       public :: init_phi_BCs

       contains

       subroutine init_phi_BCs(phi,m,BMC)
         implicit none
         type(SF),intent(inout) :: phi
         type(mesh),intent(in) :: m
         type(benchmark_case),intent(in) :: BMC
         integer,dimension(3) :: periodic_dir
         integer :: preset_ID
         call init_BC_mesh(phi,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         call Dirichlet_BCs(phi,m)

         preset_ID = BMC%VS%phi%BC
         periodic_dir = BMC%periodic_dir

         phi%all_Neumann = .false. ! Needs to be adjusted manually

         select case (preset_ID)
         case (0)
         case (1); call periodic_duct_flow(phi)
         case default; stop 'Error: bad preset_ID in init_phi_BCs.f90.'
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

       end module