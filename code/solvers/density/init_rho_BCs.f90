       module init_rho_BCs_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use boundary_conditions_mod
       use BC_funcs_mod
       use SF_mod
       implicit none
       private
       integer,dimension(3) :: periodic_dir = (/0,0,0/) ! 1 = true, else false
       integer,parameter :: preDefinedT_BCs = 1
       public :: init_rho_BCs

       contains

       subroutine init_rho_BCs(rho,m)
         implicit none
         type(SF),intent(inout) :: rho
         type(mesh),intent(in) :: m
         call init_BC_mesh(rho,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         call Neumann_BCs(rho,m)
         rho%all_Neumann = .false. ! Needs to be adjusted manually

         select case (preDefinedT_BCs)
         case (0) ! Default insulating
         case default; stop 'Error: preDefinedT_BCs must = 1 in init_rho_BCs in init_rho_BCs.f90.'
         end select
         call make_periodic(rho,m,periodic_dir)
       end subroutine

       end module