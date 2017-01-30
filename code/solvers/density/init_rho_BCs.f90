       module init_rho_BCs_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use boundary_conditions_mod
       use BC_funcs_mod
       use SF_mod
       use sim_params_mod
       implicit none

       private
       public :: init_rho_BCs

       contains

       subroutine init_rho_BCs(rho,m,SP)
         implicit none
         type(SF),intent(inout) :: rho
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         integer,dimension(3) :: periodic_dir
         integer :: preset_ID
         call init_BC_mesh(rho,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         call Neumann_BCs(rho,m)
         rho%all_Neumann = .false. ! Needs to be adjusted manually

         preset_ID = SP%VS%rho%BC
         periodic_dir = SP%GP%periodic_dir
         ! preset_ID = 1 ! manual override

         select case (preset_ID)
         case (0) ! Default insulating
         case default; stop 'Error: bad preset_ID in init_TBCs.f90.'
         end select
         call make_periodic(rho,m,periodic_dir)
       end subroutine

       end module