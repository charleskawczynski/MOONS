       module MOONS_IO_mod
       use current_precision_mod
       use IO_tools_mod
       use IO_import_mod
       use IO_export_mod
       use inquire_funcs_mod
       use matrix_visualization_mod

       use version_mod
       use mesh_extend_mod
       use mesh_domain_extend_mod
       use generate_mesh_generic_mod
       use VF_extend_mod
       use string_mod
       use path_extend_mod
       use dir_tree_extend_mod
       use var_set_extend_mod
       use export_analytic_mod
       use mirror_props_mod
       use export_mesh_aux_mod

       use iter_solver_params_mod
       use time_marching_params_mod
       use sim_params_mod
       use sim_params_aux_mod
       use sim_params_extend_mod
       use export_raw_processed_symmetry_mod
       use export_raw_processed_mod
       use import_raw_mod
       use ops_mirror_field_mod

       use stop_clock_extend_mod
       use kill_switch_extend_mod
       use export_frequency_extend_mod
       use export_now_extend_mod
       use export_safe_extend_mod
       use refine_mesh_extend_mod

       use energy_extend_mod
       use momentum_extend_mod
       use induction_extend_mod
       use MOONS_mod
       ! use current_precision_mod
       ! use string_mod
       ! use path_extend_mod
       ! use dir_tree_extend_mod
       ! use MOONS_mod
       ! use energy_extend_mod
       ! use momentum_extend_mod
       ! use induction_extend_mod

       implicit none

       private
       public :: export_MOONS
       public :: import_MOONS

       interface export_MOONS; module procedure export_MOONS_M;     end interface
       interface import_MOONS; module procedure import_MOONS_M;     end interface

       contains

       subroutine export_MOONS_M(M)
         implicit none
         type(MOONS),intent(in) :: M
         logical,dimension(5) :: L
         L(1) = M%C%SP%EF%final_solution%export_now
         L(2) = M%C%EN%all%this
         L(3) = M%C%EN%B%this
         L(4) = M%C%EN%U%this
         L(5) = M%C%EN%T%this
         if (any(L)) then
           write(*,*) ' ************* STARTED EXPORTING MOONS FOR RESTART *************'
           ! call export_structured(M%C,str(M%C%DT%restart))
           call export_structured(M,str(M%C%DT%restart))
           ! if (M%C%SP%VS%U%SS%initialize) call export_structured(M%GE%mom,str(M%C%DT%restart))
           ! if (M%C%SP%VS%B%SS%initialize) call export_structured(M%GE%ind,str(M%C%DT%restart))
           ! if (M%C%SP%VS%T%SS%initialize) call export_structured(M%GE%nrg,str(M%C%DT%restart))
           write(*,*) ' ************* FINISHED EXPORTING MOONS FOR RESTART ************'
         endif
       end subroutine

       subroutine import_MOONS_M(M)
         implicit none
         type(MOONS),intent(inout) :: M
         write(*,*) ' ************* STARTED IMPORTING MOONS FOR RESTART *************'
         call import_structured(M,str(M%C%DT%restart))
         ! call import_structured(M%C,str(M%C%DT%restart))
         ! if (M%C%SP%VS%U%SS%initialize) call import_structured(M%GE%mom,str(M%C%DT%restart))
         ! if (M%C%SP%VS%B%SS%initialize) call import_structured(M%GE%ind,str(M%C%DT%restart))
         ! if (M%C%SP%VS%T%SS%initialize) call import_structured(M%GE%nrg,str(M%C%DT%restart))
         write(*,*) ' ************* FINISHED IMPORTING MOONS FOR RESTART ************'
       end subroutine

       end module