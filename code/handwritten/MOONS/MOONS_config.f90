       module MOONS_config_mod
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

       implicit none

       private
       public :: config
       interface config;         module procedure config_MOONS;         end interface

       contains

       subroutine config_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         write(*,*) ' ************** STARTED CONFIGURING MOONS ************** '
         call delete_file('','mesh_generation_error')
#ifdef fopenmp
         call omp_set_num_threads(12) ! Set number of openMP threads

#endif
         if (file_exists(str(M%C%DT%config),'primitives')) then
           ! Restart is default if files exist!
           call import_structured(M%C,str(M%C%DT%config))
           ! call import_restart(M%C,str(M%C%DT%restart),'config')
           ! Need to make sure init knows to restart:
           call set_restart(M%C%SP)
           ! call import_TMP(M%C%SP%VS)  ! start from last exported time step
           ! call import(M%C%SP%coupled) ! start from last exported time step
         else
           call init(M%C%SP)
         endif
         call display_compiler_info(str(M%C%DT%params),'compiler_info')
         call print_version()
         call export_version(str(M%C%DT%LDC))
         write(*,*) ' ************** FINISHED CONFIGURING MOONS ************** '
       end subroutine

       end module