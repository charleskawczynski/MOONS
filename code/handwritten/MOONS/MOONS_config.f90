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
       use restart_file_mod

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
       interface config;   module procedure config_MOONS;   end interface

       contains

       subroutine config_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         call delete_file('','mesh_generation_error')
#ifdef fopenmp
         call omp_set_num_threads(12) ! Set number of openMP threads

#endif

         call init(M%DT,str(M%dir_target))  ! Initialize + make directory tree

         M%SP%FCL%matrix_visualization = .false.
         if (M%SP%FCL%matrix_visualization) then
           call export_matrix_visualization(M%DT)
         endif

         call delete(M%RF)

         M%SP%FCL%fresh_restart_file = .true.
         if (M%SP%FCL%fresh_restart_file) then
           call export(M%RF,'','restart_file') ! Use default, compiled SP + fields
         endif

         call import(M%RF,'','restart_file')

         if (M%RF%restart_input_file) then
           call import(M%SP,'','sim_params_raw')
         endif

         if ((.not.M%RF%restart_input_file).and.(.not.M%RF%restart_fields)) then
           call init(M%SP,M%DT)
         endif

         if (M%RF%restart_fields) then
           call set_restart(M%SP,M%RF%restart_fields) ! restart fields+mesh
           call import_TMP(M%SP%VS)                   ! start from last exported time step
           call import(M%SP%coupled)                  ! start from last exported time step
         endif

         call export(M%SP,str(M%DT%params),'sim_params_raw_exported')
         call display(M%SP,str(M%DT%params),'sim_params_initial')
         call display_compiler_info(str(M%DT%params),'compiler_info')

         call export(M%SP%coupled)
         call export_TMP(M%SP%VS)

         call print_version()
         call export_version(str(M%DT%LDC))
       end subroutine

       end module