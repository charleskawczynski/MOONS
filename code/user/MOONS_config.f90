       module MOONS_config_mod
       use current_precision_mod
       use IO_tools_mod
       use IO_import_mod
       use IO_export_mod
       use inquire_funcs_mod
       use matrix_visualization_mod

       use version_mod
       use mesh_mod
       use mesh_domain_mod
       use mesh_generate_mod
       use generate_mesh_generic_mod
       use VF_mod
       use string_mod
       use path_mod
       use dir_tree_mod
       use var_set_mod
       use export_analytic_mod
       use mirror_props_mod
       use vorticity_streamfunction_mod
       use operator_interchangability_test_mod
       use Poisson_test_mod
       use Taylor_Green_Vortex_test_mod
       use temporal_convergence_test_mod
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

       use energy_mod
       use momentum_mod
       use induction_mod
       use MHDSolver_mod
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

         call init(DT,M%dir_target)  ! Initialize + make directory tree

         M%fresh_restart_file = .true.

         call init(M%RF)

         if (M%fresh_restart_file) then
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
           call import_TMP(M%SP%VS)                 ! start from last exported time step
           call import(M%SP%coupled)                ! start from last exported time step
         endif

         call export(M%SP,str(M%DT%params),'sim_params_raw_exported')
         call display(M%SP,str(M%DT%params),'sim_params_initial')
         call display_compiler_info(str(DT%params),'compiler_info')
         call export(M%SP%coupled)
         call export_TMP(M%SP%VS)

         call print_version()
         call export_version(str(M%DT%LDC))

         ! ************************************************************** Initialize mesh + domains
         if (M%SP%FCL%restart_meshes) then
           call import(M%mom%m,str(M%DT%mesh_restart),'m_mom')
           call import(M%ind%m,str(M%DT%mesh_restart),'m_ind')
           call import(M%ind%MD_sigma,str(M%DT%mesh_restart),'MD_sigma')
           call import(M%ind%MD_fluid,str(M%DT%mesh_restart),'MD_fluid')
         else
           call mesh_generate(M%mom%m,m_ind,M%ind%MD_sigma,SP)
           ! call init(m_ind_interior,M%ind%MD_sigma%m_R2)

           if (SP%VS%U%SS%initialize) then; call init_props(M%mom%m); call patch(M%mom%m); endif
           if (SP%VS%B%SS%initialize) then; call init_props(M%ind%m); call patch(M%ind%m); endif
           if (SP%VS%U%SS%initialize) then; call init_apply_BC_order(M%mom%m,SP%GP%apply_BC_order); endif
           if (SP%VS%B%SS%initialize) then; call init_apply_BC_order(M%ind%m,SP%GP%apply_BC_order); endif

           if (SP%VS%U%SS%initialize.and.SP%VS%B%SS%initialize) then
             call init(M%ind%MD_fluid,M%mom%m,M%ind%m) ! Domain,interior,exterior
             call init_props(M%ind%MD_fluid%m_R1); call patch(M%ind%MD_fluid%m_R1)
             call init_props(M%ind%MD_fluid%m_R2); call patch(M%ind%MD_fluid%m_R2)
             call init_props(M%ind%MD_sigma%m_R1); call patch(M%ind%MD_sigma%m_R1)
             call init_props(M%ind%MD_sigma%m_R2); call patch(M%ind%MD_sigma%m_R2)
           endif
         endif

         call export_mesh_aux(M%SP,M%DT,M%mom%m,M%ind%m)

         ! ******************** EXPORT GRIDS **************************** Export mesh (to plot)
         if (M%SP%EL%export_meshes) then
           call export_mesh(M%mom%m,str(M%DT%meshes),'m_mom',1)
           call export_mesh(M%ind%m,str(M%DT%meshes),'m_ind',1)
           call export_mesh(M%ind%MD_sigma%m_R2,str(M%DT%meshes),'mesh_MD_sigma',1)
           call export_mesh(M%ind%MD_fluid%m_R2,str(M%DT%meshes),'mesh_MD_fluid',1)
         if (M%SP%MP%mirror) then
           call mirror_mesh(M%m_temp,M%mom%m,M%SP%MP)
           call export_mesh(M%m_temp,str(M%DT%meshes),'mesh_mom_mirror',1)
           call mirror_mesh(M%m_temp,M%ind%m,M%SP%MP)
           call export_mesh(M%m_temp,str(M%DT%meshes),'mesh_ind_mirror',1)
           call mirror_mesh(M%m_temp,M%ind%MD_sigma%m_R1,M%SP%MP)
           call export_mesh(M%m_temp,str(M%DT%meshes),'mesh_MD_sigma_mirror',1)
           call mirror_mesh(M%m_temp,M%ind%MD_fluid%m_R1,M%SP%MP)
           call export_mesh(M%m_temp,str(M%DT%meshes),'mesh_MD_fluid_mirror',1)
           call delete(M%m_temp)
         endif
         endif
         call export(M%mom%m,str(M%DT%mesh_restart),'m_mom')
         call export(M%ind%m,str(M%DT%mesh_restart),'m_ind')
         call export(M%ind%MD_sigma,str(M%DT%mesh_restart),'MD_sigma')
         call export(M%ind%MD_fluid,str(M%DT%mesh_restart),'MD_fluid')

         if (file_exists('','mesh_generation_error')) stop 'Error: non-converged mesh, inspect mesh.'
         if (M%SP%FCL%stop_after_mesh_export) stop 'Exported meshes.'

         if (M%SP%VS%T%SS%initialize) call init(M%nrg,M%ind%m,M%SP,M%DT)
         if (M%SP%VS%U%SS%initialize) call init(M%mom,M%mom%m,M%SP,M%DT)
         if (M%SP%VS%B%SS%initialize) call init(M%ind,M%ind%m,M%SP,M%DT)
         if (M%SP%EL%export_ICs.and.M%SP%VS%T%SS%initialize) call export_tec(M%nrg,M%SP,M%DT)
         if (M%SP%EL%export_ICs.and.M%SP%VS%U%SS%initialize) call export_tec(M%mom,M%SP,M%DT)
         if (M%SP%EL%export_ICs.and.M%SP%VS%B%SS%initialize) call export_tec(M%ind,M%SP,M%DT)
         if (M%SP%VS%T%SS%initialize) call print(M%nrg%m)
         if (M%SP%VS%U%SS%initialize) call print(M%mom%m)
         if (M%SP%VS%B%SS%initialize) call print(M%ind%m)
         if (M%SP%FCL%stop_before_solve) stop 'Exported ICs. Change stop_before_solve to run sim.'
       end subroutine

       end module