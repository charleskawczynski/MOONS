       module MOONS_mod
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
       use dir_tree_mod
       use dir_tree_extend_mod
       use var_set_mod
       use var_set_extend_mod
       use export_analytic_mod
       use mirror_props_mod
       use vorticity_streamfunction_mod
       use operator_commute_test_mod
       use Poisson_test_mod
       use Taylor_Green_Vortex_test_mod
       use temporal_convergence_test_mod
       use export_mesh_aux_mod
       use restart_file_mod

       use iter_solver_params_mod
       use iter_solver_params_extend_mod
       use time_marching_params_mod
       use time_marching_params_extend_mod
       use sim_params_mod
       use sim_params_aux_mod
       use sim_params_extend_mod
       use export_raw_processed_symmetry_mod
       use export_raw_processed_mod
       use import_raw_mod
       use ops_mirror_field_mod

       ! use density_mod
       use energy_extend_mod
       use momentum_extend_mod
       use induction_extend_mod
       use MHDSolver_mod

       implicit none

       private
       public :: MOONS

       contains

       subroutine MOONS(dir_target)
         implicit none
         character(len=*),intent(in) :: dir_target
         ! ********************** BIG VARIABLES *************************
         type(momentum) :: mom
         type(induction) :: ind
         type(energy) :: nrg
         ! type(density) :: dens
         type(mesh) :: m_mom,m_ind,m_temp,m_sigma
         ! type(mesh) :: m_ind_interior
         ! ********************** MEDIUM VARIABLES **********************
         type(mesh_domain) :: MD_fluid,MD_sigma
         type(dir_tree) :: DT
         type(sim_params) :: SP
         type(restart_file) :: RF
         logical :: fresh_restart_file
         logical :: matrix_visualization
         ! ************************************************************** Parallel + directory + input parameters
         call delete_file('','mesh_generation_error')
#ifdef fopenmp
         call omp_set_num_threads(12) ! Set number of openMP threads

#endif

         call init(DT,dir_target)  ! Initialize + make directory tree

         matrix_visualization = .false.
         if (matrix_visualization) then
           call export_matrix_visualization(DT)
           stop 'exportedd matrix visualization. Done in MOONS.f90'
         endif

         fresh_restart_file = .true.
         call delete(RF)
         if (fresh_restart_file) call export(RF,'','restart_file') ! Use default, compiled SP + fields
         call import(RF,'','restart_file')

         if (RF%restart_input_file) call import(SP,'','sim_params_raw')
         if ((.not.RF%restart_input_file).and.(.not.RF%restart_fields)) call init(SP,DT)

         if (RF%restart_fields) then
           call set_restart(SP,RF%restart_fields) ! restart fields+mesh
           call import_TMP(SP%VS)                 ! start from last exported time step
           call import(SP%coupled)                ! start from last exported time step
         endif

         call export(SP,str(DT%params),'sim_params_raw_exported')
         call display(SP,str(DT%params),'sim_params_initial')
         call display_compiler_info(str(DT%params),'compiler_info')
         call export(SP%coupled)
         call export_TMP(SP%VS)

         call print_version()
         call export_version(str(DT%LDC))

         ! ************************************************************** Initialize mesh + domains
         if (SP%FCL%restart_meshes) then
           call import(m_mom,str(DT%mesh_restart),'m_mom')
           call import(m_ind,str(DT%mesh_restart),'m_ind')
           call import(MD_sigma,str(DT%mesh_restart),'MD_sigma')
           call import(MD_fluid,str(DT%mesh_restart),'MD_fluid')
         else
           ! call mesh_generate(m_mom,m_ind,MD_sigma,SP) ! original mesh
           call generate_mesh_generic(m_mom,SP%MP_mom,SP%DP,'momentum in MOONS.f90')
           call generate_mesh_generic(m_sigma,SP%MP_sigma,SP%DP,'sigma in MOONS.f90')
           call generate_mesh_generic(m_ind,SP%MP_ind,SP%DP,'induction in MOONS.f90')
           call export_mesh(m_mom,str(DT%meshes),'m_mom',1)
           call export_mesh(m_ind,str(DT%meshes),'m_ind',1)
           call init(MD_sigma,m_sigma,m_ind)
           call delete(m_sigma)
           ! call init(m_ind_interior,MD_sigma%m_R2)

           if (SP%VS%U%SS%initialize) then; call init_props(m_mom); call patch(m_mom); endif
           if (SP%VS%B%SS%initialize) then; call init_props(m_ind); call patch(m_ind); endif
           if (SP%VS%U%SS%initialize) then; call init_apply_BC_order(m_mom,SP%GP%apply_BC_order); endif
           if (SP%VS%B%SS%initialize) then; call init_apply_BC_order(m_ind,SP%GP%apply_BC_order); endif

           if (SP%VS%U%SS%initialize.and.SP%VS%B%SS%initialize) then
             call init(MD_fluid,m_mom,m_ind) ! Domain,interior,exterior
             call init_props(MD_fluid%m_R1); call patch(MD_fluid%m_R1)
             call init_props(MD_fluid%m_R2); call patch(MD_fluid%m_R2)
             call init_props(MD_sigma%m_R1); call patch(MD_sigma%m_R1)
             call init_props(MD_sigma%m_R2); call patch(MD_sigma%m_R2)
           endif
         endif

         call export_mesh_aux(SP,DT,m_mom,m_ind)

         ! ******************** EXPORT GRIDS **************************** Export mesh (to plot)
         if (SP%EL%export_meshes) then
           call export_mesh(m_mom,str(DT%meshes),'m_mom',1)
           call export_mesh(m_ind,str(DT%meshes),'m_ind',1)
           call export_mesh(MD_sigma%m_R2,str(DT%meshes),'mesh_MD_sigma',1)
           call export_mesh(MD_fluid%m_R2,str(DT%meshes),'mesh_MD_fluid',1)
         if (SP%MP%mirror) then
           call mirror_mesh(m_temp,m_mom,SP%MP)
           call export_mesh(m_temp,str(DT%meshes),'mesh_mom_mirror',1)
           call mirror_mesh(m_temp,m_ind,SP%MP)
           call export_mesh(m_temp,str(DT%meshes),'mesh_ind_mirror',1)
           call mirror_mesh(m_temp,MD_sigma%m_R1,SP%MP)
           call export_mesh(m_temp,str(DT%meshes),'mesh_MD_sigma_mirror',1)
           call mirror_mesh(m_temp,MD_fluid%m_R1,SP%MP)
           call export_mesh(m_temp,str(DT%meshes),'mesh_MD_fluid_mirror',1)
           call delete(m_temp)
         endif
         endif

         call export(m_mom,str(DT%mesh_restart),'m_mom')
         call export(m_ind,str(DT%mesh_restart),'m_ind')
         call export(MD_sigma,str(DT%mesh_restart),'MD_sigma')
         call export(MD_fluid,str(DT%mesh_restart),'MD_fluid')

         if (file_exists('','mesh_generation_error')) stop 'Error: non-converged mesh, inspect mesh.'
         if (SP%FCL%stop_after_mesh_export) then
           stop 'Exported meshes. Turn off stop_after_mesh_export in sim_params.f90 to run sim.'
         endif

         ! Initialize energy,momentum,induction
         if (SP%VS%T%SS%initialize) call init(nrg,m_ind,SP,DT,MD_fluid)
         if (SP%VS%U%SS%initialize) call init(mom,m_mom,SP,DT)
         if (SP%VS%B%SS%initialize) call init(ind,m_ind,SP,DT,MD_fluid,MD_sigma)

         ! Clean up constructor copies
         call delete(m_mom)
         call delete(m_ind)
         ! call delete(m_ind_interior)
         call delete(MD_fluid)
         call delete(MD_sigma)

         ! ********************* EXPORT RAW ICs *************************

         if (SP%EL%export_ICs.and.SP%VS%T%SS%initialize) call export_tec(nrg,SP,DT)
         if (SP%EL%export_ICs.and.SP%VS%U%SS%initialize) call export_tec(mom,SP,DT)
         if (SP%EL%export_ICs.and.SP%VS%B%SS%initialize) call export_tec(ind,SP,DT)

         if (SP%VS%T%SS%initialize) call print(nrg%m)
         if (SP%VS%U%SS%initialize) call print(mom%m)
         if (SP%VS%B%SS%initialize) call print(ind%m)

         ! ******************** PREP TIME START/STOP ********************

         if (SP%FCL%stop_before_solve) then
           stop 'Exported ICs. Turn off stop_before_solve in sim_params.f90 to run sim.'
         endif
         if (.not.SP%FCL%skip_solver_loop) call MHDSolver(nrg,mom,ind,DT,SP,SP%coupled)

         if (SP%FCL%Poisson_test) then
           call Poisson_test(mom%U,mom%p,mom%m,DT)
         endif
         if (SP%FCL%export_vorticity_streamfunction) then
           call export_vorticity_streamfunction(mom%U,mom%m,DT,SP)
         endif
         if (SP%FCL%compute_export_E_K_Budget) then
           call compute_export_E_K_Budget(mom,SP,ind%B,ind%B0,ind%J,ind%MD_fluid,DT)
         endif
         if (SP%FCL%compute_export_E_M_budget) then
           call compute_export_E_M_budget(ind,SP,mom%U,DT)
         endif
         if (SP%FCL%export_Shercliff_Hunt_analytic_sol) then
           call export_Shercliff_Hunt_analytic_sol(mom%m,mom%U%x,SP%DP%Ha,0.0_cp,-1.0_cp,1,DT)
         endif
         if (SP%FCL%export_numerical_flow_rate) then
           call export_numerical_flow_rate(mom%m,mom%U%x,SP%DP%Re,DT)
         endif
         if (SP%FCL%Taylor_Green_Vortex_test) then
           call Taylor_Green_Vortex_test(mom%U,mom%p,mom%m,DT,SP)
         endif
         if (SP%FCL%temporal_convergence_test) then
           call temporal_convergence_test(mom%U,mom%p,mom%m,DT,SP)
         endif
         if (SP%FCL%operator_commute_test) then
           call operator_commute_test(mom%U,mom%p,mom%m,DT)
         endif

         write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
         write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
         write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
         ! ******************* DELETE ALLOCATED DERIVED TYPES ***********
         call delete(nrg)
         call delete(mom)
         call delete(ind)
         call delete(DT)
         call delete(SP)

         write(*,*) ' ******************** LAST LINE EXECUTED ********************'
       end subroutine

       end module