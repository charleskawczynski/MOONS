       module MOONS_main_mod
       use current_precision_mod
       use IO_tools_mod
       use IO_import_mod
       use IO_export_mod
       use inquire_funcs_mod
       use matrix_visualization_mod

       use version_mod
       use mesh_extend_mod
       use var_set_extend_mod
       use mesh_domain_extend_mod
       use generate_mesh_generic_mod
       use VF_extend_mod
       use string_mod
       use path_extend_mod
       use dir_tree_mod
       use var_set_mod
       use export_analytic_mod
       use mirror_props_mod
       use stop_clock_extend_mod
       use vorticity_streamfunction_mod
       use operator_commute_test_mod
       use Poisson_test_mod
       use Taylor_Green_Vortex_test_mod
       use temporal_convergence_test_mod
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

       use energy_extend_mod
       use momentum_extend_mod
       use induction_extend_mod
       use MOONS_solver_mod
       use MOONS_mod
       use MOONS_config_mod
       use MOONS_init_mod

       implicit none

       private
       public :: main

       interface main;         module procedure main_MOONS;         end interface
       interface post_process; module procedure post_process_MOONS; end interface
       interface final_export; module procedure final_export_MOONS; end interface

       contains

       subroutine main_MOONS(dir_target)
         implicit none
         type(MOONS) :: M
         character(len=*),intent(in) :: dir_target
         write(*,*) ' ******************** COMPUTATIONS STARTED ********************'
         write(*,*) ' ******************** COMPUTATIONS STARTED ********************'
         write(*,*) ' ******************** COMPUTATIONS STARTED ********************'
         call init(M%C%dir_target,dir_target)
         call init(M%C%DT,str(M%C%dir_target))  ! Initialize + make directory tree
         call config(M) ! The flow control should be uniquely defined after this line.
         call make_IO_dir(M%C,str(M%C%DT%restart))
         call export_structured(M%C)
         call export(M%C,str(M%C%DT%config),'config_original_DO_NOT_EDIT')
         call init(M)
         if (M%C%SP%VS%T%SS%initialize) call set_necessary_for_restart(M%GE%nrg)
         if (M%C%SP%VS%U%SS%initialize) call set_necessary_for_restart(M%GE%mom)
         if (M%C%SP%VS%B%SS%initialize) call set_necessary_for_restart(M%GE%ind)
         if (M%C%SP%FCL%restart_all) then
           call set_IO_dir(M%GE,str(M%C%DT%restart))
           call import_structured(M%GE,str(M%C%DT%restart))
         endif
         call make_IO_dir(M%GE,str(M%C%DT%restart)) ! repeat after init so that allocatables populate directory
         call export_structured(M%C)
         if (.not.M%C%SP%FCL%restart_all) then
           call export_structured(M%GE)
         endif
         if (.not.M%C%SP%FCL%skip_solver_loop) then
           call solve(M)
         endif
         call final_export(M)
         call post_process(M)
         call delete(M)
         write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
         write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
         write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
         write(*,*) ' ******************** LAST LINE EXECUTED ***********************'
       end subroutine

       subroutine final_export_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         call print(M%C%sc,M%C%SP%coupled)

         call export_structured(M%C)
         call export_structured(M%GE)

         if (M%C%SP%FCL%export_final_tec) then
           if (M%C%SP%VS%T%SS%initialize) call export_tec(M%GE%nrg,M%C%SP,M%C%DT)
           if (M%C%SP%VS%U%SS%initialize) call export_tec(M%GE%mom,M%C%SP,M%C%DT)
           if (M%C%SP%VS%B%SS%initialize) call export_tec(M%GE%ind,M%C%SP,M%C%DT)
         endif
       end subroutine

       subroutine post_process_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         write(*,*) ' *********************** POST PROCESSING ***********************'
         write(*,*) ' *********************** POST PROCESSING ***********************'
         write(*,*) ' *********************** POST PROCESSING ***********************'
         if (M%C%SP%FCL%matrix_visualization) then
           call export_matrix_visualization(M%C%DT)
         endif
         if (M%C%SP%FCL%Poisson_test) then
           call Poisson_test(M%GE%mom%U,M%GE%mom%p,M%GE%mom%m,M%C%DT)
         endif
         if (M%C%SP%FCL%export_vorticity_streamfunction) then
           call export_vorticity_streamfunction(M%GE%mom%U,M%GE%mom%m,M%C%DT,M%C%SP)
         endif
         if (M%C%SP%FCL%compute_export_E_K_Budget) then
           call compute_export_E_K_Budget(M%GE%mom,M%C%SP,M%GE%ind%B,M%GE%ind%B0,M%GE%ind%J,M%GE%ind%MD_fluid,M%C%DT)
         endif
         if (M%C%SP%FCL%compute_export_E_M_budget) then
           call compute_export_E_M_budget(M%GE%ind,M%C%SP,M%GE%mom%U,M%C%DT)
         endif
         if (M%C%SP%FCL%export_Shercliff_Hunt_analytic_sol) then
           call export_Shercliff_Hunt_analytic_sol(M%GE%mom%m,M%GE%mom%U%x,M%C%SP%DP%Ha,0.0_cp,-1.0_cp,1,M%C%DT)
         endif
         if (M%C%SP%FCL%export_numerical_flow_rate) then
           call export_numerical_flow_rate(M%GE%mom%m,M%GE%mom%U%x,M%C%SP%DP%Re,M%C%DT)
         endif
         if (M%C%SP%FCL%Taylor_Green_Vortex_test) then
           call Taylor_Green_Vortex_test(M%GE%mom%U,M%GE%mom%p,M%GE%mom%m,M%C%DT,M%C%SP)
         endif
         if (M%C%SP%FCL%temporal_convergence_test) then
           call temporal_convergence_test(M%GE%mom%U,M%GE%mom%p,M%GE%mom%m,M%C%DT,M%C%SP)
         endif
         if (M%C%SP%FCL%operator_commute_test) then
           call operator_commute_test(M%GE%mom%U,M%GE%mom%p,M%GE%mom%m,M%C%DT)
         endif
       end subroutine

       end module