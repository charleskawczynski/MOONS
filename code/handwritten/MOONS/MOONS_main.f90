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

       use energy_extend_mod
       use momentum_extend_mod
       use induction_extend_mod
       use MOONS_solver_mod
       use MOONS_mod
       use MOONS_config_mod

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
         call init(M%dir_target,dir_target)
         call config(M)
         call make_restart_dir(M,str(M%DT%restart))
         if (.not.M%SP%FCL%skip_solver_loop) then
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
         call print(M%SC,M%SP%coupled)
         call export(M%SC,M%SP%coupled)

         call export_ISP(M%SP%VS)
         call export_TMP(M%SP%VS)
         call export(M%SP%coupled)

         if (M%SP%FCL%export_final_tec) then
           if (M%SP%VS%T%SS%initialize) call export_tec(M%nrg,M%SP,M%DT)
           if (M%SP%VS%U%SS%initialize) call export_tec(M%mom,M%SP,M%DT)
           if (M%SP%VS%B%SS%initialize) call export_tec(M%ind,M%SP,M%DT)
         endif

         if (M%SP%FCL%export_final_restart) then
           if (M%SP%VS%T%SS%initialize) call export(M%nrg,M%SP,M%DT)
           if (M%SP%VS%U%SS%initialize) call export(M%mom,M%SP,M%DT)
           if (M%SP%VS%B%SS%initialize) call export(M%ind,M%SP,M%DT)
           ! call export(M,str(DT%restart),'MOONS')
         endif
       end subroutine

       subroutine post_process_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         write(*,*) ' *********************** POST PROCESSING ***********************'
         write(*,*) ' *********************** POST PROCESSING ***********************'
         write(*,*) ' *********************** POST PROCESSING ***********************'
         if (M%SP%FCL%Poisson_test) then
           call Poisson_test(M%mom%U,M%mom%p,M%mom%m,M%DT)
         endif
         if (M%SP%FCL%export_vorticity_streamfunction) then
           call export_vorticity_streamfunction(M%mom%U,M%mom%m,M%DT,M%SP)
         endif
         if (M%SP%FCL%compute_export_E_K_Budget) then
           call compute_export_E_K_Budget(M%mom,M%SP,M%ind%B,M%ind%B0,M%ind%J,M%ind%MD_fluid,M%DT)
         endif
         if (M%SP%FCL%compute_export_E_M_budget) then
           call compute_export_E_M_budget(M%ind,M%SP,M%mom%U,M%DT)
         endif
         if (M%SP%FCL%export_Shercliff_Hunt_analytic_sol) then
           call export_Shercliff_Hunt_analytic_sol(M%mom%m,M%mom%U%x,M%SP%DP%Ha,0.0_cp,-1.0_cp,1,M%DT)
         endif
         if (M%SP%FCL%export_numerical_flow_rate) then
           call export_numerical_flow_rate(M%mom%m,M%mom%U%x,M%SP%DP%Re,M%DT)
         endif
         if (M%SP%FCL%Taylor_Green_Vortex_test) then
           call Taylor_Green_Vortex_test(M%mom%U,M%mom%p,M%mom%m,M%DT,M%SP)
         endif
         if (M%SP%FCL%temporal_convergence_test) then
           call temporal_convergence_test(M%mom%U,M%mom%p,M%mom%m,M%DT,M%SP)
         endif
         if (M%SP%FCL%operator_commute_test) then
           call operator_commute_test(M%mom%U,M%mom%p,M%mom%m,M%DT)
         endif
       end subroutine

       end module