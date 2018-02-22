     module sim_params_init_solver_config_mod
     use current_precision_mod
     use constants_mod
     use string_mod
     use dir_tree_mod
     use var_extend_mod
     use var_set_extend_mod
     use path_extend_mod
     use equation_term_extend_mod
     use solver_settings_extend_mod
     use time_marching_params_extend_mod
     use iter_solver_params_extend_mod
     use sim_params_mod
     use sim_params_aux_mod
     implicit none

     private
     public :: sim_params_init_solver_config

     contains

     subroutine sim_params_init_solver_config(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       logical,parameter :: T = .true.
       logical,parameter :: F = .false.

       ! call init(SS        ,initialize,solve,restart,prescribe_BCs,solve_method)
       call init(SP%VS%T%SS  ,F         ,F    ,F      ,F            ,0)
       call init(SP%VS%U%SS  ,T         ,T    ,F      ,T            ,6)
       call init(SP%VS%P%SS  ,T         ,T    ,F      ,F            ,0)
       call init(SP%VS%B%SS  ,T         ,T    ,F      ,T            ,6)
       call init(SP%VS%B0%SS ,T         ,T    ,F      ,F            ,0)
       call init(SP%VS%phi%SS,T         ,T    ,F      ,F            ,0)
       call init(SP%VS%rho%SS,F         ,F    ,F      ,F            ,0)
       !     solve_method = 1 = Euler_time_no_diff_Euler_sources_no_correction
       !     solve_method = 2 = Euler_time_no_diff_AB2_sources_no_correction
       !     solve_method = 3 = Euler_time_no_diff_Euler_sources
       !     solve_method = 4 = Euler_time_no_diff_AB2_sources
       !     solve_method = 5 = Euler_time_Euler_sources
       !     solve_method = 6 = Euler_time_AB2_sources
       !     solve_method = 7 = O2_BDF_time_AB2_sources
       !     solve_method = 8 = Euler_time_AB2_sources_new

       ! call init(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res,export_convergence)
       call init(SP%VS%T%ISP,  5  ,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)
       call init(SP%VS%U%ISP,  20 ,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)
       call init(SP%VS%P%ISP,  40 ,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)
       call init(SP%VS%B%ISP,  100,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)
       call init(SP%VS%B0%ISP, 5  ,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)
       call init(SP%VS%phi%ISP,100,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)
       call init(SP%VS%rho%ISP,5  ,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)

       ! call init(TMP,RK_n_stages,RK_active,multistep_iter,t_start,t_stop,dtime)
       call init(SP%coupled,   1,F,1 ,SP%DP%t_start,SP%DP%t_final,SP%DP%dtime)
       call init(SP%VS%T%TMP,  1,F,1 ,SP%DP%t_start,SP%DP%t_final,SP%DP%dtime)
       call init(SP%VS%U%TMP,  1,F,1 ,SP%DP%t_start,SP%DP%t_final,SP%DP%dtime)
       call init(SP%VS%P%TMP,  1,F,1 ,SP%DP%t_start,SP%DP%t_final,SP%DP%dtime)
       call init(SP%VS%B%TMP,  1,F,1 ,SP%DP%t_start,SP%DP%t_final,SP%DP%dtime)
       call init(SP%VS%B0%TMP, 1,F,1 ,SP%DP%t_start,SP%DP%t_final,SP%DP%dtime)
       call init(SP%VS%phi%TMP,1,F,1 ,SP%DP%t_start,SP%DP%t_final,SP%DP%dtime)
       call init(SP%VS%rho%TMP,1,F,1 ,SP%DP%t_start,SP%DP%t_final,SP%DP%dtime)

       ! Matrix-free parameters:
       ! coeff_natural  = coefficient of terms in non-discretized equation
       ! coeff_explicit = coefficient of explicit terms without time discretization
       ! coeff_implicit = coefficient of implicit terms without time discretization
       ! coeff_implicit_time_split = dt*coeff_implicit/coeff_unsteady (computed in time_marching_methods.f90)

       SP%VS%B%MFP%alpha = 1.0_cp ! weight of implicit treatment (1 = Backward Euler, .5 = Crank Nicholson)
       SP%VS%U%MFP%alpha = 1.0_cp ! weight of implicit treatment (1 = Backward Euler, .5 = Crank Nicholson)
       SP%VS%T%MFP%alpha = 1.0_cp ! weight of implicit treatment (1 = Backward Euler, .5 = Crank Nicholson)
       SP%VS%B%MFP%coeff_natural = -1.0_cp/SP%DP%Rem ! natural diffusion coefficient on RHS
       SP%VS%U%MFP%coeff_natural =  1.0_cp/SP%DP%Re  ! natural diffusion coefficient on RHS
       SP%VS%T%MFP%coeff_natural =  1.0_cp/SP%DP%Pe  ! natural diffusion coefficient on RHS
       call assign_beta(SP%VS)           ! weight of explicit treatment, alpha must be defined first
       call assign_coeff_explicit(SP%VS) ! RHS diffusion coefficient, (beta ,coeff_natural) must be defined first
       call assign_coeff_implicit(SP%VS) ! LHS diffusion coefficient, (alpha,coeff_natural) must be defined first
     end subroutine

     end module