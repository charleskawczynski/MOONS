     module sim_params_extend_mod
     use current_precision_mod
     use constants_mod
     use string_mod
     use dir_tree_mod
     use var_extend_mod
     use var_set_extend_mod
     use path_extend_mod
     use mirror_props_extend_mod
     use equation_term_extend_mod
     use export_frequency_params_extend_mod
     use export_frequency_extend_mod
     use dimensionless_params_extend_mod
     use mesh_params_extend_mod
     use segment_extend_mod
     use mesh_quality_params_extend_mod
     use time_statistics_params_extend_mod
     use solver_settings_extend_mod
     use time_marching_params_extend_mod
     use iter_solver_params_extend_mod
     use export_field_extend_mod
     use export_planes_extend_mod
     use export_lines_extend_mod
     use sim_params_mod
     use sim_params_aux_mod
     implicit none

     private
     public :: sim_params
     public :: init

     real(cp),parameter :: seconds_per_day = 60.0_cp*60.0_cp*24.0_cp
     real(cp),parameter :: minutes = 60.0_cp

     interface init;         module procedure init_SP;            end interface
     interface define_mesh;  module procedure define_mesh_SP;     end interface

     contains

     subroutine define_mesh_SP_plasma_disruption(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_wall,t_fluid,CR,buffer
       integer :: N,N_w
       CR = 10.0_cp ! Cavity Ratio
       t_fluid = 1.0_cp; t_wall = 0.05_cp; buffer = 1.5_cp;
       ! N = 100; N_w = 9
       N = 30; N_w = 6
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_Roberts_B'   ,N   ,-t_fluid   ,t_fluid   ,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_B'   ,N   ,-t_fluid   ,t_fluid   ,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B'   ,N*2 ,-t_fluid*CR,t_fluid*CR,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call add_ext(SP%MP_sigma,seg_1d(1,'ext_Roberts_B_IO',N_w,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(2,'ext_Roberts_B_IO',N_w,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(3,'ext_Roberts_B_IO',N_w,t_wall,buffer))
       call init(SP%MP_ind,SP%MP_sigma)
     end subroutine

     subroutine small_dataset(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_fluid,buffer
       integer :: N
       t_fluid = 1.0_cp; buffer = 1.0_cp; N = 5;
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_Roberts_B'   ,N ,-t_fluid,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_B'   ,N ,-t_fluid,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B'   ,N ,-t_fluid,t_fluid,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call init(SP%MP_ind,SP%MP_sigma)
     end subroutine

     subroutine define_mesh_SP_full_BC_symmetric_geometry(SP) ! Correctness confirmed 11/27/2017
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_wall,t_fluid,t_vac,buffer
       integer :: N,N_w,N_half
       t_vac = 7.0_cp; t_fluid = 1.0_cp; t_wall = 0.05_cp; buffer = 2.0_cp; N = 80; N_w = 9; N_half = ceiling(N/2.0_cp)
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_Roberts_B'         ,N       ,-t_fluid        ,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_L'         ,N_half  ,-t_fluid        ,0.0_cp ,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_R'         ,N_half+5, 0.0_cp         ,t_fluid,buffer))
       call add_ext( SP%MP_mom,seg_1d(2,'ext_prep_Roberts_C2F_IO',N_half  , 0.0_cp+t_fluid ,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call add_ext(SP%MP_sigma,seg_1d(1,'ext_Roberts_B_IO'      ,N_w-1,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(3,'ext_prep_Roberts_B_IO' ,N_w-1,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(2,'ext_prep_Roberts_B_IO' ,N_w-1,t_wall,buffer))
       call init(SP%MP_ind,SP%MP_sigma)
       call add_ext(SP%MP_ind,seg_1d(1,'ext_Roberts_near_IO'      ,  N_w+1,t_vac - t_fluid - t_wall,buffer))
       call add_ext(SP%MP_ind,seg_1d(3,'ext_prep_Roberts_R_IO'    ,  N_w+2,t_vac - t_fluid - t_wall,buffer))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_prep_Roberts_R_IO'    ,  N_w-1,t_vac - t_fluid - t_wall,buffer))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_app_Roberts_L_IO'     ,2*N_w-2,t_vac - t_fluid         ,buffer))
     end subroutine

     subroutine define_mesh_SP_MHD_LDC_Sergey_uniform(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_uniform',45,-1.0_cp,1.0_cp,1.0_cp))
       call add_base(SP%MP_mom,seg_1d(2,'grid_uniform',45,-1.0_cp,1.0_cp,1.0_cp))
       call add_base(SP%MP_mom,seg_1d(3,'grid_uniform',45,-1.0_cp,1.0_cp,1.0_cp))
       call init(SP%MP_ind,SP%MP_mom)
       call init(SP%MP_sigma,SP%MP_ind)
       call add_ext(SP%MP_ind,seg_1d(1,'ext_uniform_IO',11))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_uniform_IO',11))
       call add_ext(SP%MP_ind,seg_1d(3,'ext_uniform_IO',11))
     end subroutine

     subroutine define_mesh_SP(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       call define_mesh_SP_plasma_disruption(SP)
       ! call small_dataset(SP)
       ! call define_mesh_SP_full_BC_symmetric_geometry(SP)
       ! call define_mesh_SP_MHD_LDC_Sergey_uniform(SP)
     end subroutine

     subroutine init_SP(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       logical,parameter :: T = .true.
       logical,parameter :: F = .false.
       real(cp) :: t_final,dtime
       call delete(SP)

       SP%FCL%stop_after_mesh_export             = F
       SP%FCL%stop_before_solve                  = F
       SP%FCL%skip_solver_loop                   = F
       SP%FCL%post_process                       = F
       SP%FCL%Poisson_test                       = F
       SP%FCL%matrix_visualization               = F
       SP%FCL%Taylor_Green_Vortex_test           = F
       SP%FCL%temporal_convergence_test          = F
       SP%FCL%operator_commute_test              = F
       SP%FCL%compute_export_E_K_Budget          = F
       SP%FCL%compute_export_E_M_budget          = F
       SP%FCL%restart_meshes                     = F

       SP%FCL%simulate_crash                     = F
       SP%FCL%restart_simulated_crash            = F
       SP%FCL%export_numerical_flow_rate         = F
       SP%FCL%export_Shercliff_Hunt_analytic_sol = F
       SP%FCL%export_vorticity_streamfunction    = F
       SP%FCL%export_heavy                       = T
       SP%FCL%export_final_tec                   = T
       SP%FCL%export_final_restart               = T
       SP%FCL%print_every_MHD_step               = F
       SP%FCL%print_mesh_before_solve            = F
       SP%FCL%compute_surface_power              = F

       SP%EL%export_analytic         = F
       SP%EL%export_meshes           = T
       SP%EL%export_vort_SF          = F
       SP%EL%export_mat_props        = F
       SP%EL%export_ICs              = F
       SP%EL%export_cell_volume      = F
       SP%EL%export_planar           = F
       SP%EL%export_symmetric        = F
       SP%EL%export_mesh_block       = F
       SP%EL%export_soln_only        = F

       SP%SCP%export_safe_period         = 1.0_cp*seconds_per_day
       ! SP%SCP%export_safe_period         = 1.0_cp*minutes
       SP%SCP%uniform_gravity_dir        = 1
       SP%SCP%uniform_B0_dir             = 3
       SP%SCP%mpg_dir                    = 1
       SP%SCP%couple_time_steps          = T
       SP%SCP%finite_Rem                 = T
       SP%SCP%include_vacuum             = F
       SP%SCP%embed_B_interior           = F

       ! call init(MP,mirror,mirror_face)
       call init(SP%MP,F,6) ! Must be defined before KE_scale,ME_scale,JE_scale
       SP%EL%export_symmetric = SP%MP%mirror

       ! call init(MQP,auto_find_N,max_mesh_stretch_ratio,N_max_points_add)
       call init(SP%MQP,T,2.0_cp,50)

       call define_mesh(SP)

       ! Statistics
       ! call init(TSP,collect,t_start,t_stop)
       ! call init(SP%TSP,T,30.0_cp,60.0_cp)
       call init(SP%TSP,F,700.0_cp,800.0_cp)

       t_final                          = 5.0_cp
       ! dtime                         = 1.0_cp*pow(-2)
       ! dtime                         = 1.0_cp*pow(-4)*0.5_cp**(4.0_cp)
       ! dtime                         = 1.0_cp*pow(-5)
       dtime                         = 1.0_cp*pow(-4)

       SP%GP%tw                      = 0.05_cp
       SP%GP%geometry                = 7
       SP%GP%periodic_dir            = (/0,0,0/)
       ! SP%GP%apply_BC_order          = (/3,4,5,6,1,2/) ! good for LDC
       ! SP%GP%apply_BC_order       = (/3,4,5,6,1,2/) ! good for periodic in y?
       SP%GP%apply_BC_order       = (/5,6,1,2,3,4/) ! good for periodic in y?
       ! SP%GP%apply_BC_order       = (/5,6,3,4,1,2/) ! good for periodic in z?
       ! SP%GP%apply_BC_order       = (/3,4,1,2,5,6/) ! good for periodic in z?

       call delete(SP%DP)
       SP%DP%Re                      = 11769054.2652_cp
       ! SP%DP%N                       = 1.0_cp
       SP%DP%Ha                      = 2645.75131106_cp
       SP%DP%Rem                     = 1.1131984703
       ! SP%DP%Q                       = 8.0_cp*pow(-1)
       ! SP%DP%Ha                      = 5.0_cp*pow(2)
       ! SP%DP%Ha                      = 10.0_cp*pow(3)
       ! SP%DP%Ha                      = 15.0_cp*pow(3)
       ! SP%DP%N                       = 1.0_cp/SP%DP%Q
       SP%DP%c_w(1:6)                = 0.0_cp
       SP%DP%c_w( 5 )                = 1.0_cp
       SP%DP%c_w( 6 )                = 1.0_cp
       SP%DP%Robin_coeff             = 0.0_cp
       SP%DP%Robin_coeff(5:6)        = -1.0_cp/SP%DP%c_w(5:6)
       ! SP%DP%c_w_coeff                = (2.0_cp*SP%DP%c_w/dh_nhat-1.0_cp)/(2.0_cp*SP%DP%c_w/dh_nhat+1.0_cp)
       ! SP%DP%sig_local_over_sig_f    = 1.0_cp*pow(-3)
       SP%DP%sig_local_over_sig_f    = 1.0_cp
       SP%DP%Gr                      = 0.0_cp
       SP%DP%Pr                      = 0.01_cp
       SP%DP%Fr                      = 1.0_cp
       SP%DP%Ec                      = 0.0_cp

       ! SP%DP%Ha                      = (1.0_cp/SP%DP%Q*SP%DP%Re)**0.5_cp
       SP%DP%N                       = SP%DP%Ha**2.0_cp/SP%DP%Re
       SP%DP%Ha                      = (SP%DP%N*SP%DP%Re)**0.5_cp
       SP%DP%Al                      = SP%DP%N/SP%DP%Rem
       SP%DP%Pe                      = SP%DP%Pr*SP%DP%Re
       SP%DP%tau                     = SP%DP%Re/SP%DP%Ha
       SP%DP%L_eta                   = SP%DP%Re**(-0.75_cp)
       SP%DP%U_eta                   = SP%DP%Re**(-0.25_cp)
       SP%DP%t_eta                   = SP%DP%Re**(-0.50_cp)
       SP%DP%KE_scale                = 1.0_cp
       SP%DP%ME_scale                = SP%DP%Al
       SP%DP%JE_scale                = SP%DP%N*2.0_cp ! x2 because (J^2/sigma) not (1/2 J^2/sigma)
       if (.not.SP%SCP%finite_Rem) SP%DP%Rem = 1.0_cp

       ! call init(EFP,export_ever,export_first_step,frequency_base,frequency_exp)
       call init(SP%EF%info          ,T,T,1,10,2,0.0_cp,t_final)
       call init(SP%EF%unsteady_0D   ,T,T,1,10,2,0.0_cp,t_final)
       call init(SP%EF%unsteady_1D   ,F,F,1,10,2,0.0_cp,t_final)
       call init(SP%EF%unsteady_2D   ,F,F,1,10,2,0.0_cp,t_final)
       call init(SP%EF%unsteady_3D   ,T,T,1,10,4,0.0_cp,2.0_cp)
       call init(SP%EF%restart_files ,F,F,1,10,2,0.0_cp,t_final)
       call init(SP%EF%final_solution,F,F,1,10,6,0.0_cp,t_final)

       ! call init(export_field,export_ever)
       call init(SP%VS%T%unsteady_field  ,F)
       call init(SP%VS%U%unsteady_field  ,T)
       call init(SP%VS%P%unsteady_field  ,F)
       call init(SP%VS%B%unsteady_field  ,T)
       call init(SP%VS%B0%unsteady_field ,F)
       call init(SP%VS%phi%unsteady_field,F)
       call init(SP%VS%rho%unsteady_field,F)
       ! call init(export_plane,export_ever,dir,plane)
       call init(SP%VS%T%unsteady_planes  ,F,2,1,'1')
       call init(SP%VS%U%unsteady_planes  ,F,3,2,'1')
       call add (SP%VS%U%unsteady_planes  ,F,3,23,'2')
       call init(SP%VS%P%unsteady_planes  ,F,2,1,'1')
       call init(SP%VS%B%unsteady_planes  ,F,2,1,'1')
       call init(SP%VS%B0%unsteady_planes ,F,2,1,'1')
       call init(SP%VS%phi%unsteady_planes,F,2,1,'1')
       call init(SP%VS%rho%unsteady_planes,F,2,1,'1')
       ! call init(export_line,export_ever,dir,line)
       call init(SP%VS%T%unsteady_lines  ,F,1,(/2,34/),'1')
       call init(SP%VS%U%unsteady_lines  ,F,1,(/2,34/),'1')
       call init(SP%VS%P%unsteady_lines  ,F,1,(/2,34/),'1')
       call init(SP%VS%B%unsteady_lines  ,F,1,(/2,34/),'1')
       call init(SP%VS%B0%unsteady_lines ,F,1,(/2,34/),'1')
       call init(SP%VS%phi%unsteady_lines,F,1,(/2,34/),'1')
       call init(SP%VS%rho%unsteady_lines,F,1,(/2,34/),'1')

       ! call init(SS        ,initialize,solve,restart,prescribe_BCs,solve_method)
       call init(SP%VS%T%SS  ,F         ,F    ,F      ,F            ,0)
       call init(SP%VS%U%SS  ,T         ,F    ,F      ,T            ,6)
       call init(SP%VS%P%SS  ,T         ,F    ,F      ,F            ,0)
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

       ! call init_IC_BC(var      ,IC   ,BC)
       call init_IC_BC(SP%VS%T    ,0    ,0 )
       call init_IC_BC(SP%VS%U    ,0    ,0 )
       call init_IC_BC(SP%VS%P    ,0    ,0 )
       call init_IC_BC(SP%VS%B    ,0    ,1 )
       call init_IC_BC(SP%VS%B0   ,1    ,0 )
       call init_IC_BC(SP%VS%phi  ,0    ,0 )
       call init_IC_BC(SP%VS%rho  ,0    ,0 )

       ! call init(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res,export_convergence)
       call init(SP%VS%T%ISP,  5  ,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)
       call init(SP%VS%U%ISP,  5  ,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)
       call init(SP%VS%P%ISP,  5  ,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)
       call init(SP%VS%B%ISP,  5  ,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)
       call init(SP%VS%B0%ISP, 5  ,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)
       call init(SP%VS%phi%ISP,5  ,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)
       call init(SP%VS%rho%ISP,5  ,pow(-6),pow(-13),1,F,SP%FCL%export_heavy)

       ! call init(TMP,RK_n_stages,RK_active,multistep_iter,n_step_stop,dtime)
       call init(SP%coupled,   1,F,1 ,t_final,dtime)
       call init(SP%VS%T%TMP,  1,F,1 ,t_final,dtime)
       call init(SP%VS%U%TMP,  1,F,1 ,t_final,dtime)
       call init(SP%VS%P%TMP,  1,F,1 ,t_final,dtime)
       call init(SP%VS%B%TMP,  1,F,1 ,t_final,dtime)
       call init(SP%VS%B0%TMP, 1,F,1 ,t_final,dtime)
       call init(SP%VS%phi%TMP,1,F,1 ,t_final,dtime)
       call init(SP%VS%rho%TMP,1,F,1 ,t_final,dtime)

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

       ! The following is needed only if curl-curl(B) is used, opposed to J in solver.
       ! if (SP%SCP%finite_Rem) SP%VS%B%MFP%coeff_explicit = SP%VS%B%MFP%coeff_explicit/SP%DP%Rem

       ! IF A TERM IS REPEATED, THE ONLY TERM THAT COUNTS IS THE LAST ONE DEFINED TO BE "TRUE"
       ! Sources to add to momentum equation. NOTE: scale is not set if add=false
       call init(SP%MT%pressure_grad       ,F,-1.0_cp                   )
       call init(SP%MT%diffusion           ,T,SP%VS%U%MFP%coeff_explicit)
       call init(SP%MT%diffusion_linear    ,F,SP%VS%U%MFP%coeff_explicit)
       call init(SP%MT%advection_convection,F,-1.0_cp/SP%DP%Rem         )
       call init(SP%MT%advection_convection,T,-1.0_cp                   )
       call init(SP%MT%advection_divergence,F,-1.0_cp                   )
       call init(SP%MT%advection_divergence,F,-1.0_cp/SP%DP%Rem         ) ! For Rem ne 1 in Bandaru
       call init(SP%MT%advection_base_flow ,F,-1.0_cp                   )
       call init(SP%MT%mean_pressure_grad  ,F,1.0_cp                    )
       call init(SP%MT%JCrossB             ,F,SP%DP%N*SP%DP%Rem         ) ! For Rem ne 1 in Bandaru (look at J definition in Bandaru)
       call init(SP%MT%JCrossB             ,T,SP%DP%N                   )
       call init(SP%MT%Q2D_JCrossB         ,F,-1.0_cp/SP%DP%tau         )
       call init(SP%MT%Buoyancy            ,F,SP%DP%Gr/SP%DP%Re**2.0_cp )
       call init(SP%MT%Gravity             ,F,1.0_cp/SP%DP%Fr**2.0_cp   )

       ! Terms computed in induction module... NOTE: scale is not set if add=false
       call init(SP%IT%B_applied       ,T, 1.0_cp           ) ! B0 = scale*B0
       call init(SP%IT%current         ,T, 1.0_cp/SP%DP%Rem ) ! J = scale curl(B)
       call init(SP%IT%advection       ,F, 1.0_cp/SP%DP%Rem ) ! For Rem ne 1 in Bandaru
       call init(SP%IT%advection       ,T, 1.0_cp           )
       call init(SP%IT%diffusion       ,T, -SP%VS%B%MFP%beta) ! since LHS and J includes scale
       call init(SP%IT%diffusion_linear,F, -SP%VS%B%MFP%beta) ! since LHS and J includes scale
       call init(SP%IT%unsteady_B0     ,T, -1.0_cp          ) ! since RHS

       ! Sources to add to energy equation. NOTE: scale is not set if add=false
       call init(SP%ET%advection          , F,-1.0_cp           )
       call init(SP%ET%diffusion          , F,1.0_cp/SP%DP%Pe   )
       call init(SP%ET%KE_diffusion       , F,-SP%DP%Ec/SP%DP%Re)
       call init(SP%ET%viscous_dissipation, F, SP%DP%Ec/SP%DP%Re)
       call init(SP%ET%joule_heating      , F,SP%DP%Ec*SP%DP%N  )
       call init(SP%ET%volumetric_heating , F,1.0_cp            )

       call post_process(SP)
     end subroutine

     end module