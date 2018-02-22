     module sim_params_init_logicals_mod
     use current_precision_mod
     use constants_mod
     use mirror_props_extend_mod
     use sim_params_mod
     use sim_params_aux_mod
     implicit none

     private
     public :: sim_params_init_logicals

     real(cp),parameter :: seconds_per_day = 60.0_cp*60.0_cp*24.0_cp
     real(cp),parameter :: minutes = 60.0_cp

     contains

     subroutine sim_params_init_logicals(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       logical,parameter :: T = .true.
       logical,parameter :: F = .false.

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
       SP%FCL%print_every_MHD_step               = T
       SP%FCL%print_mesh_before_solve            = F
       SP%FCL%compute_surface_power              = F

       SP%EL%export_analytic                     = F
       SP%EL%export_meshes                       = T
       SP%EL%export_vort_SF                      = F
       SP%EL%export_mat_props                    = F
       SP%EL%export_ICs                          = F
       SP%EL%export_cell_volume                  = F
       SP%EL%export_planar                       = F
       SP%EL%export_symmetric                    = F
       SP%EL%export_mesh_block                   = F
       SP%EL%export_soln_only                    = F

       SP%SCP%export_safe_period                 = 1.0_cp*seconds_per_day
       ! SP%SCP%export_safe_period                 = 1.0_cp*minutes
       SP%SCP%uniform_gravity_dir                = 1
       SP%SCP%uniform_B0_dir                     = 3
       SP%SCP%mpg_dir                            = 1
       SP%SCP%couple_time_steps                  = T
       SP%SCP%finite_Rem                         = T
       SP%SCP%include_vacuum                     = F
       SP%SCP%embed_B_interior                   = F

       ! call init(MP,mirror,mirror_face)
       call init(SP%MP,F,6) ! Must be defined before KE_scale,ME_scale,JE_scale
       SP%EL%export_symmetric = SP%MP%mirror
     end subroutine

     end module