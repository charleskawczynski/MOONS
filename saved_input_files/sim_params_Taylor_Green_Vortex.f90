     module sim_params_extend_mod
     use current_precision_mod
     use constants_mod
     use dir_tree_mod
     use var_mod
     use var_set_mod
     use string_mod
     use path_mod
     use segment_extend_mod
     use dimensionless_params_extend_mod
     use mesh_params_extend_mod
     use mesh_quality_params_mod
     use export_planes_mod
     use export_lines_mod
     use sim_params_mod
     use sim_params_aux_mod
     implicit none

     private
     public :: sim_params
     public :: init

     real(cp),parameter :: seconds_per_day = 60.0_cp*60.0_cp*24.0_cp

     interface init;         module procedure init_SP;            end interface
     interface define_mesh;  module procedure define_mesh_SP;     end interface

     contains

     subroutine init_SP(SP,DT)
       implicit none
       type(sim_params),intent(inout) :: SP
       type(dir_tree),intent(in) :: DT
       logical,parameter :: T = .true.
       logical,parameter :: F = .false.
       real(cp) :: time,dtime
       logical :: RV_BCs
       call delete(SP)
       RV_BCs = T
       ! call get_environment_variable(name[, value, length, status, trim_name)

       SP%FCL%stop_after_mesh_export = F !
       SP%FCL%stop_before_solve      = F ! Just export ICs, do not run simulation
       SP%FCL%skip_solver_loop       = F
       SP%FCL%post_process           = T
       SP%FCL%Poisson_test           = F
       SP%FCL%Taylor_Green_Vortex_test= T

       SP%EL%export_analytic         = F ! Export analytic solutions (MOONS.f90)
       SP%EL%export_meshes           = F ! Export all meshes before starting simulation
       SP%EL%export_vort_SF          = F ! Export vorticity-stream-function after simulation
       SP%EL%export_mat_props        = F ! Export material properties before starting simulation
       SP%EL%export_ICs              = F ! Export Post-Processed ICs before starting simulation
       SP%EL%export_cell_volume      = F ! Export cell volumes for each mesh
       SP%EL%export_planar           = F ! Export 2D data when N_cell = 1 along given direction
       SP%EL%export_symmetric        = F !
       SP%EL%export_mesh_block       = F ! Export mesh blocks to FECs
       SP%EL%export_soln_only        = F ! Export processed solution only

       SP%export_safe_period         = 1.0_cp*seconds_per_day ! CPU wall clock time to export regularly
       SP%restart_meshes             = F ! restart sim (requires no code changes)
       SP%export_heavy               = T ! Export lots of sim info
       SP%uniform_gravity_dir        = 1 ! Uniform gravity field direction
       SP%uniform_B0_dir             = 3 ! Uniform applied field direction
       SP%mpg_dir                    = 1 ! Uniform applied field direction
       SP%couple_time_steps          = T ! Ensures all dt are equal to coupled%dt
       if (     RV_BCs) SP%SCP%finite_Rem                 = T ! Ensures all dt are equal to coupled%dt
       if (.not.RV_BCs) SP%SCP%finite_Rem                 = F ! Ensures all dt are equal to coupled%dt
       if (     RV_BCs) SP%include_vacuum             = T ! Ensures all dt are equal to coupled%dt
       if (.not.RV_BCs) SP%include_vacuum             = F ! Ensures all dt are equal to coupled%dt
       SP%embed_B_interior           = F ! Solve for exterior B using interior B
       SP%compute_surface_power      = T ! Compute surface power for LDC

       SP%matrix_based               = F ! Solve induction equation
       SP%print_every_MHD_step       = F ! Print nstep every time stop (for debugging)

       ! call init(MP,mirror,mirror_face)
       call init(SP%MP,F,6) ! Must be defined before KE_scale,ME_scale,JE_scale
       SP%EL%export_symmetric = SP%MP%mirror

       ! call init(EFP,export_ever,export_first_step,frequency_base,frequency_exp)
       call init(SP%EF%info          ,T,T,1,10,2)
       call init(SP%EF%unsteady_0D   ,T,T,1,10,2)
       call init(SP%EF%unsteady_1D   ,F,F,1,10,2)
       call init(SP%EF%unsteady_2D   ,F,F,1,10,2)
       call init(SP%EF%unsteady_3D   ,F,F,1,10,4)
       call init(SP%EF%restart_files ,F,F,1,10,2)
       call init(SP%EF%final_solution,F,F,1,10,6)
       call init(SP%EF%dir,str(DT%EF))
       call init(SP%EF%name,'EF')

       ! call init(MQP,auto_find_N,max_mesh_stretch_ratio,N_max_points_add)
       call init(SP%MQP,T,1.5_cp,50)

       ! Statistics
       ! call init(TSP,collect,t_start,t_stop)
       ! call init(SP%TSP,T,30.0_cp,60.0_cp)
       call init(SP%TSP,F,700.0_cp,800.0_cp)

       time                          = 3.0_cp
       ! time                          = 0.01_cp
       ! dtime                         = 1.0_cp*pow(-2)
       dtime                         = 1.0_cp*pow(-2)*0.5_cp**(4.0_cp)

       SP%GP%tw                      = 0.05_cp
       SP%GP%geometry                = 30
       SP%GP%periodic_dir            = (/0,0,1/)
       ! SP%GP%apply_BC_order          = (/3,4,5,6,1,2/) ! good for LDC
       ! SP%GP%apply_BC_order       = (/3,4,5,6,1,2/) ! good for periodic in y?
       SP%GP%apply_BC_order       = (/5,6,1,2,3,4/) ! good for periodic in y?
       ! SP%GP%apply_BC_order       = (/5,6,3,4,1,2/) ! good for periodic in z?
       ! SP%GP%apply_BC_order       = (/3,4,1,2,5,6/) ! good for periodic in z?

       call delete(SP%DP)
       SP%DP%Re                      = 10.0_cp
       ! SP%DP%N                       = 5.0_cp*pow(0)
       SP%DP%Q                       = 9.0_cp*pow(-1)
       if (     RV_BCs) SP%DP%Rem                     = 10.0_cp*pow(0)
       if (.not.RV_BCs) SP%DP%Rem                     = 1.0_cp*pow(0)
       ! SP%DP%Ha                      = 5.0_cp*pow(2)
       ! SP%DP%Ha                      = 10.0_cp
       ! SP%DP%Ha                      = 10.0_cp*pow(3)
       ! SP%DP%Ha                      = 15.0_cp*pow(3)
       SP%DP%N                       = 1.0_cp/SP%DP%Q
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

       SP%DP%Ha                      = (1.0_cp/SP%DP%Q*SP%DP%Re)**0.5_cp
       ! SP%DP%N                       = SP%DP%Ha**2.0_cp/SP%DP%Re
       ! SP%DP%Ha                      = (SP%DP%N*SP%DP%Re)**0.5_cp
       SP%DP%Al                      = SP%DP%N/SP%DP%Rem
       SP%DP%Pe                      = SP%DP%Pr*SP%DP%Re
       SP%DP%tau                     = SP%DP%Re/SP%DP%Ha
       SP%DP%L_eta                   = SP%DP%Re**(-0.75_cp)
       SP%DP%U_eta                   = SP%DP%Re**(-0.25_cp)
       SP%DP%t_eta                   = SP%DP%Re**(-0.50_cp)
       SP%DP%KE_scale                = 1.0_cp
       SP%DP%ME_scale                = SP%DP%Al
       SP%DP%JE_scale                = SP%DP%N*2.0_cp ! x2 because (J^2/sigma) not (1/2 J^2/sigma)
       if (SP%MP%mirror) SP%DP%KE_scale = SP%DP%KE_scale*2.0_cp
       if (SP%MP%mirror) SP%DP%ME_scale = SP%DP%ME_scale*2.0_cp
       if (SP%MP%mirror) SP%DP%JE_scale = SP%DP%JE_scale*2.0_cp
       if (.not.SP%SCP%finite_Rem) SP%DP%Rem = 1.0_cp

       ! call init(export_field,export_ever)
       call init(SP%VS%T%unsteady_field  ,F)
       call init(SP%VS%U%unsteady_field  ,F)
       call init(SP%VS%P%unsteady_field  ,F)
       call init(SP%VS%B%unsteady_field  ,F)
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
       call init(SP%VS%U%SS  ,T         ,T    ,F      ,T            ,6)
       call init(SP%VS%P%SS  ,T         ,T    ,F      ,F            ,0)
       call init(SP%VS%B%SS  ,F         ,F    ,F      ,T            ,6)
       call init(SP%VS%B0%SS ,F         ,F    ,F      ,F            ,0)
       call init(SP%VS%phi%SS,F         ,F    ,F      ,F            ,0)
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
       call init_IC_BC(SP%VS%U    ,6    ,18)
       call init_IC_BC(SP%VS%P    ,2    ,0 )
       ! if (     RV_BCs) call init_IC_BC(SP%VS%B    ,0    ,9 ) ! 5 for thin wall
       ! if (.not.RV_BCs) call init_IC_BC(SP%VS%B    ,0    ,10) ! 5 for thin wall
       call init_IC_BC(SP%VS%B    ,0    ,2 )
       call init_IC_BC(SP%VS%B0   ,4    ,0 )
       call init_IC_BC(SP%VS%phi  ,0    ,0 )
       call init_IC_BC(SP%VS%rho  ,0    ,0 )

       ! call init(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res,export_convergence,export_heavy,dir,name)
       call init(SP%VS%T%ISP,  5   ,pow(-6),pow(-13),1,F,SP%export_heavy,str(DT%ISP),'ISP_T')
       call init(SP%VS%U%ISP,  50  ,pow(-6),pow(-13),1,F,SP%export_heavy,str(DT%ISP),'ISP_U')
       call init(SP%VS%P%ISP,  100 ,pow(-20),pow(-13),1,F,SP%export_heavy,str(DT%ISP),'ISP_P')
       if (     RV_BCs) call init(SP%VS%B%ISP,  20 ,pow(-6),pow(-13),1,F,SP%export_heavy,str(DT%ISP),'ISP_B')
       if (.not.RV_BCs) call init(SP%VS%B%ISP,  5 ,pow(-6),pow(-13),1,F,SP%export_heavy,str(DT%ISP),'ISP_B')
       call init(SP%VS%B0%ISP, 5  ,pow(-6),pow(-13),1,F,SP%export_heavy,str(DT%ISP),'ISP_B0')
       call init(SP%VS%phi%ISP,5  ,pow(-6),pow(-13),1,F,SP%export_heavy,str(DT%ISP),'ISP_phi')
       call init(SP%VS%rho%ISP,5  ,pow(-6),pow(-13),1,F,SP%export_heavy,str(DT%ISP),'ISP_rho')

       ! call init(TMP,RK_n_stages,RK_active,multistep_iter,n_step_stop,dtime,dir,name)
       call init(SP%coupled,   1,F,1 ,ceiling(time/dtime,li),dtime        ,str(DT%TMP),'TMP_coupled')
       call init(SP%VS%T%TMP,  1,F,1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_T')
       call init(SP%VS%U%TMP,  1,F,1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_U')
       call init(SP%VS%P%TMP,  1,F,1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_P')
       call init(SP%VS%B%TMP,  1,F,1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_B')
       call init(SP%VS%B0%TMP, 1,F,1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_B0')
       call init(SP%VS%phi%TMP,1,F,1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_phi')
       call init(SP%VS%rho%TMP,1,F,1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_rho')

       ! Matrix-free parameters:
       ! coeff_natural  = coefficient of terms in non-discretized equation
       ! coeff_explicit = coefficient of explicit terms without time discretization
       ! coeff_implicit = coefficient of implicit terms without time discretization
       ! coeff_implicit_time_split = dt*coeff_implicit/coeff_unsteady (computed in time_marching_methods.f90)

       SP%VS%B%MFP%alpha = 1.0_cp ! weight of implicit treatment (1 = Backward Euler, .5 = Crank Nicholson)
       SP%VS%U%MFP%alpha = 0.5_cp ! weight of implicit treatment (1 = Backward Euler, .5 = Crank Nicholson)
       SP%VS%T%MFP%alpha = 1.0_cp ! weight of implicit treatment (1 = Backward Euler, .5 = Crank Nicholson)
       SP%VS%B%MFP%coeff_natural = -1.0_cp/SP%DP%Rem ! natural diffusion coefficient on RHS
       SP%VS%U%MFP%coeff_natural =  1.0_cp/SP%DP%Re  ! natural diffusion coefficient on RHS
       SP%VS%T%MFP%coeff_natural =  1.0_cp/SP%DP%Pe  ! natural diffusion coefficient on RHS
       call assign_beta(SP%VS)           ! weight of explicit treatment, alpha must be defined first
       call assign_coeff_explicit(SP%VS) ! RHS diffusion coefficient, (beta ,coeff_natural) must be defined first
       call assign_coeff_implicit(SP%VS) ! LHS diffusion coefficient, (alpha,coeff_natural) must be defined first

       ! The following is needed only if curl-curl(B) is used, opposed to J in solver.
       ! if (SP%SCP%finite_Rem) SP%VS%B%MFP%coeff_explicit = SP%VS%B%MFP%coeff_explicit/SP%DP%Rem

       ! Sources to add to momentum equation. NOTE: scale is not set if add=false. ORDER MATTERS
       call init(SP%MT%pressure_grad       ,F,-1.0_cp                   )
       call init(SP%MT%diffusion           ,T,SP%VS%U%MFP%coeff_explicit)
       call init(SP%MT%diffusion_linear    ,F,SP%VS%U%MFP%coeff_explicit)
       call init(SP%MT%advection_convection,F,-1.0_cp/SP%DP%Rem         )
       call init(SP%MT%advection_convection,T,-1.0_cp                   )
       call init(SP%MT%advection_divergence,F,-1.0_cp/SP%DP%Rem         ) ! For Rem ne 1 in Bandaru
       call init(SP%MT%advection_divergence,F,-1.0_cp                   )
       call init(SP%MT%advection_base_flow ,F,-1.0_cp                   )
       call init(SP%MT%mean_pressure_grad  ,F,1.0_cp                    )
       call init(SP%MT%JCrossB             ,F,SP%DP%N                   )
       call init(SP%MT%JCrossB             ,F,SP%DP%N*SP%DP%Rem         ) ! For Rem ne 1 in Bandaru (look at J definition in Bandaru)
       call init(SP%MT%Q2D_JCrossB         ,F,-1.0_cp/SP%DP%tau         )
       call init(SP%MT%Buoyancy            ,F,SP%DP%Gr/SP%DP%Re**2.0_cp )
       call init(SP%MT%Gravity             ,F,1.0_cp/SP%DP%Fr**2.0_cp   )

       ! Sources to add to induction equation. NOTE: scale is not set if add=false. ORDER MATTERS
       call init(SP%IT%B_applied       ,T, 1.0_cp           ) ! B0 = scale*B0
       call init(SP%IT%current         ,T, 1.0_cp/SP%DP%Rem ) ! J = scale curl(B)
       call init(SP%IT%advection       ,F, 1.0_cp           )
       call init(SP%IT%advection       ,T, 1.0_cp/SP%DP%Rem ) ! For Rem ne 1 in Bandaru
       call init(SP%IT%diffusion       ,T, -SP%VS%B%MFP%beta) ! since LHS and J includes scale
       call init(SP%IT%diffusion_linear,F, -SP%VS%B%MFP%beta) ! since LHS and J includes scale
       call init(SP%IT%unsteady_B0     ,F, -1.0_cp          ) ! since RHS

       ! Sources to add to energy equation. NOTE: scale is not set if add=false. ORDER MATTERS
       call init(SP%ET%advection          , F,-1.0_cp           )
       call init(SP%ET%diffusion          , F,1.0_cp/SP%DP%Pe   )
       call init(SP%ET%KE_diffusion       , F,-SP%DP%Ec/SP%DP%Re)
       call init(SP%ET%viscous_dissipation, F, SP%DP%Ec/SP%DP%Re)
       call init(SP%ET%joule_heating      , F,SP%DP%Ec*SP%DP%N  )
       call init(SP%ET%volumetric_heating , F,1.0_cp            )

       call post_process(SP,DT)
     end subroutine

     end module