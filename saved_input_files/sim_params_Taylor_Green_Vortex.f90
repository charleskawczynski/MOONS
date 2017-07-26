     module sim_params_mod
     use current_precision_mod
     use IO_tools_mod
     use string_mod
     use path_mod
     use var_mod
     use var_set_mod
     use solver_settings_mod
     use time_marching_params_mod
     use dir_tree_mod
     use dimensionless_params_mod
     use export_logicals_mod
     use flow_control_logicals_mod
     use mesh_quality_params_mod
     use export_frequency_mod
     use energy_terms_mod
     use export_lines_mod
     use export_planes_mod
     use momentum_terms_mod
     use induction_terms_mod
     use geometry_props_mod
     use mirror_props_mod
     use time_statistics_params_mod
     implicit none

     private
     public :: sim_params
     public :: init,delete,display,print,export,import

     public :: set_restart

     real(cp),parameter :: seconds_per_day = 60.0_cp*60.0_cp*24.0_cp

     interface init;         module procedure init_SP;            end interface
     interface delete;       module procedure delete_SP;          end interface
     interface init;         module procedure init_SP_copy;       end interface
     interface display;      module procedure display_SP;         end interface
     interface display;      module procedure display_SP_wrapper; end interface
     interface print;        module procedure print_SP;           end interface
     interface export;       module procedure export_SP;          end interface
     interface export;       module procedure export_SP_wrapper;  end interface
     interface import;       module procedure import_SP;          end interface
     interface import;       module procedure import_SP_wrapper;  end interface
     interface sanity_check; module procedure sanity_check_SP;    end interface
     interface set_restart;  module procedure set_restart_SP;     end interface

     type sim_params
       type(var_set) :: VS
       type(mesh_quality_params) :: MQP
       type(time_marching_params) :: coupled
       type(dimensionless_params) :: DP
       type(export_logicals) :: EL
       type(flow_control_logicals) :: FCL
       type(export_frequency) :: EF

       type(energy_terms) :: ET
       type(momentum_terms) :: MT
       type(induction_terms) :: IT
       type(geometry_props) :: GP
       type(mirror_props) :: MP
       type(time_statistics_params) :: TSP

       real(cp) :: export_safe_period
       logical :: restart_meshes
       logical :: export_heavy

       logical :: matrix_based
       logical :: print_every_MHD_step

       logical :: couple_time_steps
       logical :: finite_Rem
       logical :: include_vacuum
       logical :: embed_B_interior
       logical :: compute_surface_power
       integer :: uniform_B0_dir
       integer :: mpg_dir
       integer :: uniform_gravity_dir
     end type

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
       if (     RV_BCs) SP%finite_Rem                 = T ! Ensures all dt are equal to coupled%dt
       if (.not.RV_BCs) SP%finite_Rem                 = F ! Ensures all dt are equal to coupled%dt
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

       time                          = 10.0_cp
       ! time                          = 0.01_cp
       ! dtime                         = 1.0_cp*pow(-2)
       dtime                         = 1.0_cp*pow(-2)*0.5_cp**(0.0_cp)
       ! dtime                         = 5.0_cp*pow(-3)*0.5_cp**(0.0_cp)

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
       if (.not.SP%finite_Rem) SP%DP%Rem = 1.0_cp

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
       call init(SP%VS%P%ISP,  500 ,pow(-20),pow(-13),1,F,SP%export_heavy,str(DT%ISP),'ISP_P')
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
       ! if (SP%finite_Rem) SP%VS%B%MFP%coeff_explicit = SP%VS%B%MFP%coeff_explicit/SP%DP%Rem

       ! Sources to add to momentum equation. NOTE: scale is not set if add=false. ORDER MATTERS
       call init(SP%MT%pressure_grad       ,F,-1.0_cp                   )
       call init(SP%MT%diffusion           ,T,SP%VS%U%MFP%coeff_explicit)
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
       call init(SP%IT%B_applied  ,T, 1.0_cp           ) ! B0 = scale*B0
       call init(SP%IT%current    ,T, 1.0_cp/SP%DP%Rem ) ! J = scale curl(B)
       call init(SP%IT%advection  ,F, 1.0_cp           )
       call init(SP%IT%advection  ,T, 1.0_cp/SP%DP%Rem ) ! For Rem ne 1 in Bandaru
       call init(SP%IT%diffusion  ,T, -SP%VS%B%MFP%beta) ! since LHS and J includes scale
       call init(SP%IT%unsteady_B0,F, -1.0_cp          ) ! since RHS

       ! Sources to add to energy equation. NOTE: scale is not set if add=false. ORDER MATTERS
       call init(SP%ET%advection          , F,-1.0_cp           )
       call init(SP%ET%diffusion          , F,1.0_cp/SP%DP%Pe   )
       call init(SP%ET%KE_diffusion       , F,-SP%DP%Ec/SP%DP%Re)
       call init(SP%ET%viscous_dissipation, F, SP%DP%Ec/SP%DP%Re)
       call init(SP%ET%joule_heating      , F,SP%DP%Ec*SP%DP%N  )
       call init(SP%ET%volumetric_heating , F,1.0_cp            )

       if (SP%couple_time_steps) call couple_time_step(SP%VS,SP%coupled)
       ! call export_import_SS(SP%VS)
       call sanity_check(SP)
     end subroutine

     subroutine set_restart_SP(SP,restart_fields)
       implicit none
       type(sim_params),intent(inout) :: SP
       logical,intent(in) :: restart_fields
       SP%restart_meshes    = restart_fields
       SP%VS%T%SS%restart   = restart_fields
       SP%VS%U%SS%restart   = restart_fields
       SP%VS%P%SS%restart   = restart_fields
       SP%VS%B%SS%restart   = restart_fields
       SP%VS%B0%SS%restart  = restart_fields
       SP%VS%phi%SS%restart = restart_fields
       SP%VS%rho%SS%restart = restart_fields
       call import(SP%coupled)
       call import_TMP(SP%VS)
     end subroutine

     subroutine sanity_check_SP(SP)
       implicit none
       type(sim_params),intent(in) :: SP
       if (SP%coupled%n_step_stop.lt.1) stop 'Error: coupled%n_step_stop<1 in sim_params.f90'
       call sanity_check(SP%VS)
     end subroutine

     subroutine init_SP_copy(SP,SP_in)
       implicit none
       type(sim_params),intent(inout) :: SP
       type(sim_params),intent(in) :: SP_in
       SP%export_safe_period     = SP_in%export_safe_period
       SP%restart_meshes         = SP_in%restart_meshes
       SP%export_heavy           = SP_in%export_heavy
       SP%couple_time_steps      = SP_in%couple_time_steps
       SP%finite_Rem             = SP_in%finite_Rem
       SP%include_vacuum         = SP_in%include_vacuum
       SP%embed_B_interior       = SP_in%embed_B_interior
       SP%compute_surface_power  = SP_in%compute_surface_power
       SP%uniform_B0_dir         = SP_in%uniform_B0_dir
       SP%mpg_dir                = SP_in%mpg_dir
       SP%uniform_gravity_dir    = SP_in%uniform_gravity_dir
       SP%matrix_based           = SP_in%matrix_based
       SP%print_every_MHD_step   = SP_in%print_every_MHD_step
       call init(SP%FCL,    SP_in%FCL)
       call init(SP%GP,     SP_in%GP)
       call init(SP%MP,     SP_in%MP)
       call init(SP%EL,     SP_in%EL)
       call init(SP%VS,     SP_in%VS)
       call init(SP%ET,     SP_in%ET)
       call init(SP%MT,     SP_in%MT)
       call init(SP%IT,     SP_in%IT)
       call init(SP%DP,     SP_in%DP)
       call init(SP%coupled,SP_in%coupled)
       call init(SP%MQP,    SP_in%MQP)
       call init(SP%TSP,    SP_in%TSP)
       call init(SP%EF,     SP_in%EF)
     end subroutine

     subroutine delete_SP(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       call delete(SP%GP)
       call delete(SP%MP)
       call delete(SP%EL)
       call delete(SP%VS)
       call delete(SP%ET)
       call delete(SP%MT)
       call delete(SP%IT)
       call delete(SP%DP)
       call delete(SP%coupled)
       call delete(SP%MQP)
       call delete(SP%TSP)
       call delete(SP%EF)
     end subroutine

     subroutine display_SP(SP,un)
       implicit none
       type(sim_params),intent(in) :: SP
       integer,intent(in) :: un
       write(un,*) 'export_safe_period     = ',SP%export_safe_period
       write(un,*) 'restart_meshes         = ',SP%restart_meshes
       write(un,*) 'export_heavy           = ',SP%export_heavy
       write(un,*) 'couple_time_steps      = ',SP%couple_time_steps
       write(un,*) 'finite_Rem             = ',SP%finite_Rem
       write(un,*) 'include_vacuum         = ',SP%include_vacuum
       write(un,*) 'embed_B_interior       = ',SP%embed_B_interior
       write(un,*) 'compute_surface_power  = ',SP%compute_surface_power
       write(un,*) 'uniform_B0_dir         = ',SP%uniform_B0_dir
       write(un,*) 'mpg_dir                = ',SP%mpg_dir
       write(un,*) 'uniform_gravity_dir    = ',SP%uniform_gravity_dir
       write(un,*) 'matrix_based           = ',SP%matrix_based
       write(un,*) 'print_every_MHD_step   = ',SP%print_every_MHD_step
       call display(SP%FCL,un)
       call display(SP%GP,un)
       call display(SP%MP,un)
       call display(SP%EL,un)
       call display(SP%ET,un)
       call display(SP%MT,un)
       call display(SP%IT,un)
       call display(SP%VS,un)
       call display(SP%DP,un)
       call display(SP%MQP,un)
       call display(SP%TSP,un)
       call display(SP%EF,un)
       call display(SP%coupled,un)
       call display_compiler_info(un)
     end subroutine

     subroutine display_compiler_info(un)
       implicit none
       integer,intent(in) :: un
       write(un,*) ' ----------------- COMPILER FLAG INFO -------------- '
#ifdef _PARALLELIZE_GF_
       write(un,*) '_PARALLELIZE_GF_ = .true.'
#else
       write(un,*) '_PARALLELIZE_GF_ = .false.'
#endif
#ifdef _PARALLELIZE_BF_PLANE_
       write(un,*) '_PARALLELIZE_BF_PLANE_ = .true.'
#else
       write(un,*) '_PARALLELIZE_BF_PLANE_ = .false.'
#endif
#ifdef PARALLELIZE_2D_OPS
       write(un,*) 'PARALLELIZE_2D_OPS = .true.'
#else
       write(un,*) 'PARALLELIZE_2D_OPS = .false.'
#endif
#ifdef PARALLELIZE_1D_OPS
       write(un,*) 'PARALLELIZE_1D_OPS = .true.'
#else
       write(un,*) 'PARALLELIZE_1D_OPS = .false.'
#endif
     end subroutine

     subroutine display_SP_wrapper(SP,dir,name)
       implicit none
       type(sim_params),intent(in) :: SP
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call display(SP,un)
       call close_and_message(un,dir,name)
     end subroutine

     subroutine print_SP(SP)
       implicit none
       type(sim_params),intent(in) :: SP
       call display(SP,6)
     end subroutine

     subroutine export_SP(SP,un)
       implicit none
       type(sim_params),intent(in) :: SP
       integer,intent(in) :: un
       write(un,*) ' ---------------- sim_params ---------------- '
       write(un,*) 'export_safe_period    = '; write(un,*) SP%export_safe_period
       write(un,*) 'restart_meshes        = '; write(un,*) SP%restart_meshes
       write(un,*) 'export_heavy          = '; write(un,*) SP%export_heavy
       write(un,*) 'couple_time_steps     = '; write(un,*) SP%couple_time_steps
       write(un,*) 'finite_Rem            = '; write(un,*) SP%finite_Rem
       write(un,*) 'include_vacuum        = '; write(un,*) SP%include_vacuum
       write(un,*) 'embed_B_interior      = '; write(un,*) SP%embed_B_interior
       write(un,*) 'compute_surface_power = '; write(un,*) SP%compute_surface_power
       write(un,*) 'uniform_B0_dir        = '; write(un,*) SP%uniform_B0_dir
       write(un,*) 'mpg_dir               = '; write(un,*) SP%mpg_dir
       write(un,*) 'uniform_gravity_dir   = '; write(un,*) SP%uniform_gravity_dir
       write(un,*) 'matrix_based          = '; write(un,*) SP%matrix_based
       write(un,*) 'print_every_MHD_step  = '; write(un,*) SP%print_every_MHD_step
       call export(SP%FCL,un)
       call export(SP%GP,un)
       call export(SP%MP,un)
       call export(SP%EL,un)
       call export(SP%ET,un)
       call export(SP%MT,un)
       call export(SP%IT,un)
       call export(SP%VS,un)
       call export(SP%DP,un)
       call export(SP%MQP,un)
       call export(SP%TSP,un)
       call export(SP%EF,un)
       call export(SP%coupled,un)
       write(un,*) ' -------------------------------------------- '
     end subroutine

     subroutine import_SP(SP,un)
       implicit none
       type(sim_params),intent(inout) :: SP
       integer,intent(in) :: un
       read(un,*);
       read(un,*); read(un,*) SP%export_safe_period
       read(un,*); read(un,*) SP%restart_meshes
       read(un,*); read(un,*) SP%export_heavy
       read(un,*); read(un,*) SP%couple_time_steps
       read(un,*); read(un,*) SP%finite_Rem
       read(un,*); read(un,*) SP%include_vacuum
       read(un,*); read(un,*) SP%embed_B_interior
       read(un,*); read(un,*) SP%compute_surface_power
       read(un,*); read(un,*) SP%uniform_B0_dir
       read(un,*); read(un,*) SP%mpg_dir
       read(un,*); read(un,*) SP%uniform_gravity_dir
       read(un,*); read(un,*) SP%matrix_based
       read(un,*); read(un,*) SP%print_every_MHD_step
       call import(SP%FCL,un)
       call import(SP%GP,un)
       call import(SP%MP,un)
       call import(SP%EL,un)
       call import(SP%ET,un)
       call import(SP%MT,un)
       call import(SP%IT,un)
       call import(SP%VS,un)
       call import(SP%DP,un)
       call import(SP%MQP,un)
       call import(SP%TSP,un)
       call import(SP%EF,un)
       call import(SP%coupled,un)
       read(un,*);
     end subroutine

     subroutine export_SP_wrapper(SP,dir,name)
       implicit none
       type(sim_params),intent(in) :: SP
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call export(SP,un)
       call close_and_message(un,dir,name)
     end subroutine

     subroutine import_SP_wrapper(SP,dir,name)
       implicit none
       type(sim_params),intent(inout) :: SP
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call import(SP,un)
       call close_and_message(un,dir,name)
     end subroutine

     end module