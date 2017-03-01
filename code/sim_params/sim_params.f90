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

     interface init;    module procedure init_SP;           end interface
     interface delete;  module procedure delete_SP;         end interface
     interface init;    module procedure init_SP_copy;      end interface
     interface display; module procedure display_SP;        end interface
     interface display; module procedure display_SP_wrapper;end interface
     interface print;   module procedure print_SP;          end interface
     interface export;  module procedure export_SP;         end interface
     interface export;  module procedure export_SP_wrapper; end interface
     interface import;  module procedure import_SP;         end interface
     interface import;  module procedure import_SP_wrapper; end interface

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

       logical :: restart_all

       logical :: matrix_based
       logical :: prescribed_periodic_BCs
       logical :: print_every_MHD_step

       logical :: couple_time_steps
       logical :: finite_Rem
       logical :: include_vacuum
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

       SP%FCL%stop_after_mesh_export = F !
       SP%FCL%stop_before_solve      = F ! Just export ICs, do not run simulation
       SP%FCL%skip_solver_loop       = F ! not used anywhere
       SP%FCL%post_process           = T ! not used anywhere
       SP%FCL%Poisson_test           = F ! not used anywhere

       SP%EL%export_analytic         = F ! Export analytic solutions (MOONS.f90)
       SP%EL%export_meshes           = F ! Export all meshes before starting simulation
       SP%EL%export_vort_SF          = T ! Export vorticity-stream-function after simulation
       SP%EL%export_mat_props        = F ! Export material properties before starting simulation
       SP%EL%export_ICs              = F ! Export Post-Processed ICs before starting simulation
       SP%EL%export_cell_volume      = F ! Export cell volumes for each mesh
       SP%EL%export_planar           = F ! Export 2D data when N_cell = 1 along given direction
       SP%EL%export_symmetric        = F !
       SP%EL%export_mesh_block       = F ! Export mesh blocks to FECs
       SP%EL%export_soln_only        = F ! Export processed solution only

       SP%restart_all                = F ! restart sim (requires no code changes)
       SP%uniform_gravity_dir        = 1 ! Uniform gravity field direction
       SP%uniform_B0_dir             = 3 ! Uniform applied field direction
       SP%mpg_dir                    = 0 ! Uniform applied field direction
       SP%couple_time_steps          = T ! Ensures all dt are equal to coupled%dt
       SP%finite_Rem                 = F ! Ensures all dt are equal to coupled%dt
       SP%include_vacuum             = F ! Ensures all dt are equal to coupled%dt
       SP%compute_surface_power      = F ! Compute surface power for LDC

       SP%matrix_based               = F ! Solve induction equation
       SP%prescribed_periodic_BCs    = T ! Ustar,Bstar defined by relation in Kim 1985
       SP%print_every_MHD_step       = F ! Print nstep every time stop (for debugging)

       ! call init(MP,mirror,mirror_face)
       call init(SP%MP,F,6) ! Must be defined before KE_scale,ME_scale,JE_scale

       ! call init(EFP,export_ever,export_first_step,frequency_base,frequency_exp)
       call init(SP%EF%info          ,T,T,1,10,1)
       call init(SP%EF%unsteady_0D   ,T,T,1,10,2)
       call init(SP%EF%unsteady_1D   ,T,F,1,10,2)
       call init(SP%EF%unsteady_2D   ,T,F,1,10,2)
       call init(SP%EF%unsteady_3D   ,F,F,1,10,4)
       call init(SP%EF%restart_files ,F,F,1,10,2)
       call init(SP%EF%final_solution,F,F,1,10,6)
       call init(SP%EF%dir,str(DT%EF))
       call init(SP%EF%name,'EF')

       ! call init(MQP,auto_find_N,max_mesh_stretch_ratio,N_max_points_add)
       call init(SP%MQP,F,2.0_cp,50)

       ! call init(TSP,collect,t_start,t_stop)
       call init(SP%TSP,F,100.0_cp,500.0_cp)

       time                          = 10000.0_cp
       dtime                         = 1.0_cp*pow(-2)

       SP%GP%tw                      = 0.05_cp
       SP%GP%geometry                = 9
       SP%GP%periodic_dir            = (/0,0,0/)
       ! SP%GP%apply_BC_order          = (/3,4,5,6,1,2/) ! good for LDC
       ! SP%GP%apply_BC_order       = (/3,4,5,6,1,2/) ! good for periodic in y?
       SP%GP%apply_BC_order       = (/5,6,1,2,3,4/) ! good for periodic in y?
       ! SP%GP%apply_BC_order       = (/5,6,3,4,1,2/) ! good for periodic in z?
       ! SP%GP%apply_BC_order       = (/3,4,1,2,5,6/) ! good for periodic in z?

       call delete(SP%DP)
       SP%DP%Re                      = 1.0_cp*pow(2)
       ! SP%DP%Q                       = 4.4_cp*pow(-1)
       SP%DP%Rem                     = 1.0_cp*pow(0)
       SP%DP%Ha                      = 1.0_cp*pow(1)
       ! SP%DP%N                       = 1.0_cp/SP%DP%Q
       ! SP%DP%N                       = 1.0_cp*pow(-1)
       SP%DP%cw                      = 0.0_cp
       SP%DP%sig_local_over_sig_f    = 1.0_cp*pow(0)
       SP%DP%Gr                      = 0.0_cp
       SP%DP%Pr                      = 0.01_cp
       SP%DP%Fr                      = 1.0_cp
       SP%DP%Ec                      = 0.0_cp

       ! SP%DP%Ha                      = (1.0_cp/SP%DP%Q*SP%DP%Re)**0.5_cp
       SP%DP%N                       = SP%DP%Ha**2.0_cp/SP%DP%Re
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
       call init(SP%VS%U%unsteady_planes  ,F,3,23,'1')
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

       ! call init_IC_BC(var      ,IC   ,BC)
       call init_IC_BC(SP%VS%T    ,0    ,0 )
       call init_IC_BC(SP%VS%U    ,0    ,1 )
       call init_IC_BC(SP%VS%P    ,0    ,0 )
       call init_IC_BC(SP%VS%B    ,0    ,1 )
       call init_IC_BC(SP%VS%B0   ,1    ,0 )
       call init_IC_BC(SP%VS%phi  ,0    ,0 )
       call init_IC_BC(SP%VS%rho  ,0    ,0 )

       ! call init(SS        ,initialize,solve,restart,solve_method)
       call init(SP%VS%T%SS  ,F         ,F    ,F      ,0)
       call init(SP%VS%U%SS  ,T         ,T    ,F      ,3)
       call init(SP%VS%P%SS  ,T         ,T    ,F      ,0)
       call init(SP%VS%B%SS  ,T         ,T    ,F      ,3)
       call init(SP%VS%B0%SS ,T         ,T    ,F      ,0)
       call init(SP%VS%phi%SS,T         ,T    ,F      ,0)
       call init(SP%VS%rho%SS,F         ,F    ,F      ,0)

       ! call init(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res,export_convergence,dir,name)
       call init(SP%VS%T%ISP,  5  ,pow(-6),pow(-13),1,F,str(DT%ISP),'ISP_T')
       call init(SP%VS%U%ISP,  5  ,pow(-6),pow(-13),1,F,str(DT%ISP),'ISP_U')
       call init(SP%VS%P%ISP,  5  ,pow(-6),pow(-13),1,F,str(DT%ISP),'ISP_P')
       ! if (     RV_BCs) call init(SP%VS%B%ISP,  20 ,pow(-6),pow(-13),1,F,str(DT%ISP),'ISP_B')
       ! if (.not.RV_BCs) call init(SP%VS%B%ISP,  5  ,pow(-6),pow(-13),1,F,str(DT%ISP),'ISP_B')
       call init(SP%VS%B%ISP,  5  ,pow(-6),pow(-13),1,F,str(DT%ISP),'ISP_B')
       call init(SP%VS%B0%ISP, 5  ,pow(-6),pow(-13),1,F,str(DT%ISP),'ISP_B0')
       call init(SP%VS%phi%ISP,5  ,pow(-6),pow(-13),1,F,str(DT%ISP),'ISP_phi')
       call init(SP%VS%rho%ISP,5  ,pow(-6),pow(-13),1,F,str(DT%ISP),'ISP_rho')

       ! call init(TMP,multistep_iter,n_step_stop,dtime,dir,name)
       call init(SP%coupled,   1 ,ceiling(time/dtime,li),dtime        ,str(DT%TMP),'TMP_coupled')
       call init(SP%VS%T%TMP,  1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_T')
       call init(SP%VS%U%TMP,  1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_U')
       call init(SP%VS%P%TMP,  1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_P')
       call init(SP%VS%B%TMP,  1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_B')
       call init(SP%VS%B0%TMP, 1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_B0')
       call init(SP%VS%phi%TMP,1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_phi')
       call init(SP%VS%rho%TMP,1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_rho')

       ! Matrix-free parameters:
       ! theta          = weight for explicit to implicit treatment
       ! coeff_natural  = coefficient of terms in non-discretized equation
       ! coeff_explicit = coefficient of explicit terms without time discretization
       ! coeff_implicit = coefficient of implicit terms without time discretization
       ! coeff_implicit_time_split = dt*coeff_implicit/coeff_unsteady (computed in time_marching_methods.f90)

       SP%VS%B%MFP%alpha = 1.0_cp ! weight of implicit treatment (1 = Backward Euler, .5 = Crank Nicholson)
       SP%VS%U%MFP%alpha = 0.5_cp ! weight of implicit treatment (1 = Backward Euler, .5 = Crank Nicholson)
       SP%VS%T%MFP%alpha = 0.5_cp ! weight of implicit treatment (1 = Backward Euler, .5 = Crank Nicholson)

       SP%VS%B%MFP%beta =  1.0_cp - SP%VS%B%MFP%alpha ! weight of explicit treatment
       SP%VS%U%MFP%beta =  1.0_cp - SP%VS%U%MFP%alpha ! weight of explicit treatment
       SP%VS%T%MFP%beta =  1.0_cp - SP%VS%T%MFP%alpha ! weight of explicit treatment

       SP%VS%B%MFP%coeff_natural = -1.0_cp/SP%DP%Rem ! natural diffusion coefficient on RHS
       SP%VS%U%MFP%coeff_natural =  1.0_cp/SP%DP%Re  ! natural diffusion coefficient on RHS
       SP%VS%T%MFP%coeff_natural =  1.0_cp/SP%DP%Pe  ! natural diffusion coefficient on RHS

       SP%VS%B%MFP%coeff_explicit   = SP%VS%B%MFP%coeff_natural*SP%VS%B%MFP%beta ! RHS diffusion coefficient
       SP%VS%U%MFP%coeff_explicit   = SP%VS%U%MFP%coeff_natural*SP%VS%U%MFP%beta ! RHS diffusion coefficient
       SP%VS%T%MFP%coeff_explicit   = SP%VS%T%MFP%coeff_natural*SP%VS%T%MFP%beta ! RHS diffusion coefficient
       SP%VS%phi%MFP%coeff_explicit = 0.0_cp ! Poisson, coefficient unused
       SP%VS%p%MFP%coeff_explicit   = 0.0_cp ! Poisson, coefficient unused
       SP%VS%rho%MFP%coeff_explicit = 0.0_cp ! Poisson, coefficient unused

       SP%VS%B%MFP%coeff_implicit   = -SP%VS%B%MFP%coeff_natural*SP%VS%B%MFP%alpha ! LHS diffusion coefficient
       SP%VS%U%MFP%coeff_implicit   = -SP%VS%U%MFP%coeff_natural*SP%VS%U%MFP%alpha ! LHS diffusion coefficient
       SP%VS%T%MFP%coeff_implicit   = -SP%VS%T%MFP%coeff_natural*SP%VS%T%MFP%alpha ! LHS diffusion coefficient
       SP%VS%phi%MFP%coeff_implicit = 0.0_cp ! Poisson, coefficient unused
       SP%VS%p%MFP%coeff_implicit   = 0.0_cp ! Poisson, coefficient unused
       SP%VS%rho%MFP%coeff_implicit = 0.0_cp ! Poisson, coefficient unused

       ! The following is needed only if curl-curl(B) is used, opposed to J in solver.
       ! if (SP%finite_Rem) SP%VS%B%MFP%coeff_explicit = SP%VS%B%MFP%coeff_explicit/SP%DP%Rem

       SP%MT%diffusion%add              = T ! add diffusion              to momentum equation
       SP%MT%advection_convection%add   = F ! add advection (conv form)  to momentum equation
       SP%MT%advection_divergence%add   = T ! add advection (div  form)  to momentum equation
       SP%MT%mean_pressure_grad%add     = F ! add mean pressure gradient to momentum equation
       SP%MT%JCrossB%add                = T ! add JCrossB                to momentum equation
       SP%MT%Q2D_JCrossB%add            = F ! add Q2D JCrossB            to momentum equation
       SP%MT%Buoyancy%add               = F ! add Buoyancy               to momentum equation
       SP%MT%Gravity%add                = F ! add Gravity                to momentum equation

       SP%MT%diffusion%scale            = SP%VS%U%MFP%coeff_explicit
       SP%MT%advection_convection%scale = -1.0_cp
       SP%MT%advection_divergence%scale = -1.0_cp
       ! SP%MT%advection_divergence%scale = -1.0_cp/SP%DP%Rem ! For Rem ne 1 in Bandaru
       SP%MT%mean_pressure_grad%scale   = 1.0_cp
       SP%MT%JCrossB%scale              = SP%DP%N
       SP%MT%Q2D_JCrossB%scale          = -1.0_cp/SP%DP%tau
       SP%MT%Buoyancy%scale             = SP%DP%Gr/SP%DP%Re**2.0_cp
       SP%MT%Gravity%scale              = 1.0_cp/SP%DP%Fr**2.0_cp
       ! SP%MT%JCrossB%scale              = SP%DP%N*SP%DP%Rem ! For Rem ne 1 in Bandaru (look at J definition)

       SP%IT%advection%add              = T ! add advection              to induction equation
       SP%IT%diffusion%add              = T ! add diffusion              to induction equation
       SP%IT%unsteady_B0%add            = F ! add unsteady_B0            to induction equation
       SP%IT%advection%scale            = 1.0_cp
       SP%IT%diffusion%scale            = -SP%VS%B%MFP%beta ! since LHS and J includes scale
       SP%IT%unsteady_B0%scale          = -1.0_cp ! since RHS
       SP%IT%current%scale              = 1.0_cp/SP%DP%Rem ! J = scale curl(B)
       ! SP%IT%advection%scale            = 1.0_cp/SP%DP%Rem ! For Rem ne 1 in Bandaru

       SP%ET%advection%add              = F ! add advection           to energy equation
       SP%ET%diffusion%add              = F ! add diffusion           to energy equation
       SP%ET%KE_diffusion%add           = F ! add KE_diffusion        to energy equation
       SP%ET%viscous_dissipation%add    = F ! add viscous_dissipation to energy equation
       SP%ET%joule_heating%add          = F ! add joule_heating       to energy equation
       SP%ET%volumetric_heating%add     = F ! add volumetric_heating  to energy equation
       SP%ET%advection%scale            = -1.0_cp
       SP%ET%diffusion%scale            = 1.0_cp/SP%DP%Pe
       SP%ET%KE_diffusion%scale         = -SP%DP%Ec/SP%DP%Re
       SP%ET%viscous_dissipation%scale  =  SP%DP%Ec/SP%DP%Re
       SP%ET%joule_heating%scale        = SP%DP%Ec*SP%DP%N
       SP%ET%volumetric_heating%scale   = 1.0_cp ! Not sure what this scale was...

       if (SP%couple_time_steps) then
         call couple_time_step(SP%VS%T%TMP  ,SP%coupled)
         call couple_time_step(SP%VS%U%TMP  ,SP%coupled)
         call couple_time_step(SP%VS%P%TMP  ,SP%coupled)
         call couple_time_step(SP%VS%B%TMP  ,SP%coupled)
         call couple_time_step(SP%VS%B0%TMP ,SP%coupled)
         call couple_time_step(SP%VS%phi%TMP,SP%coupled)
         call couple_time_step(SP%VS%rho%TMP,SP%coupled)
       endif
       ! call export_import_SS(SP%VS)
       call sanity_check(SP)
      end subroutine

     subroutine sanity_check(SP)
       implicit none
       type(sim_params),intent(in) :: SP
       if (SP%coupled%n_step_stop.lt.1) stop 'Error: coupled%n_step_stop<1 in sim_params.f90'
       if (SP%VS%T%SS%solve  .and.(.not.SP%VS%T%SS%initialize))   stop 'Error: solve but not init? T'
       if (SP%VS%U%SS%solve  .and.(.not.SP%VS%U%SS%initialize))   stop 'Error: solve but not init? U'
       if (SP%VS%P%SS%solve  .and.(.not.SP%VS%P%SS%initialize))   stop 'Error: solve but not init? P'
       if (SP%VS%B%SS%solve  .and.(.not.SP%VS%B%SS%initialize))   stop 'Error: solve but not init? B'
       if (SP%VS%B0%SS%solve .and.(.not.SP%VS%B0%SS%initialize))  stop 'Error: solve but not init? B0'
       if (SP%VS%phi%SS%solve.and.(.not.SP%VS%phi%SS%initialize)) stop 'Error: solve but not init? phi'
       if (SP%VS%rho%SS%solve.and.(.not.SP%VS%rho%SS%initialize)) stop 'Error: solve but not init? rho'
     end subroutine

     subroutine init_SP_copy(SP,SP_in)
       implicit none
       type(sim_params),intent(inout) :: SP
       type(sim_params),intent(in) :: SP_in
       SP%restart_all            = SP_in%restart_all
       SP%couple_time_steps      = SP_in%couple_time_steps
       SP%finite_Rem             = SP_in%finite_Rem
       SP%include_vacuum         = SP_in%include_vacuum
       SP%compute_surface_power  = SP_in%compute_surface_power
       SP%uniform_B0_dir         = SP_in%uniform_B0_dir
       SP%mpg_dir                = SP_in%mpg_dir
       SP%uniform_gravity_dir    = SP_in%uniform_gravity_dir
       SP%matrix_based           = SP_in%matrix_based
       SP%prescribed_periodic_BCs= SP_in%prescribed_periodic_BCs
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
       write(un,*) 'restart_all            = ',SP%restart_all
       write(un,*) 'couple_time_steps      = ',SP%couple_time_steps
       write(un,*) 'finite_Rem             = ',SP%finite_Rem
       write(un,*) 'include_vacuum         = ',SP%include_vacuum
       write(un,*) 'compute_surface_power  = ',SP%compute_surface_power
       write(un,*) 'uniform_B0_dir         = ',SP%uniform_B0_dir
       write(un,*) 'mpg_dir                = ',SP%mpg_dir
       write(un,*) 'uniform_gravity_dir    = ',SP%uniform_gravity_dir
       write(un,*) 'matrix_based           = ',SP%matrix_based
       write(un,*) 'prescribed_periodic_BCs= ',SP%prescribed_periodic_BCs
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
       write(un,*) SP%restart_all
       write(un,*) SP%couple_time_steps
       write(un,*) SP%finite_Rem
       write(un,*) SP%include_vacuum
       write(un,*) SP%compute_surface_power
       write(un,*) SP%uniform_B0_dir
       write(un,*) SP%mpg_dir
       write(un,*) SP%uniform_gravity_dir
       write(un,*) SP%matrix_based
       write(un,*) SP%prescribed_periodic_BCs
       write(un,*) SP%print_every_MHD_step
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
      end subroutine

     subroutine import_SP(SP,un)
       implicit none
       type(sim_params),intent(inout) :: SP
       integer,intent(in) :: un
       read(un,*) SP%restart_all
       read(un,*) SP%couple_time_steps
       read(un,*) SP%finite_Rem
       read(un,*) SP%include_vacuum
       read(un,*) SP%compute_surface_power
       read(un,*) SP%uniform_B0_dir
       read(un,*) SP%mpg_dir
       read(un,*) SP%uniform_gravity_dir
       read(un,*) SP%matrix_based
       read(un,*) SP%prescribed_periodic_BCs
       read(un,*) SP%print_every_MHD_step
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