     module sim_params_mod
     use current_precision_mod
     use IO_tools_mod
     use string_mod
     use path_mod
     use var_mod
     use var_set_mod
     use solver_settings_mod
     use time_marching_params_mod
     use dynamic_mesh_refinement_mod
     use dir_tree_mod
     use dimensionless_params_mod
     use export_logicals_mod
     use flow_control_logicals_mod
     use mesh_quality_params_mod
     use print_export_mod
     use momentum_forces_mod
     use geometry_props_mod
     use mirror_props_mod
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
       type(dynamic_mesh_refinement) :: DMR
       type(dimensionless_params) :: DP
       type(export_logicals) :: EL
       type(flow_control_logicals) :: FCL
       type(print_export) :: PE
       type(momentum_forces) :: MF
       type(geometry_props) :: GP
       type(mirror_props) :: MP

       logical :: restart_all

       logical :: matrix_based
       logical :: unsteady_B0

       logical :: couple_time_steps
       logical :: finite_Rem
       logical :: include_vacuum
     end type

     contains

     subroutine init_SP(SP,DT)
       implicit none
       type(sim_params),intent(inout) :: SP
       type(dir_tree),intent(in) :: DT
       logical,parameter :: T = .true.
       logical,parameter :: F = .false.
       real(cp) :: time,dtime
       SP%restart_all                = F ! restart sim (requires no code changes)

       SP%FCL%post_process_only      = F ! Skip solver loop and just post-process results
       SP%FCL%post_process           = T ! not used anywhere
       SP%FCL%stop_after_mesh_export = F !
       SP%FCL%stop_before_solve      = F ! Just export ICs, do not run simulation
       SP%FCL%skip_solver_loop       = F ! not used anywhere

       SP%EL%export_analytic         = F ! Export analytic solutions (MOONS.f90)
       SP%EL%export_meshes           = F ! Export all meshes before starting simulation
       SP%EL%export_vort_SF          = T ! Export vorticity-stream-function after simulation
       SP%EL%export_mat_props        = F ! Export material properties before starting simulation
       SP%EL%export_ICs              = F ! Export Post-Processed ICs before starting simulation
       SP%EL%export_cell_volume      = F ! Export cell volumes for each mesh
       SP%EL%export_planar           = F ! Export 2D data when N_cell = 1 along given direction
       SP%EL%export_symmetric        = T !
       SP%EL%export_mesh_block       = F ! Export mesh blocks to FECs
       SP%EL%export_soln_only        = F ! Export processed solution only

       SP%couple_time_steps          = T ! Ensures all dt are equal to coupled%dt
       SP%finite_Rem                 = T ! Ensures all dt are equal to coupled%dt
       SP%include_vacuum             = T ! Ensures all dt are equal to coupled%dt

       SP%matrix_based               = F ! Solve induction equation
       SP%unsteady_B0                = F ! Add unsteady applied field to induction eq.

       ! call init(MP,mirror,mirror_face)
       call init(SP%MP,T,6) ! Must be defined before KE_scale,ME_scale,JE_scale

       ! init(DMR,dynamic_refinement,n_max_refinements,n_history,SS_tol,SS_tol_final,dt_reduction_factor)
       call init(SP%DMR,F,2,2,pow(-1),pow(-6),1.2_cp)

       ! call init(PE,i_info,i_transient_0D,i_transient_2D,i_solution,export_planar,dir,name)
       call init(SP%PE,2,2,4,6,F,str(DT%PE),'PE')

       ! call init(MQP,auto_find_N,max_mesh_stretch_ratio,N_max_points_add)
       call init(SP%MQP,F,2.0_cp,50)

       time                          = 1000.0_cp
       dtime                         = 1.0_cp*pow(-4)

       SP%DP%Re                      = 4.0_cp*pow(2)
       SP%DP%Ha                      = 1.0_cp*pow(3)
       SP%DP%Rem                     = 1.0_cp*pow(2)
       SP%DP%cw                      = 0.0_cp
       SP%DP%sig_local_over_sig_f    = pow(-3)
       SP%DP%Gr                      = 0.0_cp
       SP%DP%Pr                      = 0.01_cp
       SP%DP%Fr                      = 1.0_cp
       SP%DP%Ec                      = 0.0_cp

       SP%DP%N                       = SP%DP%Ha**2.0_cp/SP%DP%Re
       SP%DP%Al                      = SP%DP%N/SP%DP%Rem
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

       SP%GP%tw                      = 0.05_cp
       SP%GP%geometry                = 14
       SP%GP%periodic_dir            = (/0,0,0/)
       SP%GP%apply_BC_order          = (/3,4,5,6,1,2/) ! good for LDC
       ! SP%GP%apply_BC_order       = (/5,6,3,4,1,2/) ! good for periodic in z?
       ! SP%GP%apply_BC_order       = (/3,4,1,2,5,6/) ! good for periodic in z?

       ! call init_IC_BC(var,IC,BC)
       call init_IC_BC(SP%VS%T,  0,0)
       call init_IC_BC(SP%VS%U,  0,2)
       call init_IC_BC(SP%VS%P,  0,0)
       call init_IC_BC(SP%VS%B,  0,7)
       call init_IC_BC(SP%VS%B0, 1,0)
       call init_IC_BC(SP%VS%phi,0,0)

       ! call init(SS,initialize,solve,restart,solve_method)
       call init(SP%VS%T%SS,  F,F,F,0)
       call init(SP%VS%U%SS,  T,T,F,3)
       call init(SP%VS%P%SS,  T,T,F,1)
       call init(SP%VS%B%SS,  T,T,F,3)
       call init(SP%VS%B0%SS, T,T,F,0)
       call init(SP%VS%phi%SS,T,T,F,0)

       ! call init(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res,export_convergence,dir,name)
       call init(SP%VS%T%ISP,  5  ,pow(-6),pow(-13),100,F,str(DT%ISP),'ISP_T')
       call init(SP%VS%U%ISP,  5  ,pow(-6),pow(-13),100,F,str(DT%ISP),'ISP_U')
       call init(SP%VS%P%ISP,  5  ,pow(-6),pow(-13),100,F,str(DT%ISP),'ISP_P')
       call init(SP%VS%B%ISP,  100,pow(-6),pow(-13),100,F,str(DT%ISP),'ISP_B')
       call init(SP%VS%B0%ISP, 5  ,pow(-6),pow(-13),100,F,str(DT%ISP),'ISP_B0')
       call init(SP%VS%phi%ISP,5  ,pow(-6),pow(-13),100,F,str(DT%ISP),'ISP_phi')

       ! call init(TMP,multistep_iter,n_step_stop,dtime,dir,name)
       call init(SP%coupled,1,ceiling(time/dtime,li),dtime,str(DT%TMP), 'TMP_coupled')
       call init(SP%VS%T%TMP,  1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_T')
       call init(SP%VS%U%TMP,  1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_U')
       call init(SP%VS%P%TMP,  1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_P')
       call init(SP%VS%B%TMP,  1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_B')
       call init(SP%VS%B0%TMP, 1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_B0')
       call init(SP%VS%phi%TMP,1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_phi')

       ! Matrix-free parameters:
       if (SP%finite_Rem) then; SP%VS%B%MFP%coeff = SP%VS%B%TMP%dt/SP%DP%Rem ! Backward Euler
       else;                    SP%VS%B%MFP%coeff = SP%VS%B%TMP%dt           ! Backward Euler
       endif
       SP%VS%U%MFP%coeff   = -0.5_cp*SP%VS%U%TMP%dt/SP%DP%Re            ! Crank Nicholson
       SP%VS%T%MFP%coeff   = -0.5_cp*SP%VS%T%TMP%dt/(SP%DP%Re*SP%DP%Pr) ! Crank Nicholson
       SP%VS%phi%MFP%coeff = 0.0_cp ! Poisson, coefficient unused
       SP%VS%p%MFP%coeff   = 0.0_cp ! Poisson, coefficient unused

       SP%MF%JCrossB             = T ! add JCrossB      to momentum equation
       SP%MF%Q2D_JCrossB         = F ! add Q2D JCrossB  to momentum equation
       SP%MF%Buoyancy            = F ! add Buoyancy     to momentum equation
       SP%MF%Gravity             = F ! add Gravity      to momentum equation

       if (SP%couple_time_steps) then
         call couple_time_step(SP%VS%T%TMP  ,SP%coupled)
         call couple_time_step(SP%VS%U%TMP  ,SP%coupled)
         call couple_time_step(SP%VS%P%TMP  ,SP%coupled)
         call couple_time_step(SP%VS%B%TMP  ,SP%coupled)
         call couple_time_step(SP%VS%B0%TMP ,SP%coupled)
         call couple_time_step(SP%VS%phi%TMP,SP%coupled)
       endif
       ! call export_import_SS(SP%VS)
       if (.not.SP%finite_Rem) SP%DP%Rem = 1.0_cp
       if (SP%coupled%n_step_stop.lt.1) stop 'Error: coupled%n_step_stop<1 in sim_params.f90'
      end subroutine

     subroutine init_SP_copy(SP,SP_in)
       implicit none
       type(sim_params),intent(inout) :: SP
       type(sim_params),intent(in) :: SP_in
       SP%restart_all            = SP_in%restart_all
       SP%couple_time_steps      = SP_in%couple_time_steps
       SP%finite_Rem             = SP_in%finite_Rem
       SP%include_vacuum         = SP_in%include_vacuum
       SP%matrix_based           = SP_in%matrix_based
       SP%unsteady_B0            = SP_in%unsteady_B0
       call init(SP%FCL,    SP_in%FCL)
       call init(SP%GP,     SP_in%GP)
       call init(SP%MP,     SP_in%MP)
       call init(SP%EL,     SP_in%EL)
       call init(SP%VS,     SP_in%VS)
       call init(SP%MF,     SP_in%MF)
       call init(SP%DP,     SP_in%DP)
       call init(SP%coupled,SP_in%coupled)
       call init(SP%DMR,    SP_in%DMR)
       call init(SP%MQP,    SP_in%MQP)
       call init(SP%PE,     SP_in%PE)
      end subroutine

     subroutine delete_SP(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       call delete(SP%GP)
       call delete(SP%MP)
       call delete(SP%EL)
       call delete(SP%VS)
       call delete(SP%MF)
       call delete(SP%DP)
       call delete(SP%coupled)
       call delete(SP%DMR)
       call delete(SP%MQP)
       call delete(SP%PE)
      end subroutine

     subroutine display_SP(SP,un)
       implicit none
       type(sim_params),intent(in) :: SP
       integer,intent(in) :: un
       write(un,*) 'restart_all            = ',SP%restart_all
       write(un,*) 'couple_time_steps      = ',SP%couple_time_steps
       write(un,*) 'finite_Rem             = ',SP%finite_Rem
       write(un,*) 'include_vacuum         = ',SP%include_vacuum
       write(un,*) 'matrix_based           = ',SP%matrix_based
       write(un,*) 'unsteady_B0            = ',SP%unsteady_B0
       call display(SP%FCL,un)
       call display(SP%GP,un)
       call display(SP%MP,un)
       call display(SP%EL,un)
       call display(SP%MF,un)
       call display(SP%VS,un)
       call display(SP%DP,un)
       call display(SP%DMR,un)
       call display(SP%MQP,un)
       call display(SP%PE,un)
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
       write(un,*) SP%matrix_based
       write(un,*) SP%unsteady_B0
       call export(SP%FCL,un)
       call export(SP%GP,un)
       call export(SP%MP,un)
       call export(SP%EL,un)
       call export(SP%MF,un)
       call export(SP%VS,un)
       call export(SP%DP,un)
       call export(SP%DMR,un)
       call export(SP%MQP,un)
       call export(SP%PE,un)
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
       read(un,*) SP%matrix_based
       read(un,*) SP%unsteady_B0
       call import(SP%FCL,un)
       call import(SP%GP,un)
       call import(SP%MP,un)
       call import(SP%EL,un)
       call import(SP%MF,un)
       call import(SP%VS,un)
       call import(SP%DP,un)
       call import(SP%DMR,un)
       call import(SP%MQP,un)
       call import(SP%PE,un)
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