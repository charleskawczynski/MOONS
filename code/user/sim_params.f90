     module sim_params_mod
     use current_precision_mod
     use benchmark_case_mod
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
     use mesh_quality_params_mod
     implicit none

     private
     public :: sim_params
     public :: init,export

     type sim_params
       type(mesh_quality_params) :: MQP
       type(benchmark_case) :: BMC
       type(time_marching_params) :: coupled
       type(dynamic_mesh_refinement) :: DMR
       type(dimensionless_params) :: DP
       type(export_logicals) :: EL
       logical :: restart_all

       logical :: post_process_only        ! depricated
       logical :: post_process
       logical :: skip_solver_loop
       logical :: stop_before_solve
       logical :: stop_after_mesh_export

       logical :: matrix_based

       logical :: coupled_time_step
       logical :: finite_Rem
       logical :: include_vacuum

       logical :: addJCrossB
       logical :: addBuoyancy
       logical :: addGravity
       logical :: add_Q2D_JCrossB
     end type

     interface init;    module procedure init_sim_params;        end interface
     interface init;    module procedure init_sim_params_copy;   end interface

     interface export;  module procedure export_SP;              end interface
     interface export;  module procedure export_SP_wrapper;      end interface

     contains

     subroutine init_sim_params(SP,DT)
       implicit none
       type(sim_params),intent(inout) :: SP
       type(dir_tree),intent(in) :: DT
       logical,parameter :: T = .true.
       logical,parameter :: F = .false.
       real(cp) :: time,dtime
       time                      = 30.0_cp
       dtime                     = 1.0_cp*pow(-2)
       SP%restart_all            = F ! restart sim (requires no code changes)

       SP%post_process_only      = F ! Skip solver loop and just post-process results
       SP%post_process           = F ! Skip solver loop and just post-process results
       SP%stop_after_mesh_export = F !
       SP%stop_before_solve      = F ! Just export ICs, do not run simulation
       SP%skip_solver_loop       = F ! Skip solver loop

       SP%EL%export_analytic     = F ! Export analytic solutions (MOONS.f90)
       SP%EL%export_meshes       = T ! Export all meshes before starting simulation
       SP%EL%export_mat_props    = F ! Export material properties before starting simulation
       SP%EL%export_ICs          = F ! Export Post-Processed ICs before starting simulation
       SP%EL%export_cell_volume  = F ! Export cell volumes for each mesh
       SP%EL%export_planar       = F ! Export 2D data when N_cell = 1 along given direction
       SP%EL%export_symmetric    = F !
       SP%EL%export_mesh_block   = F ! Export mesh blocks to FECs
       SP%EL%export_soln_only    = F ! Export processed solution only

       SP%coupled_time_step      = F ! Ensures all dt are equal to coupled%dt
       SP%finite_Rem             = F ! Ensures all dt are equal to coupled%dt
       SP%include_vacuum         = F ! Ensures all dt are equal to coupled%dt

       SP%matrix_based           = F ! Solve induction equation

       SP%DP%Re                   = 1.0_cp*pow(2)
       SP%DP%Ha                   = 1.0_cp*pow(1)
       SP%DP%Rem                  = 1.0_cp*pow(0)
       SP%DP%tw                   = 0.5_cp
       SP%BMC%cw                  = 0.0_cp
       SP%DP%sig_local_over_sig_f = pow(0)
       SP%DP%Gr                   = 0.0_cp
       SP%DP%Pr                   = 0.01_cp
       SP%DP%Fr                   = 1.0_cp
       SP%DP%Ec                   = 0.0_cp
       SP%BMC%geometry            = 9
       SP%BMC%periodic_dir        = (/0,0,0/)

       ! init(DMR,dynamic_refinement,n_max_refinements,n_history,SS_tol,SS_tol_final,dt_reduction_factor)
       call init(SP%DMR,F,2,2,pow(-1),pow(-6),1.2_cp)

       ! call init(MQP,auto_find_N,max_mesh_stretch_ratio,N_max_points_add)
       call init(SP%MQP,F,2.0_cp,50)

       ! call init_IC_BC(var,IC,BC)
       call init_IC_BC(SP%BMC%VS%T,  0,0)
       call init_IC_BC(SP%BMC%VS%U,  0,1)
       call init_IC_BC(SP%BMC%VS%P,  0,0)
       call init_IC_BC(SP%BMC%VS%B,  0,1)
       call init_IC_BC(SP%BMC%VS%B0, 1,0)
       call init_IC_BC(SP%BMC%VS%phi,0,0)

       ! call init(SS,initialize,solve,restart,solve_method)
       call init(SP%BMC%VS%T%SS,  T,F,F,0)
       call init(SP%BMC%VS%U%SS,  T,T,F,1)
       call init(SP%BMC%VS%P%SS,  T,T,F,1)
       call init(SP%BMC%VS%B%SS,  T,T,F,1)
       call init(SP%BMC%VS%B0%SS, T,F,F,0)
       call init(SP%BMC%VS%phi%SS,T,T,F,0)

       ! call init(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res,dir,name)
       call init(SP%BMC%VS%T%ISP,  1000,pow(-5),pow(-7),100,str(DT%ISP), 'ISP_T')
       call init(SP%BMC%VS%U%ISP,  1000,pow(-5),pow(-7),100,str(DT%ISP), 'ISP_U')
       call init(SP%BMC%VS%P%ISP,  5   ,pow(-5),pow(-7),100,str(DT%ISP), 'ISP_P')
       call init(SP%BMC%VS%B%ISP,  1000,pow(-5),pow(-7),100,str(DT%ISP), 'ISP_B')
       call init(SP%BMC%VS%B0%ISP, 1000,pow(-5),pow(-7),100,str(DT%ISP), 'ISP_B0')
       call init(SP%BMC%VS%phi%ISP,5   ,pow(-5),pow(-7),100,str(DT%ISP), 'ISP_phi')

       ! call init(TMP,multistep_iter,n_step_stop,dtime,dir,name)
       call init(SP%coupled,1,ceiling(time/dtime,li),dtime,str(DT%TMP), 'TMP_coupled')
       call init(SP%BMC%VS%T%TMP,  1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_T')
       call init(SP%BMC%VS%U%TMP,  1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_U')
       call init(SP%BMC%VS%P%TMP,  1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_P')
       call init(SP%BMC%VS%B%TMP,  5 ,SP%coupled%n_step_stop,SP%coupled%dt/pow(2),str(DT%TMP),'TMP_B')
       call init(SP%BMC%VS%B0%TMP, 1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_B0')
       call init(SP%BMC%VS%phi%TMP,1 ,SP%coupled%n_step_stop,SP%coupled%dt,str(DT%TMP),'TMP_phi')

       SP%addJCrossB             = T ! add JCrossB      to momentum equation
       SP%add_Q2D_JCrossB        = F ! add Q2D JCrossB  to momentum equation
       SP%addBuoyancy            = F ! add Buoyancy     to momentum equation
       SP%addGravity             = F ! add Gravity      to momentum equation

       if (SP%coupled_time_step) then
         call couple_time_step(SP%BMC%VS%T%TMP  ,SP%coupled)
         call couple_time_step(SP%BMC%VS%U%TMP  ,SP%coupled)
         call couple_time_step(SP%BMC%VS%P%TMP  ,SP%coupled)
         call couple_time_step(SP%BMC%VS%B%TMP  ,SP%coupled)
         call couple_time_step(SP%BMC%VS%B0%TMP ,SP%coupled)
         call couple_time_step(SP%BMC%VS%phi%TMP,SP%coupled)
       endif
       call export_import_SS(SP%BMC%VS)
       if (.not.SP%finite_Rem) SP%DP%Rem = 1.0_cp
       if (SP%coupled%n_step_stop.lt.1) stop 'Error: coupled%n_step_stop<1 in sim_params.f90'
      end subroutine

     subroutine init_sim_params_copy(SP,SP_in)
       implicit none
       type(sim_params),intent(inout) :: SP
       type(sim_params),intent(in) :: SP_in
       SP%restart_all            = SP_in%restart_all
       SP%stop_before_solve      = SP_in%stop_before_solve
       SP%stop_after_mesh_export = SP_in%stop_after_mesh_export
       SP%post_process           = SP_in%post_process
       SP%skip_solver_loop       = SP_in%skip_solver_loop
       SP%post_process_only      = SP_in%post_process_only
       SP%coupled_time_step      = SP_in%coupled_time_step
       SP%finite_Rem             = SP_in%finite_Rem
       SP%include_vacuum         = SP_in%include_vacuum
       SP%matrix_based           = SP_in%matrix_based
       SP%addJCrossB             = SP_in%addJCrossB
       SP%addBuoyancy            = SP_in%addBuoyancy
       SP%addGravity             = SP_in%addGravity
       SP%add_Q2D_JCrossB        = SP_in%add_Q2D_JCrossB
       call init(SP%EL,     SP_in%EL)
       call init(SP%BMC,    SP_in%BMC)
       call init(SP%DP,     SP_in%DP)
       call init(SP%coupled,SP_in%coupled)
       call init(SP%DMR,    SP_in%DMR)
       call init(SP%MQP,    SP_in%MQP)
      end subroutine

     subroutine export_SP(SP,un)
       implicit none
       type(sim_params),intent(inout) :: SP
       integer,intent(in) :: un
       write(un,*) 'restart_all            = ',SP%restart_all
       write(un,*) 'stop_before_solve      = ',SP%stop_before_solve
       write(un,*) 'stop_after_mesh_export = ',SP%stop_after_mesh_export
       write(un,*) 'post_process           = ',SP%post_process
       write(un,*) 'skip_solver_loop       = ',SP%skip_solver_loop
       write(un,*) 'post_process_only      = ',SP%post_process_only
       write(un,*) 'coupled_time_step      = ',SP%coupled_time_step
       write(un,*) 'finite_Rem             = ',SP%finite_Rem
       write(un,*) 'include_vacuum         = ',SP%include_vacuum
       write(un,*) 'matrix_based           = ',SP%matrix_based
       write(un,*) 'addJCrossB             = ',SP%addJCrossB
       write(un,*) 'addBuoyancy            = ',SP%addBuoyancy
       write(un,*) 'addGravity             = ',SP%addGravity
       write(un,*) 'add_Q2D_JCrossB        = ',SP%add_Q2D_JCrossB
       call export(SP%EL,un)
       call export(SP%BMC,un)
       call export(SP%DP,un)
       call export(SP%DMR,un)
       call export(SP%MQP,un)
       call export(SP%coupled,un)
      end subroutine

     subroutine export_SP_wrapper(SP,dir,name)
       implicit none
       type(sim_params),intent(inout) :: SP
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call export(SP,un)
       call close_and_message(un,dir,name)
      end subroutine

     end module