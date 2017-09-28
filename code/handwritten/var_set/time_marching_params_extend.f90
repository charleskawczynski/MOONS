       module time_marching_params_extend_mod
       use time_marching_params_mod
       use current_precision_mod
       use string_mod
       use RK_Params_mod
       use RK_Params_extend_mod
       use IO_tools_mod
       implicit none

       private
       public :: init
       public :: iterate_step
       public :: iterate_RK
       public :: assign_RK_stage
       public :: couple_time_step
       public :: prolongate
       public :: update_dt

       interface init;              module procedure init_TMP;                end interface
       interface iterate_step;      module procedure iterate_step_TMP;        end interface
       interface iterate_RK;        module procedure iterate_RK_TMP;          end interface
       interface assign_RK_stage;   module procedure assign_RK_stage_TMP;     end interface

       interface couple_time_step;  module procedure couple_time_step_TMP;    end interface

       interface prolongate;        module procedure prolongate_TMP;          end interface
       interface update_dt;         module procedure update_dt_TMP;           end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_TMP(TMP,n_stages,active,multistep_iter,t_final,dt)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         integer,intent(in) :: n_stages,multistep_iter
         logical,intent(in) :: active
         real(cp),intent(in) :: t_final,dt
         call init(TMP%RKP,n_stages,active)
         TMP%n_step_start = 0
         TMP%n_step = 0
         TMP%multistep_iter = multistep_iter
         TMP%t = 0.0_cp
         TMP%C_max = 0.1_cp
         TMP%TS%dt = dt
         TMP%TS%t_final = t_final
         TMP%n_step_stop = int(t_final/dt,li)
         if (TMP%n_step_stop.lt.1) then
           stop 'Error: TMP%n_step_stop<1 in time_marching_params.f90'
         endif
       end subroutine

       subroutine iterate_step_TMP(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         TMP%n_step = TMP%n_step + 1
       end subroutine

       subroutine iterate_RK_TMP(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         call update_time(TMP%RKP,TMP%t,TMP%TS%dt)
       end subroutine

       subroutine assign_RK_stage_TMP(TMP,RK_stage)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         integer,intent(in) :: RK_stage
         call assign_stage(TMP%RKP,RK_stage)
       end subroutine

       subroutine couple_time_step_TMP(TMP,coupled)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         type(time_marching_params),intent(in) :: coupled
         TMP%t = coupled%t
         TMP%multistep_iter = 1
         TMP%TS%t_final = coupled%TS%t_final
         TMP%TS%dt = coupled%TS%dt
         TMP%n_step_start = coupled%n_step_start
         TMP%n_step_stop = coupled%n_step_stop
         TMP%C_max = coupled%C_max
         TMP%n_step = coupled%n_step
       end subroutine

       subroutine prolongate_TMP(TMP,dt_reduction_factor)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         real(cp),intent(in) :: dt_reduction_factor
         TMP%TS%dt = TMP%TS%dt*dt_reduction_factor
         TMP%C_max = TMP%C_max*dt_reduction_factor
         TMP%n_step_stop = TMP%n_step_stop*ceiling(1.0_cp/dt_reduction_factor)
       end subroutine

       subroutine update_dt_TMP(TMP,dt)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         real(cp),intent(in) :: dt
         TMP%TS%dt = dt
         TMP%n_step_stop = ceiling(TMP%TS%t_final/TMP%TS%dt)
       end subroutine

       end module