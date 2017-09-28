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
       public :: import_dt
       public :: iterate_RK
       public :: assign_RK_stage
       public :: couple_time_step
       public :: prolongate
       public :: update_dt

       interface init;              module procedure init_TMP;                end interface
       interface import_dt;         module procedure import_dt_TMP_wrapper;   end interface
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

       subroutine init_TMP(TMP,n_stages,active,multistep_iter,n_step_stop,dt)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         integer,intent(in) :: n_stages,multistep_iter
         logical,intent(in) :: active
         integer(li),intent(in) :: n_step_stop
         real(cp),intent(in) :: dt
         call init(TMP%RKP,n_stages,active)
         TMP%n_step_start = 0
         TMP%n_step = 0
         TMP%multistep_iter = multistep_iter
         TMP%n_step_stop = n_step_stop
         TMP%t = 0.0_cp
         TMP%C_max = 0.1_cp
         TMP%dt = dt
         TMP%t_final = TMP%dt*real(TMP%n_step_stop,cp)
       end subroutine

       subroutine import_dt_TMP_wrapper(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         type(time_marching_params) :: temp
         call init(temp,TMP)
         call import(temp,str(TMP%dir),str(TMP%name))
         TMP%dt = temp%dt
         call delete(temp)
       end subroutine

       subroutine iterate_step_TMP(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         TMP%n_step = TMP%n_step + 1
       end subroutine

       subroutine iterate_RK_TMP(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         call update_time(TMP%RKP,TMP%t,TMP%dt)
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
         TMP%t_final = coupled%t_final
         TMP%dt = coupled%dt
         TMP%n_step_start = coupled%n_step_start
         TMP%n_step_stop = coupled%n_step_stop
         TMP%C_max = coupled%C_max
         TMP%n_step = coupled%n_step
       end subroutine

       subroutine prolongate_TMP(TMP,dt_reduction_factor)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         real(cp),intent(in) :: dt_reduction_factor
         TMP%dt = TMP%dt*dt_reduction_factor
         TMP%C_max = TMP%C_max*dt_reduction_factor
         TMP%n_step_stop = TMP%n_step_stop*ceiling(1.0_cp/dt_reduction_factor)
         TMP%t_final = TMP%dt*real(TMP%n_step_stop,cp)
       end subroutine

       subroutine update_dt_TMP(TMP,dt)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         real(cp),intent(in) :: dt
         TMP%dt = dt
         TMP%n_step_stop = ceiling(TMP%t_final/TMP%dt)
       end subroutine

       end module