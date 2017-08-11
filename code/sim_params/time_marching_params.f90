       module time_marching_params_mod
       use current_precision_mod
       use string_mod
       use RK_Params_mod
       use IO_tools_mod
       implicit none

       private
       public :: time_marching_params
       public :: init,delete,export,import,display,print
       public :: iterate_step
       public :: import_dt
       public :: iterate_RK
       public :: assign_RK_stage
       public :: couple_time_step
       public :: prolongate
       public :: update_dt

       type time_marching_params
         type(RK_Params) :: RKP            ! Runge-Kutta Parameters
         integer(li) :: n_step = 0         ! nth time step
         integer(li) :: n_step_stop = 0    ! nth time step to stop
         integer(li) :: n_step_start = 0   ! nth time step to start
         integer :: multistep_iter = 0     ! nth time step to start
         real(cp) :: t = 0.0_cp            ! time, or pseudo time
         real(cp) :: C_max = 0.0_cp        ! C_max number
         real(cp) :: t_final = 0.0_cp      ! finale time, or final pseudo time
         real(cp) :: dt = 0.0_cp           ! time step, or pseudo time step
         integer :: un = 0                 ! file unit
         type(string) :: dir,name          ! directory / name
       end type

       interface init;              module procedure init_TMP;                end interface
       interface init;              module procedure init_copy_TMP;           end interface
       interface delete;            module procedure delete_TMP;              end interface
       interface export;            module procedure export_TMP;              end interface
       interface export;            module procedure export_TMP_wrapper;      end interface
       interface export;            module procedure export_TMP_wrapper_name; end interface
       interface import;            module procedure import_TMP;              end interface
       interface import;            module procedure import_TMP_wrapper;      end interface
       interface import;            module procedure import_TMP_wrapper_name; end interface
       interface import_dt;         module procedure import_dt_TMP_wrapper;   end interface
       interface display;           module procedure display_TMP;             end interface
       interface print;             module procedure print_TMP;               end interface
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

       subroutine init_TMP(TMP,n_stages,active,multistep_iter,n_step_stop,dt,dir,name)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         integer,intent(in) :: n_stages,multistep_iter
         logical,intent(in) :: active
         integer(li),intent(in) :: n_step_stop
         real(cp),intent(in) :: dt
         character(len=*),intent(in) :: dir,name
         call init(TMP%RKP,n_stages,active)
         TMP%n_step_start = 0
         TMP%n_step = 0
         TMP%multistep_iter = multistep_iter
         TMP%n_step_stop = n_step_stop
         TMP%t = 0.0_cp
         TMP%C_max = 0.1_cp
         TMP%dt = dt
         TMP%t_final = TMP%dt*real(TMP%n_step_stop,cp)
         call init(TMP%dir,dir)
         call init(TMP%name,name)
       end subroutine

       subroutine init_copy_TMP(TMP,TMP_in)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         type(time_marching_params),intent(in) :: TMP_in
         call init(TMP%RKP,TMP_in%RKP)
         TMP%n_step_start = TMP_in%n_step_start
         TMP%n_step = TMP_in%n_step
         TMP%n_step_stop = TMP_in%n_step_stop
         TMP%multistep_iter = TMP_in%multistep_iter
         TMP%t = TMP_in%t
         TMP%C_max = TMP_in%C_max
         TMP%t_final = TMP_in%t_final
         TMP%dt = TMP_in%dt
         call init(TMP%dir,TMP_in%dir)
         call init(TMP%name,TMP_in%name)
         TMP%un = TMP_in%un
       end subroutine

       subroutine delete_TMP(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         TMP%n_step_start = 0
         TMP%n_step = 0
         TMP%n_step_stop = 1
         TMP%multistep_iter = 0
         TMP%t = 0.0_cp
         TMP%C_max = 0.0_cp
         TMP%t_final = 0.0_cp
         TMP%dt = 10.0_cp**(-10.0_cp)
         call delete(TMP%RKP)
         call delete(TMP%dir)
         call delete(TMP%name)
         TMP%un = 0
       end subroutine

       subroutine export_TMP(TMP,un)
         implicit none
         type(time_marching_params),intent(in) :: TMP
         integer,intent(in) :: un
         write(un,*) ' ------------- time_marching_params ------------- '
         write(un,*) 'multistep_iter = '; write(un,*) TMP%multistep_iter
         write(un,*) 'n_step_start   = '; write(un,*) TMP%n_step_start
         write(un,*) 'n_step         = '; write(un,*) TMP%n_step
         write(un,*) 'n_step_stop    = '; write(un,*) TMP%n_step_stop
         write(un,*) 'C_max          = '; write(un,*) TMP%C_max
         write(un,*) 't              = '; write(un,*) TMP%t
         write(un,*) 't_final        = '; write(un,*) TMP%t_final
         write(un,*) 'dt             = '; write(un,*) TMP%dt
         call export(TMP%RKP,un)
         call export(TMP%dir,un)
         call export(TMP%name,un)
         write(un,*) ' ------------------------------------------------ '
       end subroutine

       subroutine import_TMP(TMP,un)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         integer,intent(in) :: un
         read(un,*);
         read(un,*); read(un,*) TMP%multistep_iter
         read(un,*); read(un,*) TMP%n_step_start
         read(un,*); read(un,*) TMP%n_step
         read(un,*); read(un,*) TMP%n_step_stop
         read(un,*); read(un,*) TMP%C_max
         read(un,*); read(un,*) TMP%t
         read(un,*); read(un,*) TMP%t_final
         read(un,*); read(un,*) TMP%dt
         call import(TMP%RKP,un)
         call import(TMP%dir,un)
         call import(TMP%name,un)
         read(un,*);
       end subroutine

       subroutine export_TMP_wrapper(TMP)
         implicit none
         type(time_marching_params),intent(in) :: TMP
         integer :: un
         un = new_and_open(str(TMP%dir),str(TMP%name))
         call export(TMP,un)
         call close_and_message(un,str(TMP%dir),str(TMP%name))
       end subroutine

       subroutine import_TMP_wrapper(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         integer :: un
         un = open_to_read(str(TMP%dir),str(TMP%name))
         call import(TMP,un)
         ! call close_and_message(un,str(TMP%dir),str(TMP%name))
         close(un)
       end subroutine

       subroutine import_dt_TMP_wrapper(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         type(time_marching_params) :: temp
         call init(temp,TMP)
         call import(temp)
         TMP%dt = temp%dt
         call delete(temp)
       end subroutine

       subroutine export_TMP_wrapper_name(TMP,dir)
         implicit none
         type(time_marching_params),intent(in) :: TMP
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,str(TMP%name))
         call export(TMP,un)
         call close_and_message(un,dir,str(TMP%name))
       end subroutine

       subroutine import_TMP_wrapper_name(TMP,dir)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,str(TMP%name))
         call import(TMP,un)
         call close_and_message(un,dir,str(TMP%name))
       end subroutine

       subroutine display_TMP(TMP,un)
         implicit none
         type(time_marching_params),intent(in) :: TMP
         integer,intent(in) :: un
         call display(TMP%dir,un)
         call display(TMP%name,un)
         call display(TMP%RKP,un)
         write(un,*) 'multistep_iter = ',TMP%multistep_iter
         write(un,*) 'n_step_start   = ',TMP%n_step_start
         write(un,*) 'n_step         = ',TMP%n_step
         write(un,*) 'n_step_stop    = ',TMP%n_step_stop
         write(un,*) 'C_max          = ',TMP%C_max
         write(un,*) 't              = ',TMP%t
         write(un,*) 't_final        = ',TMP%t_final
         write(un,*) 'dt             = ',TMP%dt
       end subroutine

       subroutine print_TMP(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         call display(TMP,6)
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