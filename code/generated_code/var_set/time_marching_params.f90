       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module time_marching_params_mod
       use current_precision_mod
       use IO_tools_mod
       use RK_Params_mod
       use string_mod
       implicit none

       private
       public :: time_marching_params
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_time_marching_params;           end interface
       interface delete; module procedure delete_time_marching_params;         end interface
       interface display;module procedure display_time_marching_params;        end interface
       interface display;module procedure display_wrapper_time_marching_params;end interface
       interface print;  module procedure print_time_marching_params;          end interface
       interface export; module procedure export_time_marching_params;         end interface
       interface import; module procedure import_time_marching_params;         end interface
       interface export; module procedure export_wrapper_time_marching_params; end interface
       interface import; module procedure import_wrapper_time_marching_params; end interface

       type time_marching_params
         type(rk_params) :: rkp
         integer :: multistep_iter = 0
         integer :: un = 0
         integer(li) :: n_step = 0
         integer(li) :: n_step_stop = 0
         integer(li) :: n_step_start = 0
         real(cp) :: t = 0.0_cp
         real(cp) :: c_max = 0.0_cp
         real(cp) :: t_final = 0.0_cp
         real(cp) :: dt = 0.0_cp
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_time_marching_params(this,that)
         implicit none
         type(time_marching_params),intent(inout) :: this
         type(time_marching_params),intent(in) :: that
         call delete(this)
         call init(this%rkp,that%rkp)
         this%multistep_iter = that%multistep_iter
         this%un = that%un
         this%n_step = that%n_step
         this%n_step_stop = that%n_step_stop
         this%n_step_start = that%n_step_start
         this%t = that%t
         this%c_max = that%c_max
         this%t_final = that%t_final
         this%dt = that%dt
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_time_marching_params(this)
         implicit none
         type(time_marching_params),intent(inout) :: this
         call delete(this%rkp)
         this%multistep_iter = 0
         this%un = 0
         this%n_step = 0
         this%n_step_stop = 0
         this%n_step_start = 0
         this%t = 0.0_cp
         this%c_max = 0.0_cp
         this%t_final = 0.0_cp
         this%dt = 0.0_cp
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_time_marching_params(this,un)
         implicit none
         type(time_marching_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- time_marching_params'
         call display(this%rkp,un)
         write(un,*) 'multistep_iter = ',this%multistep_iter
         write(un,*) 'un             = ',this%un
         write(un,*) 'n_step         = ',this%n_step
         write(un,*) 'n_step_stop    = ',this%n_step_stop
         write(un,*) 'n_step_start   = ',this%n_step_start
         write(un,*) 't              = ',this%t
         write(un,*) 'c_max          = ',this%c_max
         write(un,*) 't_final        = ',this%t_final
         write(un,*) 'dt             = ',this%dt
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine print_time_marching_params(this)
         implicit none
         type(time_marching_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_time_marching_params(this,un)
         implicit none
         type(time_marching_params),intent(in) :: this
         integer,intent(in) :: un
         call export(this%rkp,un)
         write(un,*) 'multistep_iter  = ';write(un,*) this%multistep_iter
         write(un,*) 'un              = ';write(un,*) this%un
         write(un,*) 'n_step          = ';write(un,*) this%n_step
         write(un,*) 'n_step_stop     = ';write(un,*) this%n_step_stop
         write(un,*) 'n_step_start    = ';write(un,*) this%n_step_start
         write(un,*) 't               = ';write(un,*) this%t
         write(un,*) 'c_max           = ';write(un,*) this%c_max
         write(un,*) 't_final         = ';write(un,*) this%t_final
         write(un,*) 'dt              = ';write(un,*) this%dt
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_time_marching_params(this,un)
         implicit none
         type(time_marching_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%rkp,un)
         read(un,*); read(un,*) this%multistep_iter
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%n_step
         read(un,*); read(un,*) this%n_step_stop
         read(un,*); read(un,*) this%n_step_start
         read(un,*); read(un,*) this%t
         read(un,*); read(un,*) this%c_max
         read(un,*); read(un,*) this%t_final
         read(un,*); read(un,*) this%dt
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine display_wrapper_time_marching_params(this,dir,name)
         implicit none
         type(time_marching_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_time_marching_params(this,dir,name)
         implicit none
         type(time_marching_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_time_marching_params(this,dir,name)
         implicit none
         type(time_marching_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module