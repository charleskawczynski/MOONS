       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module time_marching_params_mod
       use current_precision_mod
       use IO_tools_mod
       use RK_params_mod
       use string_mod
       implicit none

       private
       public :: time_marching_params
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_time_marching_params;      end interface
       interface delete;       module procedure delete_time_marching_params;         end interface
       interface display;      module procedure display_time_marching_params;        end interface
       interface display_short;module procedure display_short_time_marching_params;  end interface
       interface display;      module procedure display_wrapper_time_marching_params;end interface
       interface print;        module procedure print_time_marching_params;          end interface
       interface print_short;  module procedure print_short_time_marching_params;    end interface
       interface export;       module procedure export_time_marching_params;         end interface
       interface import;       module procedure import_time_marching_params;         end interface
       interface export;       module procedure export_wrapper_time_marching_params; end interface
       interface import;       module procedure import_wrapper_time_marching_params; end interface
       interface export;       module procedure export_DN_time_marching_params;      end interface
       interface import;       module procedure import_DN_time_marching_params;      end interface

       type time_marching_params
         type(RK_Params) :: RKP
         integer :: multistep_iter = 0
         integer :: un = 0
         integer(li) :: n_step = 0
         integer(li) :: n_step_stop = 0
         integer(li) :: n_step_start = 0
         real(cp) :: t = 0.0_cp
         real(cp) :: C_max = 0.0_cp
         real(cp) :: t_final = 0.0_cp
         real(cp) :: dt = 0.0_cp
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_copy_time_marching_params(this,that)
         implicit none
         type(time_marching_params),intent(inout) :: this
         type(time_marching_params),intent(in) :: that
         call delete(this)
         call init(this%RKP,that%RKP)
         this%multistep_iter = that%multistep_iter
         this%un = that%un
         this%n_step = that%n_step
         this%n_step_stop = that%n_step_stop
         this%n_step_start = that%n_step_start
         this%t = that%t
         this%C_max = that%C_max
         this%t_final = that%t_final
         this%dt = that%dt
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_time_marching_params(this)
         implicit none
         type(time_marching_params),intent(inout) :: this
         call delete(this%RKP)
         this%multistep_iter = 0
         this%un = 0
         this%n_step = 0
         this%n_step_stop = 0
         this%n_step_start = 0
         this%t = 0.0_cp
         this%C_max = 0.0_cp
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
         call display(this%RKP,un)
         write(un,*) 'multistep_iter = ',this%multistep_iter
         write(un,*) 'un             = ',this%un
         write(un,*) 'n_step         = ',this%n_step
         write(un,*) 'n_step_stop    = ',this%n_step_stop
         write(un,*) 'n_step_start   = ',this%n_step_start
         write(un,*) 't              = ',this%t
         write(un,*) 'C_max          = ',this%C_max
         write(un,*) 't_final        = ',this%t_final
         write(un,*) 'dt             = ',this%dt
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_short_time_marching_params(this,un)
         implicit none
         type(time_marching_params),intent(in) :: this
         integer,intent(in) :: un
         call display(this%RKP,un)
         write(un,*) 'multistep_iter = ',this%multistep_iter
         write(un,*) 'un             = ',this%un
         write(un,*) 'n_step         = ',this%n_step
         write(un,*) 'n_step_stop    = ',this%n_step_stop
         write(un,*) 'n_step_start   = ',this%n_step_start
         write(un,*) 't              = ',this%t
         write(un,*) 'C_max          = ',this%C_max
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

       subroutine print_short_time_marching_params(this)
         implicit none
         type(time_marching_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_time_marching_params(this,un)
         implicit none
         type(time_marching_params),intent(in) :: this
         integer,intent(in) :: un
         call export(this%RKP,un)
         write(un,*) 'multistep_iter  = ';write(un,*) this%multistep_iter
         write(un,*) 'un              = ';write(un,*) this%un
         write(un,*) 'n_step          = ';write(un,*) this%n_step
         write(un,*) 'n_step_stop     = ';write(un,*) this%n_step_stop
         write(un,*) 'n_step_start    = ';write(un,*) this%n_step_start
         write(un,*) 't               = ';write(un,*) this%t
         write(un,*) 'C_max           = ';write(un,*) this%C_max
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
         call import(this%RKP,un)
         read(un,*); read(un,*) this%multistep_iter
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%n_step
         read(un,*); read(un,*) this%n_step_stop
         read(un,*); read(un,*) this%n_step_start
         read(un,*); read(un,*) this%t
         read(un,*); read(un,*) this%C_max
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
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine export_DN_time_marching_params(this)
         implicit none
         type(time_marching_params),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_time_marching_params(this)
         implicit none
         type(time_marching_params),intent(inout) :: this
         type(string) :: dir,name
         integer :: un
         call init(dir,this%dir)
         call init(name,this%name)
         un = open_to_read(str(dir),str(name))
         call import(this,un)
         call delete(dir)
         call delete(name)
         close(un)
       end subroutine

       end module