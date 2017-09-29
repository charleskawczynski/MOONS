       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module time_marching_params_mod
       use current_precision_mod
       use IO_tools_mod
       use RK_params_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       use time_step_mod
       implicit none

       private
       public :: time_marching_params
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_time_marching_params;          end interface
       interface delete;           module procedure delete_time_marching_params;             end interface
       interface display;          module procedure display_time_marching_params;            end interface
       interface display_short;    module procedure display_short_time_marching_params;      end interface
       interface display;          module procedure display_wrap_time_marching_params;       end interface
       interface print;            module procedure print_time_marching_params;              end interface
       interface print_short;      module procedure print_short_time_marching_params;        end interface
       interface export;           module procedure export_time_marching_params;             end interface
       interface export_primitives;module procedure export_primitives_time_marching_params;  end interface
       interface import;           module procedure import_time_marching_params;             end interface
       interface export_structured;module procedure export_structured_D_time_marching_params;end interface
       interface import_structured;module procedure import_structured_D_time_marching_params;end interface
       interface import_primitives;module procedure import_primitives_time_marching_params;  end interface
       interface export;           module procedure export_wrap_time_marching_params;        end interface
       interface import;           module procedure import_wrap_time_marching_params;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_time_marching_params;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_time_marching_params;        end interface
       interface suppress_warnings;module procedure suppress_warnings_time_marching_params;  end interface

       type time_marching_params
         type(RK_Params) :: RKP
         type(time_step) :: TS
         integer :: multistep_iter = 0
         integer :: un = 0
         integer(li) :: n_step = 0
         integer(li) :: n_step_stop = 0
         integer(li) :: n_step_start = 0
         real(cp) :: t = 0.0_cp
         real(cp) :: C_max = 0.0_cp
       end type

       contains

       subroutine init_copy_time_marching_params(this,that)
         implicit none
         type(time_marching_params),intent(inout) :: this
         type(time_marching_params),intent(in) :: that
         call delete(this)
         call init(this%RKP,that%RKP)
         call init(this%TS,that%TS)
         this%multistep_iter = that%multistep_iter
         this%un = that%un
         this%n_step = that%n_step
         this%n_step_stop = that%n_step_stop
         this%n_step_start = that%n_step_start
         this%t = that%t
         this%C_max = that%C_max
       end subroutine

       subroutine delete_time_marching_params(this)
         implicit none
         type(time_marching_params),intent(inout) :: this
         call delete(this%RKP)
         call delete(this%TS)
         this%multistep_iter = 0
         this%un = 0
         this%n_step = 0
         this%n_step_stop = 0
         this%n_step_start = 0
         this%t = 0.0_cp
         this%C_max = 0.0_cp
       end subroutine

       subroutine display_time_marching_params(this,un)
         implicit none
         type(time_marching_params),intent(in) :: this
         integer,intent(in) :: un
         call display(this%RKP,un)
         call display(this%TS,un)
         write(un,*) 'multistep_iter = ',this%multistep_iter
         write(un,*) 'un             = ',this%un
         write(un,*) 'n_step         = ',this%n_step
         write(un,*) 'n_step_stop    = ',this%n_step_stop
         write(un,*) 'n_step_start   = ',this%n_step_start
         write(un,*) 't              = ',this%t
         write(un,*) 'C_max          = ',this%C_max
       end subroutine

       subroutine display_short_time_marching_params(this,un)
         implicit none
         type(time_marching_params),intent(in) :: this
         integer,intent(in) :: un
         call display(this%RKP,un)
         call display(this%TS,un)
         write(un,*) 'multistep_iter = ',this%multistep_iter
         write(un,*) 'un             = ',this%un
         write(un,*) 'n_step         = ',this%n_step
         write(un,*) 'n_step_stop    = ',this%n_step_stop
         write(un,*) 'n_step_start   = ',this%n_step_start
         write(un,*) 't              = ',this%t
         write(un,*) 'C_max          = ',this%C_max
       end subroutine

       subroutine display_wrap_time_marching_params(this,dir,name)
         implicit none
         type(time_marching_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
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
         call export(this%TS,un)
         write(un,*) 'multistep_iter  = ';write(un,*) this%multistep_iter
         write(un,*) 'un              = ';write(un,*) this%un
         write(un,*) 'n_step          = ';write(un,*) this%n_step
         write(un,*) 'n_step_stop     = ';write(un,*) this%n_step_stop
         write(un,*) 'n_step_start    = ';write(un,*) this%n_step_start
         write(un,*) 't               = ';write(un,*) this%t
         write(un,*) 'C_max           = ';write(un,*) this%C_max
       end subroutine

       subroutine import_time_marching_params(this,un)
         implicit none
         type(time_marching_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%RKP,un)
         call import(this%TS,un)
         read(un,*); read(un,*) this%multistep_iter
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%n_step
         read(un,*); read(un,*) this%n_step_stop
         read(un,*); read(un,*) this%n_step_start
         read(un,*); read(un,*) this%t
         read(un,*); read(un,*) this%C_max
       end subroutine

       subroutine export_primitives_time_marching_params(this,un)
         implicit none
         type(time_marching_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'multistep_iter  = ';write(un,*) this%multistep_iter
         write(un,*) 'un              = ';write(un,*) this%un
         write(un,*) 'n_step          = ';write(un,*) this%n_step
         write(un,*) 'n_step_stop     = ';write(un,*) this%n_step_stop
         write(un,*) 'n_step_start    = ';write(un,*) this%n_step_start
         write(un,*) 't               = ';write(un,*) this%t
         write(un,*) 'C_max           = ';write(un,*) this%C_max
       end subroutine

       subroutine import_primitives_time_marching_params(this,un)
         implicit none
         type(time_marching_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%multistep_iter
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%n_step
         read(un,*); read(un,*) this%n_step_stop
         read(un,*); read(un,*) this%n_step_start
         read(un,*); read(un,*) this%t
         read(un,*); read(un,*) this%C_max
       end subroutine

       subroutine export_wrap_time_marching_params(this,dir,name)
         implicit none
         type(time_marching_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_time_marching_params(this,dir,name)
         implicit none
         type(time_marching_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_time_marching_params(this,dir)
         implicit none
         type(time_marching_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%RKP,dir//'RKP'//fortran_PS)
         call set_IO_dir(this%TS,dir//'TS'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_time_marching_params(this,dir)
         implicit none
         type(time_marching_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%RKP,dir//'RKP'//fortran_PS)
         call make_IO_dir(this%TS,dir//'TS'//fortran_PS)
       end subroutine

       subroutine export_structured_D_time_marching_params(this,dir)
         implicit none
         type(time_marching_params),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Exporting time_marching_params structured'
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_structured(this%RKP,dir//'RKP'//fortran_PS)
         call export_structured(this%TS,dir//'TS'//fortran_PS)
       end subroutine

       subroutine import_structured_D_time_marching_params(this,dir)
         implicit none
         type(time_marching_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Importing time_marching_params structured'
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_structured(this%RKP,dir//'RKP'//fortran_PS)
         call import_structured(this%TS,dir//'TS'//fortran_PS)
       end subroutine

       subroutine suppress_warnings_time_marching_params(this)
         implicit none
         type(time_marching_params),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module