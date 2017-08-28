       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module stats_period_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none

       private
       public :: stats_period
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_stats_period;           end interface
       interface delete; module procedure delete_stats_period;         end interface
       interface display;module procedure display_stats_period;        end interface
       interface display;module procedure display_wrapper_stats_period;end interface
       interface print;  module procedure print_stats_period;          end interface
       interface export; module procedure export_stats_period;         end interface
       interface import; module procedure import_stats_period;         end interface
       interface export; module procedure export_wrapper_stats_period; end interface
       interface import; module procedure import_wrapper_stats_period; end interface

       type stats_period
         real(cp) :: t_start = 0.0_cp
         real(cp) :: t_start_actual = 0.0_cp
         real(cp) :: t_stop = 0.0_cp
         real(cp) :: period = 0.0_cp
         integer :: n_stats_collected = 0
         logical :: compute_stats = .false.
         logical :: define_t_start_actual = .false.
         logical :: t_start_actual_defined = .false.
         logical :: export_stats = .false.
         logical :: exported_stats = .false.
       end type

       contains

       subroutine init_stats_period(this,that)
         implicit none
         type(stats_period),intent(inout) :: this
         type(stats_period),intent(in) :: that
         call delete(this)
         this%t_start = that%t_start
         this%t_start_actual = that%t_start_actual
         this%t_stop = that%t_stop
         this%period = that%period
         this%n_stats_collected = that%n_stats_collected
         this%compute_stats = that%compute_stats
         this%define_t_start_actual = that%define_t_start_actual
         this%t_start_actual_defined = that%t_start_actual_defined
         this%export_stats = that%export_stats
         this%exported_stats = that%exported_stats
       end subroutine

       subroutine delete_stats_period(this)
         implicit none
         type(stats_period),intent(inout) :: this
         this%t_start = 0.0_cp
         this%t_start_actual = 0.0_cp
         this%t_stop = 0.0_cp
         this%period = 0.0_cp
         this%n_stats_collected = 0
         this%compute_stats = .false.
         this%define_t_start_actual = .false.
         this%t_start_actual_defined = .false.
         this%export_stats = .false.
         this%exported_stats = .false.
       end subroutine

       subroutine display_stats_period(this,un)
         implicit none
         type(stats_period),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- stats_period'
         write(un,*) 't_start                = ',this%t_start
         write(un,*) 't_start_actual         = ',this%t_start_actual
         write(un,*) 't_stop                 = ',this%t_stop
         write(un,*) 'period                 = ',this%period
         write(un,*) 'n_stats_collected      = ',this%n_stats_collected
         write(un,*) 'compute_stats          = ',this%compute_stats
         write(un,*) 'define_t_start_actual  = ',this%define_t_start_actual
         write(un,*) 't_start_actual_defined = ',this%t_start_actual_defined
         write(un,*) 'export_stats           = ',this%export_stats
         write(un,*) 'exported_stats         = ',this%exported_stats
       end subroutine

       subroutine print_stats_period(this)
         implicit none
         type(stats_period),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_stats_period(this,un)
         implicit none
         type(stats_period),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 't_start                 = ';write(un,*) this%t_start
         write(un,*) 't_start_actual          = ';write(un,*) this%t_start_actual
         write(un,*) 't_stop                  = ';write(un,*) this%t_stop
         write(un,*) 'period                  = ';write(un,*) this%period
         write(un,*) 'n_stats_collected       = ';write(un,*) this%n_stats_collected
         write(un,*) 'compute_stats           = ';write(un,*) this%compute_stats
         write(un,*) 'define_t_start_actual   = ';write(un,*) this%define_t_start_actual
         write(un,*) 't_start_actual_defined  = ';write(un,*) this%t_start_actual_defined
         write(un,*) 'export_stats            = ';write(un,*) this%export_stats
         write(un,*) 'exported_stats          = ';write(un,*) this%exported_stats
       end subroutine

       subroutine import_stats_period(this,un)
         implicit none
         type(stats_period),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%t_start
         read(un,*); read(un,*) this%t_start_actual
         read(un,*); read(un,*) this%t_stop
         read(un,*); read(un,*) this%period
         read(un,*); read(un,*) this%n_stats_collected
         read(un,*); read(un,*) this%compute_stats
         read(un,*); read(un,*) this%define_t_start_actual
         read(un,*); read(un,*) this%t_start_actual_defined
         read(un,*); read(un,*) this%export_stats
         read(un,*); read(un,*) this%exported_stats
       end subroutine

       subroutine display_wrapper_stats_period(this,dir,name)
         implicit none
         type(stats_period),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_stats_period(this,dir,name)
         implicit none
         type(stats_period),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_stats_period(this,dir,name)
         implicit none
         type(stats_period),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module