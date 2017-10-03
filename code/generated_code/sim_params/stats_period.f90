       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module stats_period_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: stats_period
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_stats_period;          end interface
       interface delete;           module procedure delete_stats_period;             end interface
       interface display;          module procedure display_stats_period;            end interface
       interface display_short;    module procedure display_short_stats_period;      end interface
       interface display;          module procedure display_wrap_stats_period;       end interface
       interface print;            module procedure print_stats_period;              end interface
       interface print_short;      module procedure print_short_stats_period;        end interface
       interface export;           module procedure export_stats_period;             end interface
       interface export_primitives;module procedure export_primitives_stats_period;  end interface
       interface import;           module procedure import_stats_period;             end interface
       interface export_structured;module procedure export_structured_D_stats_period;end interface
       interface import_structured;module procedure import_structured_D_stats_period;end interface
       interface import_primitives;module procedure import_primitives_stats_period;  end interface
       interface export;           module procedure export_wrap_stats_period;        end interface
       interface import;           module procedure import_wrap_stats_period;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_stats_period;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_stats_period;        end interface
       interface suppress_warnings;module procedure suppress_warnings_stats_period;  end interface

       type stats_period
         real(cp) :: t_start = 0.0_cp
         real(cp) :: t_start_actual = 0.0_cp
         real(cp) :: t_stop = 0.0_cp
         real(cp) :: period = 0.0_cp
         integer :: N_stats_collected = 0
         logical :: compute_stats = .false.
         logical :: define_t_start_actual = .false.
         logical :: t_start_actual_defined = .false.
         logical :: export_stats = .false.
         logical :: exported_stats = .false.
       end type

       contains

       subroutine init_copy_stats_period(this,that)
         implicit none
         type(stats_period),intent(inout) :: this
         type(stats_period),intent(in) :: that
         call delete(this)
         this%t_start = that%t_start
         this%t_start_actual = that%t_start_actual
         this%t_stop = that%t_stop
         this%period = that%period
         this%N_stats_collected = that%N_stats_collected
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
         this%N_stats_collected = 0
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
         write(un,*) 't_start                = ',this%t_start
         write(un,*) 't_start_actual         = ',this%t_start_actual
         write(un,*) 't_stop                 = ',this%t_stop
         write(un,*) 'period                 = ',this%period
         write(un,*) 'N_stats_collected      = ',this%N_stats_collected
         write(un,*) 'compute_stats          = ',this%compute_stats
         write(un,*) 'define_t_start_actual  = ',this%define_t_start_actual
         write(un,*) 't_start_actual_defined = ',this%t_start_actual_defined
         write(un,*) 'export_stats           = ',this%export_stats
         write(un,*) 'exported_stats         = ',this%exported_stats
       end subroutine

       subroutine display_short_stats_period(this,un)
         implicit none
         type(stats_period),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 't_start                = ',this%t_start
         write(un,*) 't_start_actual         = ',this%t_start_actual
         write(un,*) 't_stop                 = ',this%t_stop
         write(un,*) 'period                 = ',this%period
         write(un,*) 'N_stats_collected      = ',this%N_stats_collected
         write(un,*) 'compute_stats          = ',this%compute_stats
         write(un,*) 'define_t_start_actual  = ',this%define_t_start_actual
         write(un,*) 't_start_actual_defined = ',this%t_start_actual_defined
         write(un,*) 'export_stats           = ',this%export_stats
         write(un,*) 'exported_stats         = ',this%exported_stats
       end subroutine

       subroutine display_wrap_stats_period(this,dir,name)
         implicit none
         type(stats_period),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_stats_period(this)
         implicit none
         type(stats_period),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_stats_period(this)
         implicit none
         type(stats_period),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_stats_period(this,un)
         implicit none
         type(stats_period),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 't_start                 = ';write(un,*) this%t_start
         write(un,*) 't_start_actual          = ';write(un,*) this%t_start_actual
         write(un,*) 't_stop                  = ';write(un,*) this%t_stop
         write(un,*) 'period                  = ';write(un,*) this%period
         write(un,*) 'N_stats_collected       = ';write(un,*) this%N_stats_collected
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
         read(un,*); read(un,*) this%N_stats_collected
         read(un,*); read(un,*) this%compute_stats
         read(un,*); read(un,*) this%define_t_start_actual
         read(un,*); read(un,*) this%t_start_actual_defined
         read(un,*); read(un,*) this%export_stats
         read(un,*); read(un,*) this%exported_stats
       end subroutine

       subroutine export_primitives_stats_period(this,un)
         implicit none
         type(stats_period),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 't_start                 = ';write(un,*) this%t_start
         write(un,*) 't_start_actual          = ';write(un,*) this%t_start_actual
         write(un,*) 't_stop                  = ';write(un,*) this%t_stop
         write(un,*) 'period                  = ';write(un,*) this%period
         write(un,*) 'N_stats_collected       = ';write(un,*) this%N_stats_collected
         write(un,*) 'compute_stats           = ';write(un,*) this%compute_stats
         write(un,*) 'define_t_start_actual   = ';write(un,*) this%define_t_start_actual
         write(un,*) 't_start_actual_defined  = ';write(un,*) this%t_start_actual_defined
         write(un,*) 'export_stats            = ';write(un,*) this%export_stats
         write(un,*) 'exported_stats          = ';write(un,*) this%exported_stats
       end subroutine

       subroutine import_primitives_stats_period(this,un)
         implicit none
         type(stats_period),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%t_start
         read(un,*); read(un,*) this%t_start_actual
         read(un,*); read(un,*) this%t_stop
         read(un,*); read(un,*) this%period
         read(un,*); read(un,*) this%N_stats_collected
         read(un,*); read(un,*) this%compute_stats
         read(un,*); read(un,*) this%define_t_start_actual
         read(un,*); read(un,*) this%t_start_actual_defined
         read(un,*); read(un,*) this%export_stats
         read(un,*); read(un,*) this%exported_stats
       end subroutine

       subroutine export_wrap_stats_period(this,dir,name)
         implicit none
         type(stats_period),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_stats_period(this,dir,name)
         implicit none
         type(stats_period),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_stats_period(this,dir)
         implicit none
         type(stats_period),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_stats_period(this,dir)
         implicit none
         type(stats_period),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir(dir)
       end subroutine

       subroutine export_structured_D_stats_period(this,dir)
         implicit none
         type(stats_period),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Exporting stats_period structured'
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_stats_period(this,dir)
         implicit none
         type(stats_period),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Importing stats_period structured'
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_stats_period(this)
         implicit none
         type(stats_period),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module