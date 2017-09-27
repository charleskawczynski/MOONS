       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module clock_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: clock
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_clock;        end interface
       interface delete;           module procedure delete_clock;           end interface
       interface display;          module procedure display_clock;          end interface
       interface display_short;    module procedure display_short_clock;    end interface
       interface display;          module procedure display_wrap_clock;     end interface
       interface print;            module procedure print_clock;            end interface
       interface print_short;      module procedure print_short_clock;      end interface
       interface export;           module procedure export_clock;           end interface
       interface export_primitives;module procedure export_primitives_clock;end interface
       interface export_restart;   module procedure export_restart_clock;   end interface
       interface import;           module procedure import_clock;           end interface
       interface import_restart;   module procedure import_restart_clock;   end interface
       interface import_primitives;module procedure import_primitives_clock;end interface
       interface export;           module procedure export_wrap_clock;      end interface
       interface import;           module procedure import_wrap_clock;      end interface
       interface make_restart_dir; module procedure make_restart_dir_clock; end interface
       interface suppress_warnings;module procedure suppress_warnings_clock;end interface

       type clock
         real(cp) :: t_elapsed = 0.0_cp
         real(cp) :: t_elapsed_computational = 0.0_cp
         real(cp) :: t_start_computational = 0.0_cp
         real(cp) :: t_stop_computational = 0.0_cp
         real(cp) :: t_start = 0.0_cp
         real(cp) :: t_stop = 0.0_cp
         integer(li) :: i_start = 0
         integer(li) :: i_stop = 0
         integer(li) :: count_rate = 0
       end type

       contains

       subroutine init_copy_clock(this,that)
         implicit none
         type(clock),intent(inout) :: this
         type(clock),intent(in) :: that
         call delete(this)
         this%t_elapsed = that%t_elapsed
         this%t_elapsed_computational = that%t_elapsed_computational
         this%t_start_computational = that%t_start_computational
         this%t_stop_computational = that%t_stop_computational
         this%t_start = that%t_start
         this%t_stop = that%t_stop
         this%i_start = that%i_start
         this%i_stop = that%i_stop
         this%count_rate = that%count_rate
       end subroutine

       subroutine delete_clock(this)
         implicit none
         type(clock),intent(inout) :: this
         this%t_elapsed = 0.0_cp
         this%t_elapsed_computational = 0.0_cp
         this%t_start_computational = 0.0_cp
         this%t_stop_computational = 0.0_cp
         this%t_start = 0.0_cp
         this%t_stop = 0.0_cp
         this%i_start = 0
         this%i_stop = 0
         this%count_rate = 0
       end subroutine

       subroutine display_clock(this,un)
         implicit none
         type(clock),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 't_elapsed               = ',this%t_elapsed
         write(un,*) 't_elapsed_computational = ',this%t_elapsed_computational
         write(un,*) 't_start_computational   = ',this%t_start_computational
         write(un,*) 't_stop_computational    = ',this%t_stop_computational
         write(un,*) 't_start                 = ',this%t_start
         write(un,*) 't_stop                  = ',this%t_stop
         write(un,*) 'i_start                 = ',this%i_start
         write(un,*) 'i_stop                  = ',this%i_stop
         write(un,*) 'count_rate              = ',this%count_rate
       end subroutine

       subroutine display_short_clock(this,un)
         implicit none
         type(clock),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 't_elapsed               = ',this%t_elapsed
         write(un,*) 't_elapsed_computational = ',this%t_elapsed_computational
         write(un,*) 't_start_computational   = ',this%t_start_computational
         write(un,*) 't_stop_computational    = ',this%t_stop_computational
         write(un,*) 't_start                 = ',this%t_start
         write(un,*) 't_stop                  = ',this%t_stop
         write(un,*) 'i_start                 = ',this%i_start
         write(un,*) 'i_stop                  = ',this%i_stop
         write(un,*) 'count_rate              = ',this%count_rate
       end subroutine

       subroutine display_wrap_clock(this,dir,name)
         implicit none
         type(clock),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_clock(this)
         implicit none
         type(clock),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_clock(this)
         implicit none
         type(clock),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_clock(this,un)
         implicit none
         type(clock),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 't_elapsed                = ';write(un,*) this%t_elapsed
         write(un,*) 't_elapsed_computational  = ';write(un,*) this%t_elapsed_computational
         write(un,*) 't_start_computational    = ';write(un,*) this%t_start_computational
         write(un,*) 't_stop_computational     = ';write(un,*) this%t_stop_computational
         write(un,*) 't_start                  = ';write(un,*) this%t_start
         write(un,*) 't_stop                   = ';write(un,*) this%t_stop
         write(un,*) 'i_start                  = ';write(un,*) this%i_start
         write(un,*) 'i_stop                   = ';write(un,*) this%i_stop
         write(un,*) 'count_rate               = ';write(un,*) this%count_rate
       end subroutine

       subroutine export_clock(this,un)
         implicit none
         type(clock),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 't_elapsed                = ';write(un,*) this%t_elapsed
         write(un,*) 't_elapsed_computational  = ';write(un,*) this%t_elapsed_computational
         write(un,*) 't_start_computational    = ';write(un,*) this%t_start_computational
         write(un,*) 't_stop_computational     = ';write(un,*) this%t_stop_computational
         write(un,*) 't_start                  = ';write(un,*) this%t_start
         write(un,*) 't_stop                   = ';write(un,*) this%t_stop
         write(un,*) 'i_start                  = ';write(un,*) this%i_start
         write(un,*) 'i_stop                   = ';write(un,*) this%i_stop
         write(un,*) 'count_rate               = ';write(un,*) this%count_rate
       end subroutine

       subroutine import_primitives_clock(this,un)
         implicit none
         type(clock),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%t_elapsed
         read(un,*); read(un,*) this%t_elapsed_computational
         read(un,*); read(un,*) this%t_start_computational
         read(un,*); read(un,*) this%t_stop_computational
         read(un,*); read(un,*) this%t_start
         read(un,*); read(un,*) this%t_stop
         read(un,*); read(un,*) this%i_start
         read(un,*); read(un,*) this%i_stop
         read(un,*); read(un,*) this%count_rate
       end subroutine

       subroutine import_clock(this,un)
         implicit none
         type(clock),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%t_elapsed
         read(un,*); read(un,*) this%t_elapsed_computational
         read(un,*); read(un,*) this%t_start_computational
         read(un,*); read(un,*) this%t_stop_computational
         read(un,*); read(un,*) this%t_start
         read(un,*); read(un,*) this%t_stop
         read(un,*); read(un,*) this%i_start
         read(un,*); read(un,*) this%i_stop
         read(un,*); read(un,*) this%count_rate
       end subroutine

       subroutine export_restart_clock(this,dir)
         implicit none
         type(clock),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_restart_clock(this,dir)
         implicit none
         type(clock),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_clock(this,dir,name)
         implicit none
         type(clock),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_clock(this,dir,name)
         implicit none
         type(clock),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_clock(this,dir)
         implicit none
         type(clock),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine suppress_warnings_clock(this)
         implicit none
         type(clock),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module