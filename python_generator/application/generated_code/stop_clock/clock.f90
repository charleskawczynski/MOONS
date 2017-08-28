       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module clock_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none

       private
       public :: clock
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_clock;           end interface
       interface delete; module procedure delete_clock;         end interface
       interface display;module procedure display_clock;        end interface
       interface display;module procedure display_wrapper_clock;end interface
       interface print;  module procedure print_clock;          end interface
       interface export; module procedure export_clock;         end interface
       interface import; module procedure import_clock;         end interface
       interface export; module procedure export_wrapper_clock; end interface
       interface import; module procedure import_wrapper_clock; end interface

       type clock
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

       subroutine init_clock(this,that)
         implicit none
         type(clock),intent(inout) :: this
         type(clock),intent(in) :: that
         call delete(this)
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
         write(un,*) ' -------------------- clock'
         write(un,*) 't_elapsed_computational = ',this%t_elapsed_computational
         write(un,*) 't_start_computational   = ',this%t_start_computational
         write(un,*) 't_stop_computational    = ',this%t_stop_computational
         write(un,*) 't_start                 = ',this%t_start
         write(un,*) 't_stop                  = ',this%t_stop
         write(un,*) 'i_start                 = ',this%i_start
         write(un,*) 'i_stop                  = ',this%i_stop
         write(un,*) 'count_rate              = ',this%count_rate
       end subroutine

       subroutine print_clock(this)
         implicit none
         type(clock),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_clock(this,un)
         implicit none
         type(clock),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 't_elapsed_computational  = ';write(un,*) this%t_elapsed_computational
         write(un,*) 't_start_computational    = ';write(un,*) this%t_start_computational
         write(un,*) 't_stop_computational     = ';write(un,*) this%t_stop_computational
         write(un,*) 't_start                  = ';write(un,*) this%t_start
         write(un,*) 't_stop                   = ';write(un,*) this%t_stop
         write(un,*) 'i_start                  = ';write(un,*) this%i_start
         write(un,*) 'i_stop                   = ';write(un,*) this%i_stop
         write(un,*) 'count_rate               = ';write(un,*) this%count_rate
       end subroutine

       subroutine import_clock(this,un)
         implicit none
         type(clock),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%t_elapsed_computational
         read(un,*); read(un,*) this%t_start_computational
         read(un,*); read(un,*) this%t_stop_computational
         read(un,*); read(un,*) this%t_start
         read(un,*); read(un,*) this%t_stop
         read(un,*); read(un,*) this%i_start
         read(un,*); read(un,*) this%i_stop
         read(un,*); read(un,*) this%count_rate
       end subroutine

       subroutine display_wrapper_clock(this,dir,name)
         implicit none
         type(clock),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_clock(this,dir,name)
         implicit none
         type(clock),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_clock(this,dir,name)
         implicit none
         type(clock),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module