       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module time_statistics_params_mod
       use IO_tools_mod
       use stats_period_mod
       implicit none

       private
       public :: time_statistics_params
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_time_statistics_params;           end interface
       interface delete; module procedure delete_time_statistics_params;         end interface
       interface display;module procedure display_time_statistics_params;        end interface
       interface display;module procedure display_wrapper_time_statistics_params;end interface
       interface print;  module procedure print_time_statistics_params;          end interface
       interface export; module procedure export_time_statistics_params;         end interface
       interface import; module procedure import_time_statistics_params;         end interface
       interface export; module procedure export_wrapper_time_statistics_params; end interface
       interface import; module procedure import_wrapper_time_statistics_params; end interface

       type time_statistics_params
         logical :: collect = .false.
         type(stats_period) :: o1_stats
         type(stats_period) :: o2_stats
       end type

       contains

       subroutine init_time_statistics_params(this,that)
         implicit none
         type(time_statistics_params),intent(inout) :: this
         type(time_statistics_params),intent(in) :: that
         call delete(this)
         this%collect = that%collect
         call init(this%o1_stats,that%o1_stats)
         call init(this%o2_stats,that%o2_stats)
       end subroutine

       subroutine delete_time_statistics_params(this)
         implicit none
         type(time_statistics_params),intent(inout) :: this
         this%collect = .false.
         call delete(this%o1_stats)
         call delete(this%o2_stats)
       end subroutine

       subroutine display_time_statistics_params(this,un)
         implicit none
         type(time_statistics_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- time_statistics_params'
         write(un,*) 'collect  = ',this%collect
         call display(this%o1_stats,un)
         call display(this%o2_stats,un)
       end subroutine

       subroutine print_time_statistics_params(this)
         implicit none
         type(time_statistics_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_time_statistics_params(this,un)
         implicit none
         type(time_statistics_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'collect   = ';write(un,*) this%collect
         call export(this%o1_stats,un)
         call export(this%o2_stats,un)
       end subroutine

       subroutine import_time_statistics_params(this,un)
         implicit none
         type(time_statistics_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%collect
         call import(this%o1_stats,un)
         call import(this%o2_stats,un)
       end subroutine

       subroutine display_wrapper_time_statistics_params(this,dir,name)
         implicit none
         type(time_statistics_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_time_statistics_params(this,dir,name)
         implicit none
         type(time_statistics_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_time_statistics_params(this,dir,name)
         implicit none
         type(time_statistics_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module