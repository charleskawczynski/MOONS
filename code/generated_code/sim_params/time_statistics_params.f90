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
       public :: display_short,print_short

       interface init;         module procedure init_copy_ti;    end interface
       interface delete;       module procedure delete_ti;       end interface
       interface display;      module procedure display_ti;      end interface
       interface display_short;module procedure display_short_ti;end interface
       interface display;      module procedure display_wrap_ti; end interface
       interface print;        module procedure print_ti;        end interface
       interface print_short;  module procedure print_short_ti;  end interface
       interface export;       module procedure export_ti;       end interface
       interface import;       module procedure import_ti;       end interface
       interface export;       module procedure export_wrap_ti;  end interface
       interface import;       module procedure import_wrap_ti;  end interface

       type time_statistics_params
         logical :: collect = .false.
         type(stats_period) :: O1_stats
         type(stats_period) :: O2_stats
       end type

       contains

       subroutine init_copy_ti(this,that)
         implicit none
         type(time_statistics_params),intent(inout) :: this
         type(time_statistics_params),intent(in) :: that
         call delete(this)
         this%collect = that%collect
         call init(this%O1_stats,that%O1_stats)
         call init(this%O2_stats,that%O2_stats)
       end subroutine

       subroutine delete_ti(this)
         implicit none
         type(time_statistics_params),intent(inout) :: this
         this%collect = .false.
         call delete(this%O1_stats)
         call delete(this%O2_stats)
       end subroutine

       subroutine display_ti(this,un)
         implicit none
         type(time_statistics_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- time_statistics_params'
         write(un,*) 'collect  = ',this%collect
         call display(this%O1_stats,un)
         call display(this%O2_stats,un)
       end subroutine

       subroutine display_short_ti(this,un)
         implicit none
         type(time_statistics_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'collect  = ',this%collect
         call display(this%O1_stats,un)
         call display(this%O2_stats,un)
       end subroutine

       subroutine print_ti(this)
         implicit none
         type(time_statistics_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_ti(this)
         implicit none
         type(time_statistics_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_ti(this,un)
         implicit none
         type(time_statistics_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'collect   = ';write(un,*) this%collect
         call export(this%O1_stats,un)
         call export(this%O2_stats,un)
       end subroutine

       subroutine import_ti(this,un)
         implicit none
         type(time_statistics_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%collect
         call import(this%O1_stats,un)
         call import(this%O2_stats,un)
       end subroutine

       subroutine display_wrap_ti(this,dir,name)
         implicit none
         type(time_statistics_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_ti(this,dir,name)
         implicit none
         type(time_statistics_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_ti(this,dir,name)
         implicit none
         type(time_statistics_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module