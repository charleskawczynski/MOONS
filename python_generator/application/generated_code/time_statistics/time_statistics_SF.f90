       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module time_statistics_sf_mod
       use IO_tools_mod
       use SF_mod
       use probe_mod
       use string_mod
       use time_statistics_params_mod
       implicit none

       private
       public :: time_statistics_sf
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_time_statistics_sf;           end interface
       interface delete; module procedure delete_time_statistics_sf;         end interface
       interface display;module procedure display_time_statistics_sf;        end interface
       interface display;module procedure display_wrapper_time_statistics_sf;end interface
       interface print;  module procedure print_time_statistics_sf;          end interface
       interface export; module procedure export_time_statistics_sf;         end interface
       interface import; module procedure import_time_statistics_sf;         end interface
       interface export; module procedure export_wrapper_time_statistics_sf; end interface
       interface import; module procedure import_wrapper_time_statistics_sf; end interface

       type time_statistics_sf
         type(string) :: dir
         type(string) :: name
         type(sf) :: u_sum
         type(sf) :: u_ave
         type(probe) :: mean_energy
         type(sf) :: rms
         type(time_statistics_params) :: tsp
       end type

       contains

       subroutine init_time_statistics_sf(this,that)
         implicit none
         type(time_statistics_sf),intent(inout) :: this
         type(time_statistics_sf),intent(in) :: that
         call delete(this)
         call init(this%dir,that%dir)
         call init(this%name,that%name)
         call init(this%u_sum,that%u_sum)
         call init(this%u_ave,that%u_ave)
         call init(this%mean_energy,that%mean_energy)
         call init(this%rms,that%rms)
         call init(this%tsp,that%tsp)
       end subroutine

       subroutine delete_time_statistics_sf(this)
         implicit none
         type(time_statistics_sf),intent(inout) :: this
         call delete(this%dir)
         call delete(this%name)
         call delete(this%u_sum)
         call delete(this%u_ave)
         call delete(this%mean_energy)
         call delete(this%rms)
         call delete(this%tsp)
       end subroutine

       subroutine display_time_statistics_sf(this,un)
         implicit none
         type(time_statistics_sf),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- time_statistics_sf'
         call display(this%dir,un)
         call display(this%name,un)
         call display(this%u_sum,un)
         call display(this%u_ave,un)
         call display(this%mean_energy,un)
         call display(this%rms,un)
         call display(this%tsp,un)
       end subroutine

       subroutine print_time_statistics_sf(this)
         implicit none
         type(time_statistics_sf),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_time_statistics_sf(this,un)
         implicit none
         type(time_statistics_sf),intent(in) :: this
         integer,intent(in) :: un
         call export(this%dir,un)
         call export(this%name,un)
         call export(this%u_sum,un)
         call export(this%u_ave,un)
         call export(this%mean_energy,un)
         call export(this%rms,un)
         call export(this%tsp,un)
       end subroutine

       subroutine import_time_statistics_sf(this,un)
         implicit none
         type(time_statistics_sf),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%dir,un)
         call import(this%name,un)
         call import(this%u_sum,un)
         call import(this%u_ave,un)
         call import(this%mean_energy,un)
         call import(this%rms,un)
         call import(this%tsp,un)
       end subroutine

       subroutine display_wrapper_time_statistics_sf(this,dir,name)
         implicit none
         type(time_statistics_sf),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_time_statistics_sf(this,dir,name)
         implicit none
         type(time_statistics_sf),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_time_statistics_sf(this,dir,name)
         implicit none
         type(time_statistics_sf),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module