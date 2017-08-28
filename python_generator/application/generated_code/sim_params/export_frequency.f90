       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_frequency_mod
       use IO_tools_mod
       use export_frequency_params_mod
       use string_mod
       implicit none

       private
       public :: export_frequency
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_export_frequency;           end interface
       interface delete; module procedure delete_export_frequency;         end interface
       interface display;module procedure display_export_frequency;        end interface
       interface display;module procedure display_wrapper_export_frequency;end interface
       interface print;  module procedure print_export_frequency;          end interface
       interface export; module procedure export_export_frequency;         end interface
       interface import; module procedure import_export_frequency;         end interface
       interface export; module procedure export_wrapper_export_frequency; end interface
       interface import; module procedure import_wrapper_export_frequency; end interface

       type export_frequency
         type(export_frequency_params) :: info
         type(export_frequency_params) :: unsteady_0d
         type(export_frequency_params) :: unsteady_1d
         type(export_frequency_params) :: unsteady_2d
         type(export_frequency_params) :: unsteady_3d
         type(export_frequency_params) :: final_solution
         type(export_frequency_params) :: restart_files
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_export_frequency(this,that)
         implicit none
         type(export_frequency),intent(inout) :: this
         type(export_frequency),intent(in) :: that
         call delete(this)
         call init(this%info,that%info)
         call init(this%unsteady_0d,that%unsteady_0d)
         call init(this%unsteady_1d,that%unsteady_1d)
         call init(this%unsteady_2d,that%unsteady_2d)
         call init(this%unsteady_3d,that%unsteady_3d)
         call init(this%final_solution,that%final_solution)
         call init(this%restart_files,that%restart_files)
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_export_frequency(this)
         implicit none
         type(export_frequency),intent(inout) :: this
         call delete(this%info)
         call delete(this%unsteady_0d)
         call delete(this%unsteady_1d)
         call delete(this%unsteady_2d)
         call delete(this%unsteady_3d)
         call delete(this%final_solution)
         call delete(this%restart_files)
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_export_frequency(this,un)
         implicit none
         type(export_frequency),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- export_frequency'
         call display(this%info,un)
         call display(this%unsteady_0d,un)
         call display(this%unsteady_1d,un)
         call display(this%unsteady_2d,un)
         call display(this%unsteady_3d,un)
         call display(this%final_solution,un)
         call display(this%restart_files,un)
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine print_export_frequency(this)
         implicit none
         type(export_frequency),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_export_frequency(this,un)
         implicit none
         type(export_frequency),intent(in) :: this
         integer,intent(in) :: un
         call export(this%info,un)
         call export(this%unsteady_0d,un)
         call export(this%unsteady_1d,un)
         call export(this%unsteady_2d,un)
         call export(this%unsteady_3d,un)
         call export(this%final_solution,un)
         call export(this%restart_files,un)
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_export_frequency(this,un)
         implicit none
         type(export_frequency),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%info,un)
         call import(this%unsteady_0d,un)
         call import(this%unsteady_1d,un)
         call import(this%unsteady_2d,un)
         call import(this%unsteady_3d,un)
         call import(this%final_solution,un)
         call import(this%restart_files,un)
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine display_wrapper_export_frequency(this,dir,name)
         implicit none
         type(export_frequency),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_export_frequency(this,dir,name)
         implicit none
         type(export_frequency),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_export_frequency(this,dir,name)
         implicit none
         type(export_frequency),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module