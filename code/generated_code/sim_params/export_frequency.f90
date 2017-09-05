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
       public :: display_short,print_short

       interface init;         module procedure init_copy_ex;    end interface
       interface delete;       module procedure delete_ex;       end interface
       interface display;      module procedure display_ex;      end interface
       interface display_short;module procedure display_short_ex;end interface
       interface display;      module procedure display_wrap_ex; end interface
       interface print;        module procedure print_ex;        end interface
       interface print_short;  module procedure print_short_ex;  end interface
       interface export;       module procedure export_ex;       end interface
       interface import;       module procedure import_ex;       end interface
       interface export;       module procedure export_wrap_ex;  end interface
       interface import;       module procedure import_wrap_ex;  end interface
       interface export;       module procedure export_DN_ex;    end interface
       interface import;       module procedure import_DN_ex;    end interface

       type export_frequency
         type(export_frequency_params) :: info
         type(export_frequency_params) :: unsteady_0D
         type(export_frequency_params) :: unsteady_1D
         type(export_frequency_params) :: unsteady_2D
         type(export_frequency_params) :: unsteady_3D
         type(export_frequency_params) :: final_solution
         type(export_frequency_params) :: restart_files
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_copy_ex(this,that)
         implicit none
         type(export_frequency),intent(inout) :: this
         type(export_frequency),intent(in) :: that
         call delete(this)
         call init(this%info,that%info)
         call init(this%unsteady_0D,that%unsteady_0D)
         call init(this%unsteady_1D,that%unsteady_1D)
         call init(this%unsteady_2D,that%unsteady_2D)
         call init(this%unsteady_3D,that%unsteady_3D)
         call init(this%final_solution,that%final_solution)
         call init(this%restart_files,that%restart_files)
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_ex(this)
         implicit none
         type(export_frequency),intent(inout) :: this
         call delete(this%info)
         call delete(this%unsteady_0D)
         call delete(this%unsteady_1D)
         call delete(this%unsteady_2D)
         call delete(this%unsteady_3D)
         call delete(this%final_solution)
         call delete(this%restart_files)
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_ex(this,un)
         implicit none
         type(export_frequency),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- export_frequency'
         call display(this%info,un)
         call display(this%unsteady_0D,un)
         call display(this%unsteady_1D,un)
         call display(this%unsteady_2D,un)
         call display(this%unsteady_3D,un)
         call display(this%final_solution,un)
         call display(this%restart_files,un)
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_short_ex(this,un)
         implicit none
         type(export_frequency),intent(in) :: this
         integer,intent(in) :: un
         call display(this%info,un)
         call display(this%unsteady_0D,un)
         call display(this%unsteady_1D,un)
         call display(this%unsteady_2D,un)
         call display(this%unsteady_3D,un)
         call display(this%final_solution,un)
         call display(this%restart_files,un)
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine print_ex(this)
         implicit none
         type(export_frequency),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_ex(this)
         implicit none
         type(export_frequency),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_ex(this,un)
         implicit none
         type(export_frequency),intent(in) :: this
         integer,intent(in) :: un
         call export(this%info,un)
         call export(this%unsteady_0D,un)
         call export(this%unsteady_1D,un)
         call export(this%unsteady_2D,un)
         call export(this%unsteady_3D,un)
         call export(this%final_solution,un)
         call export(this%restart_files,un)
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_ex(this,un)
         implicit none
         type(export_frequency),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%info,un)
         call import(this%unsteady_0D,un)
         call import(this%unsteady_1D,un)
         call import(this%unsteady_2D,un)
         call import(this%unsteady_3D,un)
         call import(this%final_solution,un)
         call import(this%restart_files,un)
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine display_wrap_ex(this,dir,name)
         implicit none
         type(export_frequency),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_ex(this,dir,name)
         implicit none
         type(export_frequency),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_ex(this,dir,name)
         implicit none
         type(export_frequency),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine export_DN_ex(this)
         implicit none
         type(export_frequency),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_ex(this)
         implicit none
         type(export_frequency),intent(inout) :: this
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