       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module dir_group_mod
       use IO_tools_mod
       use path_mod
       implicit none

       private
       public :: dir_group
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_dir_group;           end interface
       interface delete; module procedure delete_dir_group;         end interface
       interface display;module procedure display_dir_group;        end interface
       interface display;module procedure display_wrapper_dir_group;end interface
       interface print;  module procedure print_dir_group;          end interface
       interface export; module procedure export_dir_group;         end interface
       interface import; module procedure import_dir_group;         end interface
       interface export; module procedure export_wrapper_dir_group; end interface
       interface import; module procedure import_wrapper_dir_group; end interface

       type dir_group
         type(path) :: base
         type(path) :: field
         type(path) :: restart
         type(path) :: debug
         type(path) :: energy
         type(path) :: residual
         type(path) :: unsteady
         type(path) :: stats
         type(path) :: bcs
       end type

       contains

       subroutine init_dir_group(this,that)
         implicit none
         type(dir_group),intent(inout) :: this
         type(dir_group),intent(in) :: that
         call delete(this)
         call init(this%base,that%base)
         call init(this%field,that%field)
         call init(this%restart,that%restart)
         call init(this%debug,that%debug)
         call init(this%energy,that%energy)
         call init(this%residual,that%residual)
         call init(this%unsteady,that%unsteady)
         call init(this%stats,that%stats)
         call init(this%bcs,that%bcs)
       end subroutine

       subroutine delete_dir_group(this)
         implicit none
         type(dir_group),intent(inout) :: this
         call delete(this%base)
         call delete(this%field)
         call delete(this%restart)
         call delete(this%debug)
         call delete(this%energy)
         call delete(this%residual)
         call delete(this%unsteady)
         call delete(this%stats)
         call delete(this%bcs)
       end subroutine

       subroutine display_dir_group(this,un)
         implicit none
         type(dir_group),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- dir_group'
         call display(this%base,un)
         call display(this%field,un)
         call display(this%restart,un)
         call display(this%debug,un)
         call display(this%energy,un)
         call display(this%residual,un)
         call display(this%unsteady,un)
         call display(this%stats,un)
         call display(this%bcs,un)
       end subroutine

       subroutine print_dir_group(this)
         implicit none
         type(dir_group),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_dir_group(this,un)
         implicit none
         type(dir_group),intent(in) :: this
         integer,intent(in) :: un
         call export(this%base,un)
         call export(this%field,un)
         call export(this%restart,un)
         call export(this%debug,un)
         call export(this%energy,un)
         call export(this%residual,un)
         call export(this%unsteady,un)
         call export(this%stats,un)
         call export(this%bcs,un)
       end subroutine

       subroutine import_dir_group(this,un)
         implicit none
         type(dir_group),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%base,un)
         call import(this%field,un)
         call import(this%restart,un)
         call import(this%debug,un)
         call import(this%energy,un)
         call import(this%residual,un)
         call import(this%unsteady,un)
         call import(this%stats,un)
         call import(this%bcs,un)
       end subroutine

       subroutine display_wrapper_dir_group(this,dir,name)
         implicit none
         type(dir_group),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_dir_group(this,dir,name)
         implicit none
         type(dir_group),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_dir_group(this,dir,name)
         implicit none
         type(dir_group),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module