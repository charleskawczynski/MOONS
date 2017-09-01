       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module path_mod
       use IO_tools_mod
       use string_mod
       implicit none

       private
       public :: path
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_path;      end interface
       interface delete;       module procedure delete_path;         end interface
       interface display;      module procedure display_path;        end interface
       interface display_short;module procedure display_short_path;  end interface
       interface display;      module procedure display_wrapper_path;end interface
       interface print;        module procedure print_path;          end interface
       interface print_short;  module procedure print_short_path;    end interface
       interface export;       module procedure export_path;         end interface
       interface import;       module procedure import_path;         end interface
       interface export;       module procedure export_wrapper_path; end interface
       interface import;       module procedure import_wrapper_path; end interface

       type path
         type(string) :: a
         type(string) :: r
       end type

       contains

       subroutine init_copy_path(this,that)
         implicit none
         type(path),intent(inout) :: this
         type(path),intent(in) :: that
         call delete(this)
         call init(this%a,that%a)
         call init(this%r,that%r)
       end subroutine

       subroutine delete_path(this)
         implicit none
         type(path),intent(inout) :: this
         call delete(this%a)
         call delete(this%r)
       end subroutine

       subroutine display_path(this,un)
         implicit none
         type(path),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- path'
         call display(this%a,un)
         call display(this%r,un)
       end subroutine

       subroutine display_short_path(this,un)
         implicit none
         type(path),intent(in) :: this
         integer,intent(in) :: un
         call display(this%a,un)
         call display(this%r,un)
       end subroutine

       subroutine print_path(this)
         implicit none
         type(path),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_path(this)
         implicit none
         type(path),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_path(this,un)
         implicit none
         type(path),intent(in) :: this
         integer,intent(in) :: un
         call export(this%a,un)
         call export(this%r,un)
       end subroutine

       subroutine import_path(this,un)
         implicit none
         type(path),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%a,un)
         call import(this%r,un)
       end subroutine

       subroutine display_wrapper_path(this,dir,name)
         implicit none
         type(path),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_path(this,dir,name)
         implicit none
         type(path),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_path(this,dir,name)
         implicit none
         type(path),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module