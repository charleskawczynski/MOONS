       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module path_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: path
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_path;          end interface
       interface delete;           module procedure delete_path;             end interface
       interface display;          module procedure display_path;            end interface
       interface display_short;    module procedure display_short_path;      end interface
       interface display;          module procedure display_wrap_path;       end interface
       interface print;            module procedure print_path;              end interface
       interface print_short;      module procedure print_short_path;        end interface
       interface export;           module procedure export_path;             end interface
       interface export_primitives;module procedure export_primitives_path;  end interface
       interface import;           module procedure import_path;             end interface
       interface export_structured;module procedure export_structured_D_path;end interface
       interface import_structured;module procedure import_structured_D_path;end interface
       interface import_primitives;module procedure import_primitives_path;  end interface
       interface export;           module procedure export_wrap_path;        end interface
       interface import;           module procedure import_wrap_path;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_path;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_path;        end interface
       interface suppress_warnings;module procedure suppress_warnings_path;  end interface

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

       subroutine display_wrap_path(this,dir,name)
         implicit none
         type(path),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
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

       subroutine export_primitives_path(this,un)
         implicit none
         type(path),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_path(this,un)
         implicit none
         type(path),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_wrap_path(this,dir,name)
         implicit none
         type(path),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_path(this,dir,name)
         implicit none
         type(path),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_path(this,dir)
         implicit none
         type(path),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) write(*,*) dir
       end subroutine

       subroutine make_IO_dir_path(this,dir)
         implicit none
         type(path),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_structured_D_path(this,dir)
         implicit none
         type(path),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_path(this,dir)
         implicit none
         type(path),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_path(this)
         implicit none
         type(path),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module