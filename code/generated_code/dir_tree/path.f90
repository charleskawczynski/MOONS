       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module path_mod
       use string_mod
       use datatype_conversion_mod
       use IO_tools_mod
       use dir_manip_mod
       implicit none

       private
       public :: path
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings

       interface init;                   module procedure init_copy_path;              end interface
       interface delete;                 module procedure delete_path;                 end interface
       interface display;                module procedure display_path;                end interface
       interface display_short;          module procedure display_short_path;          end interface
       interface display;                module procedure display_wrap_path;           end interface
       interface print;                  module procedure print_path;                  end interface
       interface print_short;            module procedure print_short_path;            end interface
       interface export;                 module procedure export_path;                 end interface
       interface export_primitives;      module procedure export_primitives_path;      end interface
       interface import;                 module procedure import_path;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_path;end interface
       interface export_structured;      module procedure export_structured_D_path;    end interface
       interface import_structured;      module procedure import_structured_D_path;    end interface
       interface import_primitives;      module procedure import_primitives_path;      end interface
       interface export;                 module procedure export_wrap_path;            end interface
       interface import;                 module procedure import_wrap_path;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_path;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_path;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_path;      end interface

       type path
         type(string) :: absolute
         type(string) :: relative
       end type

       contains

       subroutine init_copy_path(this,that)
         implicit none
         type(path),intent(inout) :: this
         type(path),intent(in) :: that
         call delete(this)
         call init(this%absolute,that%absolute)
         call init(this%relative,that%relative)
       end subroutine

       subroutine delete_path(this)
         implicit none
         type(path),intent(inout) :: this
         call delete(this%absolute)
         call delete(this%relative)
       end subroutine

       subroutine display_path(this,un)
         implicit none
         type(path),intent(in) :: this
         integer,intent(in) :: un
         call display(this%absolute,un)
         call display(this%relative,un)
       end subroutine

       subroutine display_short_path(this,un)
         implicit none
         type(path),intent(in) :: this
         integer,intent(in) :: un
         call display(this%absolute,un)
         call display(this%relative,un)
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
         call export(this%absolute,un)
         call export(this%relative,un)
       end subroutine

       subroutine import_path(this,un)
         implicit none
         type(path),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%absolute,un)
         call import(this%relative,un)
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
         if (.false.) then
           write(*,*) dir
         endif
         call set_IO_dir(this%relative,dir//'relative'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_path(this,dir)
         implicit none
         type(path),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%relative,dir//'relative'//fortran_PS)
       end subroutine

       subroutine export_folder_structure_path(this,dir)
         implicit none
         type(path),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         call export_structured(this%relative,dir//'relative'//fortran_PS)
       end subroutine

       subroutine export_structured_D_path(this,dir)
         implicit none
         type(path),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%relative,dir//'relative'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_path(this,dir)
         implicit none
         type(path),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         call import_structured(this%relative,dir//'relative'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_path(this)
         implicit none
         type(path),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module