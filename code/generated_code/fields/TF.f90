       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module TF_mod
       use IO_tools_mod
       use VF_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: TF
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_TF;          end interface
       interface delete;           module procedure delete_TF;             end interface
       interface display;          module procedure display_TF;            end interface
       interface display_short;    module procedure display_short_TF;      end interface
       interface display;          module procedure display_wrap_TF;       end interface
       interface print;            module procedure print_TF;              end interface
       interface print_short;      module procedure print_short_TF;        end interface
       interface export;           module procedure export_TF;             end interface
       interface export_primitives;module procedure export_primitives_TF;  end interface
       interface import;           module procedure import_TF;             end interface
       interface export_structured;module procedure export_structured_D_TF;end interface
       interface import_structured;module procedure import_structured_D_TF;end interface
       interface import_primitives;module procedure import_primitives_TF;  end interface
       interface export;           module procedure export_wrap_TF;        end interface
       interface import;           module procedure import_wrap_TF;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_TF;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_TF;        end interface
       interface suppress_warnings;module procedure suppress_warnings_TF;  end interface

       type TF
         type(VF) :: x
         type(VF) :: y
         type(VF) :: z
       end type

       contains

       subroutine init_copy_TF(this,that)
         implicit none
         type(TF),intent(inout) :: this
         type(TF),intent(in) :: that
         call delete(this)
         call init(this%x,that%x)
         call init(this%y,that%y)
         call init(this%z,that%z)
       end subroutine

       subroutine delete_TF(this)
         implicit none
         type(TF),intent(inout) :: this
         call delete(this%x)
         call delete(this%y)
         call delete(this%z)
       end subroutine

       subroutine display_TF(this,un)
         implicit none
         type(TF),intent(in) :: this
         integer,intent(in) :: un
         call display(this%x,un)
         call display(this%y,un)
         call display(this%z,un)
       end subroutine

       subroutine display_short_TF(this,un)
         implicit none
         type(TF),intent(in) :: this
         integer,intent(in) :: un
         call display(this%x,un)
         call display(this%y,un)
         call display(this%z,un)
       end subroutine

       subroutine display_wrap_TF(this,dir,name)
         implicit none
         type(TF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_TF(this)
         implicit none
         type(TF),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_TF(this)
         implicit none
         type(TF),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_TF(this,un)
         implicit none
         type(TF),intent(in) :: this
         integer,intent(in) :: un
         call export(this%x,un)
         call export(this%y,un)
         call export(this%z,un)
       end subroutine

       subroutine import_TF(this,un)
         implicit none
         type(TF),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%x,un)
         call import(this%y,un)
         call import(this%z,un)
       end subroutine

       subroutine export_primitives_TF(this,un)
         implicit none
         type(TF),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_TF(this,un)
         implicit none
         type(TF),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_wrap_TF(this,dir,name)
         implicit none
         type(TF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         call export(this%x,dir,name//'_x')
         call export(this%y,dir,name//'_y')
         call export(this%z,dir,name//'_z')
       end subroutine

       subroutine import_wrap_TF(this,dir,name)
         implicit none
         type(TF),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         call import(this%x,dir,name//'_x')
         call import(this%y,dir,name//'_y')
         call import(this%z,dir,name//'_z')
       end subroutine

       subroutine set_IO_dir_TF(this,dir)
         implicit none
         type(TF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
       end subroutine

       subroutine make_IO_dir_TF(this,dir)
         implicit none
         type(TF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_structured_D_TF(this,dir)
         implicit none
         type(TF),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_TF(this,dir)
         implicit none
         type(TF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_TF(this)
         implicit none
         type(TF),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module