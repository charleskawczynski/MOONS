       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module simple_int_tensor_mod
       use string_mod
       use datatype_conversion_mod
       use IO_tools_mod
       use dir_manip_mod
       implicit none

       private
       public :: simple_int_tensor
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings

       interface init;                   module procedure init_copy_simple_int_tensor;              end interface
       interface delete;                 module procedure delete_simple_int_tensor;                 end interface
       interface display;                module procedure display_simple_int_tensor;                end interface
       interface display_short;          module procedure display_short_simple_int_tensor;          end interface
       interface display;                module procedure display_wrap_simple_int_tensor;           end interface
       interface print;                  module procedure print_simple_int_tensor;                  end interface
       interface print_short;            module procedure print_short_simple_int_tensor;            end interface
       interface export;                 module procedure export_simple_int_tensor;                 end interface
       interface export_primitives;      module procedure export_primitives_simple_int_tensor;      end interface
       interface import;                 module procedure import_simple_int_tensor;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_simple_int_tensor;end interface
       interface export_structured;      module procedure export_structured_D_simple_int_tensor;    end interface
       interface import_structured;      module procedure import_structured_D_simple_int_tensor;    end interface
       interface import_primitives;      module procedure import_primitives_simple_int_tensor;      end interface
       interface export;                 module procedure export_wrap_simple_int_tensor;            end interface
       interface import;                 module procedure import_wrap_simple_int_tensor;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_simple_int_tensor;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_simple_int_tensor;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_simple_int_tensor;      end interface

       type simple_int_tensor
         integer,dimension(3) :: eye = 0
       end type

       contains

       subroutine init_copy_simple_int_tensor(this,that)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         type(simple_int_tensor),intent(in) :: that
         call delete(this)
         this%eye = that%eye
       end subroutine

       subroutine delete_simple_int_tensor(this)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         this%eye = 0
       end subroutine

       subroutine display_simple_int_tensor(this,un)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'eye = ',this%eye
       end subroutine

       subroutine display_short_simple_int_tensor(this,un)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'eye = ',this%eye
       end subroutine

       subroutine display_wrap_simple_int_tensor(this,dir,name)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_simple_int_tensor(this)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_simple_int_tensor(this)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_simple_int_tensor(this,un)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
       end subroutine

       subroutine import_simple_int_tensor(this,un)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
       end subroutine

       subroutine export_primitives_simple_int_tensor(this,un)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'eye  = ';write(un,*) this%eye
       end subroutine

       subroutine import_primitives_simple_int_tensor(this,un)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%eye
       end subroutine

       subroutine export_wrap_simple_int_tensor(this,dir,name)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_simple_int_tensor(this,dir,name)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_simple_int_tensor(this,dir)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_simple_int_tensor(this,dir)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_folder_structure_simple_int_tensor(this,dir)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
       end subroutine

       subroutine export_structured_D_simple_int_tensor(this,dir)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_simple_int_tensor(this,dir)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_simple_int_tensor(this)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module