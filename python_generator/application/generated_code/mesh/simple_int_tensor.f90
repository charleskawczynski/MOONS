       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module simple_int_tensor_mod
       use IO_tools_mod
       implicit none

       private
       public :: simple_int_tensor
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_simple_int_tensor;           end interface
       interface delete; module procedure delete_simple_int_tensor;         end interface
       interface display;module procedure display_simple_int_tensor;        end interface
       interface display;module procedure display_wrapper_simple_int_tensor;end interface
       interface print;  module procedure print_simple_int_tensor;          end interface
       interface export; module procedure export_simple_int_tensor;         end interface
       interface import; module procedure import_simple_int_tensor;         end interface
       interface export; module procedure export_wrapper_simple_int_tensor; end interface
       interface import; module procedure import_wrapper_simple_int_tensor; end interface

       type simple_int_tensor
         integer,dimension(3) :: eye = 0
       end type

       contains

       subroutine init_simple_int_tensor(this,that)
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
         write(un,*) ' -------------------- simple_int_tensor'
         write(un,*) 'eye = ',this%eye
       end subroutine

       subroutine print_simple_int_tensor(this)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_simple_int_tensor(this,un)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'eye  = ';write(un,*) this%eye
       end subroutine

       subroutine import_simple_int_tensor(this,un)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%eye
       end subroutine

       subroutine display_wrapper_simple_int_tensor(this,dir,name)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_simple_int_tensor(this,dir,name)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_simple_int_tensor(this,dir,name)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module