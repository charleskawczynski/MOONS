       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module index_2d_mod
       use IO_tools_mod
       implicit none

       private
       public :: index_2d
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_index_2d;           end interface
       interface delete; module procedure delete_index_2d;         end interface
       interface display;module procedure display_index_2d;        end interface
       interface display;module procedure display_wrapper_index_2d;end interface
       interface print;  module procedure print_index_2d;          end interface
       interface export; module procedure export_index_2d;         end interface
       interface import; module procedure import_index_2d;         end interface
       interface export; module procedure export_wrapper_index_2d; end interface
       interface import; module procedure import_wrapper_index_2d; end interface

       type index_2d
         integer,dimension(2) :: i = 0
       end type

       contains

       subroutine init_index_2d(this,that)
         implicit none
         type(index_2d),intent(inout) :: this
         type(index_2d),intent(in) :: that
         call delete(this)
         this%i = that%i
       end subroutine

       subroutine delete_index_2d(this)
         implicit none
         type(index_2d),intent(inout) :: this
         this%i = 0
       end subroutine

       subroutine display_index_2d(this,un)
         implicit none
         type(index_2d),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- index_2d'
         write(un,*) 'i = ',this%i
       end subroutine

       subroutine print_index_2d(this)
         implicit none
         type(index_2d),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_index_2d(this,un)
         implicit none
         type(index_2d),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'i  = ';write(un,*) this%i
       end subroutine

       subroutine import_index_2d(this,un)
         implicit none
         type(index_2d),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%i
       end subroutine

       subroutine display_wrapper_index_2d(this,dir,name)
         implicit none
         type(index_2d),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_index_2d(this,dir,name)
         implicit none
         type(index_2d),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_index_2d(this,dir,name)
         implicit none
         type(index_2d),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module