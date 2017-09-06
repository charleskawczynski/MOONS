       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module index_2D_mod
       use IO_tools_mod
       implicit none

       private
       public :: index_2D
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_in;    end interface
       interface delete;       module procedure delete_in;       end interface
       interface display;      module procedure display_in;      end interface
       interface display_short;module procedure display_short_in;end interface
       interface display;      module procedure display_wrap_in; end interface
       interface print;        module procedure print_in;        end interface
       interface print_short;  module procedure print_short_in;  end interface
       interface export;       module procedure export_in;       end interface
       interface import;       module procedure import_in;       end interface
       interface export;       module procedure export_wrap_in;  end interface
       interface import;       module procedure import_wrap_in;  end interface

       type index_2D
         integer,dimension(2) :: i = 0
       end type

       contains

       subroutine init_copy_in(this,that)
         implicit none
         type(index_2D),intent(inout) :: this
         type(index_2D),intent(in) :: that
         call delete(this)
         this%i = that%i
       end subroutine

       subroutine delete_in(this)
         implicit none
         type(index_2D),intent(inout) :: this
         this%i = 0
       end subroutine

       subroutine display_in(this,un)
         implicit none
         type(index_2D),intent(in) :: this
         integer,intent(in) :: un
       end subroutine

       subroutine display_short_in(this,un)
         implicit none
         type(index_2D),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'i = ',this%i
       end subroutine

       subroutine print_in(this)
         implicit none
         type(index_2D),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_in(this)
         implicit none
         type(index_2D),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_in(this,un)
         implicit none
         type(index_2D),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'i  = ';write(un,*) this%i
       end subroutine

       subroutine import_in(this,un)
         implicit none
         type(index_2D),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%i
       end subroutine

       subroutine display_wrap_in(this,dir,name)
         implicit none
         type(index_2D),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_in(this,dir,name)
         implicit none
         type(index_2D),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_in(this,dir,name)
         implicit none
         type(index_2D),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module