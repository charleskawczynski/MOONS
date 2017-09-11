       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module step_mod
       use IO_tools_mod
       implicit none

       private
       public :: step
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_step;    end interface
       interface delete;       module procedure delete_step;       end interface
       interface display;      module procedure display_step;      end interface
       interface display_short;module procedure display_short_step;end interface
       interface display;      module procedure display_wrap_step; end interface
       interface print;        module procedure print_step;        end interface
       interface print_short;  module procedure print_short_step;  end interface
       interface export;       module procedure export_step;       end interface
       interface import;       module procedure import_step;       end interface
       interface export;       module procedure export_wrap_step;  end interface
       interface import;       module procedure import_wrap_step;  end interface

       type step
         logical :: this = .false.
         logical :: next = .false.
       end type

       contains

       subroutine init_copy_step(this,that)
         implicit none
         type(step),intent(inout) :: this
         type(step),intent(in) :: that
         call delete(this)
         this%this = that%this
         this%next = that%next
       end subroutine

       subroutine delete_step(this)
         implicit none
         type(step),intent(inout) :: this
         this%this = .false.
         this%next = .false.
       end subroutine

       subroutine display_step(this,un)
         implicit none
         type(step),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'this = ',this%this
         write(un,*) 'next = ',this%next
       end subroutine

       subroutine display_short_step(this,un)
         implicit none
         type(step),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'this = ',this%this
         write(un,*) 'next = ',this%next
       end subroutine

       subroutine print_step(this)
         implicit none
         type(step),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_step(this)
         implicit none
         type(step),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_step(this,un)
         implicit none
         type(step),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'this  = ';write(un,*) this%this
         write(un,*) 'next  = ';write(un,*) this%next
       end subroutine

       subroutine import_step(this,un)
         implicit none
         type(step),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%this
         read(un,*); read(un,*) this%next
       end subroutine

       subroutine display_wrap_step(this,dir,name)
         implicit none
         type(step),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_step(this,dir,name)
         implicit none
         type(step),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_step(this,dir,name)
         implicit none
         type(step),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module