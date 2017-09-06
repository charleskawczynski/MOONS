       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module overlap_mod
       use IO_tools_mod
       implicit none

       private
       public :: overlap
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_ov;    end interface
       interface delete;       module procedure delete_ov;       end interface
       interface display;      module procedure display_ov;      end interface
       interface display_short;module procedure display_short_ov;end interface
       interface display;      module procedure display_wrap_ov; end interface
       interface print;        module procedure print_ov;        end interface
       interface print_short;  module procedure print_short_ov;  end interface
       interface export;       module procedure export_ov;       end interface
       interface import;       module procedure import_ov;       end interface
       interface export;       module procedure export_wrap_ov;  end interface
       interface import;       module procedure import_wrap_ov;  end interface

       type overlap
         integer,dimension(2) :: i1 = 0
         integer,dimension(2) :: i2 = 0
         integer :: iR = 0
         logical :: success = .false.
       end type

       contains

       subroutine init_copy_ov(this,that)
         implicit none
         type(overlap),intent(inout) :: this
         type(overlap),intent(in) :: that
         call delete(this)
         this%i1 = that%i1
         this%i2 = that%i2
         this%iR = that%iR
         this%success = that%success
       end subroutine

       subroutine delete_ov(this)
         implicit none
         type(overlap),intent(inout) :: this
         this%i1 = 0
         this%i2 = 0
         this%iR = 0
         this%success = .false.
       end subroutine

       subroutine display_ov(this,un)
         implicit none
         type(overlap),intent(in) :: this
         integer,intent(in) :: un
       end subroutine

       subroutine display_short_ov(this,un)
         implicit none
         type(overlap),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'i1      = ',this%i1
         write(un,*) 'i2      = ',this%i2
         write(un,*) 'iR      = ',this%iR
         write(un,*) 'success = ',this%success
       end subroutine

       subroutine print_ov(this)
         implicit none
         type(overlap),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_ov(this)
         implicit none
         type(overlap),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_ov(this,un)
         implicit none
         type(overlap),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'i1       = ';write(un,*) this%i1
         write(un,*) 'i2       = ';write(un,*) this%i2
         write(un,*) 'iR       = ';write(un,*) this%iR
         write(un,*) 'success  = ';write(un,*) this%success
       end subroutine

       subroutine import_ov(this,un)
         implicit none
         type(overlap),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%i1
         read(un,*); read(un,*) this%i2
         read(un,*); read(un,*) this%iR
         read(un,*); read(un,*) this%success
       end subroutine

       subroutine display_wrap_ov(this,dir,name)
         implicit none
         type(overlap),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_ov(this,dir,name)
         implicit none
         type(overlap),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_ov(this,dir,name)
         implicit none
         type(overlap),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module