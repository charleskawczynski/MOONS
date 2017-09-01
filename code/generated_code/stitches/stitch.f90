       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module stitch_mod
       use IO_tools_mod
       implicit none

       private
       public :: stitch
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_stitch;      end interface
       interface delete;       module procedure delete_stitch;         end interface
       interface display;      module procedure display_stitch;        end interface
       interface display_short;module procedure display_short_stitch;  end interface
       interface display;      module procedure display_wrapper_stitch;end interface
       interface print;        module procedure print_stitch;          end interface
       interface print_short;  module procedure print_short_stitch;    end interface
       interface export;       module procedure export_stitch;         end interface
       interface import;       module procedure import_stitch;         end interface
       interface export;       module procedure export_wrapper_stitch; end interface
       interface import;       module procedure import_wrapper_stitch; end interface

       type stitch
         logical :: L = .false.
         integer :: ID = 0
       end type

       contains

       subroutine init_copy_stitch(this,that)
         implicit none
         type(stitch),intent(inout) :: this
         type(stitch),intent(in) :: that
         call delete(this)
         this%L = that%L
         this%ID = that%ID
       end subroutine

       subroutine delete_stitch(this)
         implicit none
         type(stitch),intent(inout) :: this
         this%L = .false.
         this%ID = 0
       end subroutine

       subroutine display_stitch(this,un)
         implicit none
         type(stitch),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- stitch'
         write(un,*) 'L  = ',this%L
         write(un,*) 'ID = ',this%ID
       end subroutine

       subroutine display_short_stitch(this,un)
         implicit none
         type(stitch),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'L  = ',this%L
         write(un,*) 'ID = ',this%ID
       end subroutine

       subroutine print_stitch(this)
         implicit none
         type(stitch),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_stitch(this)
         implicit none
         type(stitch),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_stitch(this,un)
         implicit none
         type(stitch),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'L   = ';write(un,*) this%L
         write(un,*) 'ID  = ';write(un,*) this%ID
       end subroutine

       subroutine import_stitch(this,un)
         implicit none
         type(stitch),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%L
         read(un,*); read(un,*) this%ID
       end subroutine

       subroutine display_wrapper_stitch(this,dir,name)
         implicit none
         type(stitch),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_stitch(this,dir,name)
         implicit none
         type(stitch),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_stitch(this,dir,name)
         implicit none
         type(stitch),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module