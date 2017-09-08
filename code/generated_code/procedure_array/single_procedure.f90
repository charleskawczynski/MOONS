       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module single_procedure_mod
       use IO_tools_mod
       use apply_face_BC_op_mod
       implicit none

       private
       public :: single_procedure
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_single_procedure;    end interface
       interface delete;       module procedure delete_single_procedure;       end interface
       interface display;      module procedure display_single_procedure;      end interface
       interface display_short;module procedure display_short_single_procedure;end interface
       interface display;      module procedure display_wrap_single_procedure; end interface
       interface print;        module procedure print_single_procedure;        end interface
       interface print_short;  module procedure print_short_single_procedure;  end interface
       interface export;       module procedure export_single_procedure;       end interface
       interface import;       module procedure import_single_procedure;       end interface
       interface export;       module procedure export_wrap_single_procedure;  end interface
       interface import;       module procedure import_wrap_single_procedure;  end interface

       type single_procedure
         procedure(apply_face_BC_op),pointer,nopass :: P
         logical :: defined = .false.
         integer :: ID = 0
       end type

       contains

       subroutine init_copy_single_procedure(this,that)
         implicit none
         type(single_procedure),intent(inout) :: this
         type(single_procedure),intent(in) :: that
         call delete(this)
         this%P => that%P
         this%defined = that%defined
         this%ID = that%ID
       end subroutine

       subroutine delete_single_procedure(this)
         implicit none
         type(single_procedure),intent(inout) :: this
         nullify(this%P)
         this%defined = .false.
         this%ID = 0
       end subroutine

       subroutine display_single_procedure(this,un)
         implicit none
         type(single_procedure),intent(in) :: this
         integer,intent(in) :: un
       end subroutine

       subroutine display_short_single_procedure(this,un)
         implicit none
         type(single_procedure),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'defined = ',this%defined
         write(un,*) 'ID      = ',this%ID
       end subroutine

       subroutine print_single_procedure(this)
         implicit none
         type(single_procedure),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_single_procedure(this)
         implicit none
         type(single_procedure),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_single_procedure(this,un)
         implicit none
         type(single_procedure),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'defined  = ';write(un,*) this%defined
         write(un,*) 'ID       = ';write(un,*) this%ID
       end subroutine

       subroutine import_single_procedure(this,un)
         implicit none
         type(single_procedure),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%ID
       end subroutine

       subroutine display_wrap_single_procedure(this,dir,name)
         implicit none
         type(single_procedure),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_single_procedure(this,dir,name)
         implicit none
         type(single_procedure),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_single_procedure(this,dir,name)
         implicit none
         type(single_procedure),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module