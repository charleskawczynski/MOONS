       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module sparse_mod
       use IO_tools_mod
       use array_mod
       implicit none

       private
       public :: sparse
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_sp;    end interface
       interface delete;       module procedure delete_sp;       end interface
       interface display;      module procedure display_sp;      end interface
       interface display_short;module procedure display_short_sp;end interface
       interface display;      module procedure display_wrap_sp; end interface
       interface print;        module procedure print_sp;        end interface
       interface print_short;  module procedure print_short_sp;  end interface
       interface export;       module procedure export_sp;       end interface
       interface import;       module procedure import_sp;       end interface
       interface export;       module procedure export_wrap_sp;  end interface
       interface import;       module procedure import_wrap_sp;  end interface

       type sparse
         type(array) :: L
         type(array) :: D
         type(array) :: U
         logical :: staggered = .false.
       end type

       contains

       subroutine init_copy_sp(this,that)
         implicit none
         type(sparse),intent(inout) :: this
         type(sparse),intent(in) :: that
         call delete(this)
         call init(this%L,that%L)
         call init(this%D,that%D)
         call init(this%U,that%U)
         this%staggered = that%staggered
       end subroutine

       subroutine delete_sp(this)
         implicit none
         type(sparse),intent(inout) :: this
         call delete(this%L)
         call delete(this%D)
         call delete(this%U)
         this%staggered = .false.
       end subroutine

       subroutine display_sp(this,un)
         implicit none
         type(sparse),intent(in) :: this
         integer,intent(in) :: un
         call display(this%L,un)
         call display(this%D,un)
         call display(this%U,un)
       end subroutine

       subroutine display_short_sp(this,un)
         implicit none
         type(sparse),intent(in) :: this
         integer,intent(in) :: un
         call display(this%L,un)
         call display(this%D,un)
         call display(this%U,un)
         write(un,*) 'staggered = ',this%staggered
       end subroutine

       subroutine print_sp(this)
         implicit none
         type(sparse),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_sp(this)
         implicit none
         type(sparse),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_sp(this,un)
         implicit none
         type(sparse),intent(in) :: this
         integer,intent(in) :: un
         call export(this%L,un)
         call export(this%D,un)
         call export(this%U,un)
         write(un,*) 'staggered  = ';write(un,*) this%staggered
       end subroutine

       subroutine import_sp(this,un)
         implicit none
         type(sparse),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%L,un)
         call import(this%D,un)
         call import(this%U,un)
         read(un,*); read(un,*) this%staggered
       end subroutine

       subroutine display_wrap_sp(this,dir,name)
         implicit none
         type(sparse),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_sp(this,dir,name)
         implicit none
         type(sparse),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_sp(this,dir,name)
         implicit none
         type(sparse),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module