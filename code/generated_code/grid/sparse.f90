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

       interface init;   module procedure init_sparse;           end interface
       interface delete; module procedure delete_sparse;         end interface
       interface display;module procedure display_sparse;        end interface
       interface display;module procedure display_wrapper_sparse;end interface
       interface print;  module procedure print_sparse;          end interface
       interface export; module procedure export_sparse;         end interface
       interface import; module procedure import_sparse;         end interface
       interface export; module procedure export_wrapper_sparse; end interface
       interface import; module procedure import_wrapper_sparse; end interface

       type sparse
         type(array) :: l
         type(array) :: d
         type(array) :: u
         logical :: staggered = .false.
       end type

       contains

       subroutine init_sparse(this,that)
         implicit none
         type(sparse),intent(inout) :: this
         type(sparse),intent(in) :: that
         call delete(this)
         call init(this%l,that%l)
         call init(this%d,that%d)
         call init(this%u,that%u)
         this%staggered = that%staggered
       end subroutine

       subroutine delete_sparse(this)
         implicit none
         type(sparse),intent(inout) :: this
         call delete(this%l)
         call delete(this%d)
         call delete(this%u)
         this%staggered = .false.
       end subroutine

       subroutine display_sparse(this,un)
         implicit none
         type(sparse),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- sparse'
         call display(this%l,un)
         call display(this%d,un)
         call display(this%u,un)
         write(un,*) 'staggered = ',this%staggered
       end subroutine

       subroutine print_sparse(this)
         implicit none
         type(sparse),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_sparse(this,un)
         implicit none
         type(sparse),intent(in) :: this
         integer,intent(in) :: un
         call export(this%l,un)
         call export(this%d,un)
         call export(this%u,un)
         write(un,*) 'staggered  = ';write(un,*) this%staggered
       end subroutine

       subroutine import_sparse(this,un)
         implicit none
         type(sparse),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%l,un)
         call import(this%d,un)
         call import(this%u,un)
         read(un,*); read(un,*) this%staggered
       end subroutine

       subroutine display_wrapper_sparse(this,dir,name)
         implicit none
         type(sparse),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_sparse(this,dir,name)
         implicit none
         type(sparse),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_sparse(this,dir,name)
         implicit none
         type(sparse),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module