       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module tf_mod
       use IO_tools_mod
       use VF_mod
       implicit none

       private
       public :: tf
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_tf;           end interface
       interface delete; module procedure delete_tf;         end interface
       interface display;module procedure display_tf;        end interface
       interface display;module procedure display_wrapper_tf;end interface
       interface print;  module procedure print_tf;          end interface
       interface export; module procedure export_tf;         end interface
       interface import; module procedure import_tf;         end interface
       interface export; module procedure export_wrapper_tf; end interface
       interface import; module procedure import_wrapper_tf; end interface

       type tf
         type(vf) :: x
         type(vf) :: y
         type(vf) :: z
       end type

       contains

       subroutine init_tf(this,that)
         implicit none
         type(tf),intent(inout) :: this
         type(tf),intent(in) :: that
         call delete(this)
         call init(this%x,that%x)
         call init(this%y,that%y)
         call init(this%z,that%z)
       end subroutine

       subroutine delete_tf(this)
         implicit none
         type(tf),intent(inout) :: this
         call delete(this%x)
         call delete(this%y)
         call delete(this%z)
       end subroutine

       subroutine display_tf(this,un)
         implicit none
         type(tf),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- tf'
         call display(this%x,un)
         call display(this%y,un)
         call display(this%z,un)
       end subroutine

       subroutine print_tf(this)
         implicit none
         type(tf),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_tf(this,un)
         implicit none
         type(tf),intent(in) :: this
         integer,intent(in) :: un
         call export(this%x,un)
         call export(this%y,un)
         call export(this%z,un)
       end subroutine

       subroutine import_tf(this,un)
         implicit none
         type(tf),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%x,un)
         call import(this%y,un)
         call import(this%z,un)
       end subroutine

       subroutine display_wrapper_tf(this,dir,name)
         implicit none
         type(tf),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_tf(this,dir,name)
         implicit none
         type(tf),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_tf(this,dir,name)
         implicit none
         type(tf),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module