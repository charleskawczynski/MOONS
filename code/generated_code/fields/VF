       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module vf_mod
       use IO_tools_mod
       use SF_mod
       implicit none

       private
       public :: vf
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_vf;           end interface
       interface delete; module procedure delete_vf;         end interface
       interface display;module procedure display_vf;        end interface
       interface display;module procedure display_wrapper_vf;end interface
       interface print;  module procedure print_vf;          end interface
       interface export; module procedure export_vf;         end interface
       interface import; module procedure import_vf;         end interface
       interface export; module procedure export_wrapper_vf; end interface
       interface import; module procedure import_wrapper_vf; end interface

       type vf
         type(sf) :: x
         type(sf) :: y
         type(sf) :: z
       end type

       contains

       subroutine init_vf(this,that)
         implicit none
         type(vf),intent(inout) :: this
         type(vf),intent(in) :: that
         call delete(this)
         call init(this%x,that%x)
         call init(this%y,that%y)
         call init(this%z,that%z)
       end subroutine

       subroutine delete_vf(this)
         implicit none
         type(vf),intent(inout) :: this
         call delete(this%x)
         call delete(this%y)
         call delete(this%z)
       end subroutine

       subroutine display_vf(this,un)
         implicit none
         type(vf),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- vf'
         call display(this%x,un)
         call display(this%y,un)
         call display(this%z,un)
       end subroutine

       subroutine print_vf(this)
         implicit none
         type(vf),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_vf(this,un)
         implicit none
         type(vf),intent(in) :: this
         integer,intent(in) :: un
         call export(this%x,un)
         call export(this%y,un)
         call export(this%z,un)
       end subroutine

       subroutine import_vf(this,un)
         implicit none
         type(vf),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%x,un)
         call import(this%y,un)
         call import(this%z,un)
       end subroutine

       subroutine display_wrapper_vf(this,dir,name)
         implicit none
         type(vf),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_vf(this,dir,name)
         implicit none
         type(vf),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_vf(this,dir,name)
         implicit none
         type(vf),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module