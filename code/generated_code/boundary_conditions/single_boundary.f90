       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module single_boundary_mod
       use IO_tools_mod
       use bctype_mod
       use grid_field_mod
       implicit none

       private
       public :: single_boundary
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_single_boundary;           end interface
       interface delete; module procedure delete_single_boundary;         end interface
       interface display;module procedure display_single_boundary;        end interface
       interface display;module procedure display_wrapper_single_boundary;end interface
       interface print;  module procedure print_single_boundary;          end interface
       interface export; module procedure export_single_boundary;         end interface
       interface import; module procedure import_single_boundary;         end interface
       interface export; module procedure export_wrapper_single_boundary; end interface
       interface import; module procedure import_wrapper_single_boundary; end interface

       type single_boundary
         type(bctype) :: bct
         type(grid_field) :: b
         type(grid_field) :: b_modified
         type(grid_field) :: b_total
       end type

       contains

       subroutine init_single_boundary(this,that)
         implicit none
         type(single_boundary),intent(inout) :: this
         type(single_boundary),intent(in) :: that
         call delete(this)
         call init(this%bct,that%bct)
         call init(this%b,that%b)
         call init(this%b_modified,that%b_modified)
         call init(this%b_total,that%b_total)
       end subroutine

       subroutine delete_single_boundary(this)
         implicit none
         type(single_boundary),intent(inout) :: this
         call delete(this%bct)
         call delete(this%b)
         call delete(this%b_modified)
         call delete(this%b_total)
       end subroutine

       subroutine display_single_boundary(this,un)
         implicit none
         type(single_boundary),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- single_boundary'
         call display(this%bct,un)
         call display(this%b,un)
         call display(this%b_modified,un)
         call display(this%b_total,un)
       end subroutine

       subroutine print_single_boundary(this)
         implicit none
         type(single_boundary),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_single_boundary(this,un)
         implicit none
         type(single_boundary),intent(in) :: this
         integer,intent(in) :: un
         call export(this%bct,un)
         call export(this%b,un)
         call export(this%b_modified,un)
         call export(this%b_total,un)
       end subroutine

       subroutine import_single_boundary(this,un)
         implicit none
         type(single_boundary),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%bct,un)
         call import(this%b,un)
         call import(this%b_modified,un)
         call import(this%b_total,un)
       end subroutine

       subroutine display_wrapper_single_boundary(this,dir,name)
         implicit none
         type(single_boundary),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_single_boundary(this,dir,name)
         implicit none
         type(single_boundary),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_single_boundary(this,dir,name)
         implicit none
         type(single_boundary),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module