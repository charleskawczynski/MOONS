       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module boundary_conditions_mod
       use IO_tools_mod
       use BC_logicals_mod
       use boundary_mod
       use data_location_mod
       use face_SD_mod
       use procedure_array_mod
       implicit none

       private
       public :: boundary_conditions
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_boundary_conditions;           end interface
       interface delete; module procedure delete_boundary_conditions;         end interface
       interface display;module procedure display_boundary_conditions;        end interface
       interface display;module procedure display_wrapper_boundary_conditions;end interface
       interface print;  module procedure print_boundary_conditions;          end interface
       interface export; module procedure export_boundary_conditions;         end interface
       interface import; module procedure import_boundary_conditions;         end interface
       interface export; module procedure export_wrapper_boundary_conditions; end interface
       interface import; module procedure import_wrapper_boundary_conditions; end interface

       type boundary_conditions
         integer,dimension(6) :: apply_bc_order = 0
         type(bc_logicals) :: bcl
         type(data_location) :: dl
         type(boundary) :: face
         type(procedure_array) :: pa_face_bcs
         type(procedure_array) :: pa_face_implicit_bcs
         type(face_sd) :: f_bcs
       end type

       contains

       subroutine init_boundary_conditions(this,that)
         implicit none
         type(boundary_conditions),intent(inout) :: this
         type(boundary_conditions),intent(in) :: that
         call delete(this)
         this%apply_bc_order = that%apply_bc_order
         call init(this%bcl,that%bcl)
         call init(this%dl,that%dl)
         call init(this%face,that%face)
         call init(this%pa_face_bcs,that%pa_face_bcs)
         call init(this%pa_face_implicit_bcs,that%pa_face_implicit_bcs)
         call init(this%f_bcs,that%f_bcs)
       end subroutine

       subroutine delete_boundary_conditions(this)
         implicit none
         type(boundary_conditions),intent(inout) :: this
         this%apply_bc_order = 0
         call delete(this%bcl)
         call delete(this%dl)
         call delete(this%face)
         call delete(this%pa_face_bcs)
         call delete(this%pa_face_implicit_bcs)
         call delete(this%f_bcs)
       end subroutine

       subroutine display_boundary_conditions(this,un)
         implicit none
         type(boundary_conditions),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- boundary_conditions'
         write(un,*) 'apply_bc_order       = ',this%apply_bc_order
         call display(this%bcl,un)
         call display(this%dl,un)
         call display(this%face,un)
         call display(this%pa_face_bcs,un)
         call display(this%pa_face_implicit_bcs,un)
         call display(this%f_bcs,un)
       end subroutine

       subroutine print_boundary_conditions(this)
         implicit none
         type(boundary_conditions),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_boundary_conditions(this,un)
         implicit none
         type(boundary_conditions),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'apply_bc_order        = ';write(un,*) this%apply_bc_order
         call export(this%bcl,un)
         call export(this%dl,un)
         call export(this%face,un)
         call export(this%pa_face_bcs,un)
         call export(this%pa_face_implicit_bcs,un)
         call export(this%f_bcs,un)
       end subroutine

       subroutine import_boundary_conditions(this,un)
         implicit none
         type(boundary_conditions),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%apply_bc_order
         call import(this%bcl,un)
         call import(this%dl,un)
         call import(this%face,un)
         call import(this%pa_face_bcs,un)
         call import(this%pa_face_implicit_bcs,un)
         call import(this%f_bcs,un)
       end subroutine

       subroutine display_wrapper_boundary_conditions(this,dir,name)
         implicit none
         type(boundary_conditions),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_boundary_conditions(this,dir,name)
         implicit none
         type(boundary_conditions),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_boundary_conditions(this,dir,name)
         implicit none
         type(boundary_conditions),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module