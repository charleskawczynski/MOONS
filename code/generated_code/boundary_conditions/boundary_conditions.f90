       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module boundary_conditions_mod
       use IO_tools_mod
       use data_location_mod
       use BC_logicals_mod
       use boundary_mod
       use face_SD_mod
       use procedure_array_mod
       implicit none

       private
       public :: boundary_conditions
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_boundary_conditions;    end interface
       interface delete;       module procedure delete_boundary_conditions;       end interface
       interface display;      module procedure display_boundary_conditions;      end interface
       interface display_short;module procedure display_short_boundary_conditions;end interface
       interface display;      module procedure display_wrap_boundary_conditions; end interface
       interface print;        module procedure print_boundary_conditions;        end interface
       interface print_short;  module procedure print_short_boundary_conditions;  end interface
       interface export;       module procedure export_boundary_conditions;       end interface
       interface import;       module procedure import_boundary_conditions;       end interface
       interface export;       module procedure export_wrap_boundary_conditions;  end interface
       interface import;       module procedure import_wrap_boundary_conditions;  end interface

       type boundary_conditions
         integer,dimension(6) :: apply_BC_order = 0
         type(BC_logicals) :: BCL
         type(data_location) :: DL
         type(boundary) :: face
         type(procedure_array) :: PA_face_BCs
         type(procedure_array) :: PA_face_implicit_BCs
         type(face_SD) :: f_BCs
       end type

       contains

       subroutine init_copy_boundary_conditions(this,that)
         implicit none
         type(boundary_conditions),intent(inout) :: this
         type(boundary_conditions),intent(in) :: that
         call delete(this)
         this%apply_BC_order = that%apply_BC_order
         call init(this%BCL,that%BCL)
         call init(this%DL,that%DL)
         call init(this%face,that%face)
         call init(this%PA_face_BCs,that%PA_face_BCs)
         call init(this%PA_face_implicit_BCs,that%PA_face_implicit_BCs)
         call init(this%f_BCs,that%f_BCs)
       end subroutine

       subroutine delete_boundary_conditions(this)
         implicit none
         type(boundary_conditions),intent(inout) :: this
         this%apply_BC_order = 0
         call delete(this%BCL)
         call delete(this%DL)
         call delete(this%face)
         call delete(this%PA_face_BCs)
         call delete(this%PA_face_implicit_BCs)
         call delete(this%f_BCs)
       end subroutine

       subroutine display_boundary_conditions(this,un)
         implicit none
         type(boundary_conditions),intent(in) :: this
         integer,intent(in) :: un
         call display(this%BCL,un)
         call display(this%DL,un)
         call display(this%face,un)
         call display(this%PA_face_BCs,un)
         call display(this%PA_face_implicit_BCs,un)
         call display(this%f_BCs,un)
       end subroutine

       subroutine display_short_boundary_conditions(this,un)
         implicit none
         type(boundary_conditions),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'apply_BC_order       = ',this%apply_BC_order
         call display(this%BCL,un)
         call display(this%DL,un)
         call display(this%face,un)
         call display(this%PA_face_BCs,un)
         call display(this%PA_face_implicit_BCs,un)
         call display(this%f_BCs,un)
       end subroutine

       subroutine print_boundary_conditions(this)
         implicit none
         type(boundary_conditions),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_boundary_conditions(this)
         implicit none
         type(boundary_conditions),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_boundary_conditions(this,un)
         implicit none
         type(boundary_conditions),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'apply_BC_order        = ';write(un,*) this%apply_BC_order
         call export(this%BCL,un)
         call export(this%DL,un)
         call export(this%face,un)
         call export(this%PA_face_BCs,un)
         call export(this%PA_face_implicit_BCs,un)
         call export(this%f_BCs,un)
       end subroutine

       subroutine import_boundary_conditions(this,un)
         implicit none
         type(boundary_conditions),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%apply_BC_order
         call import(this%BCL,un)
         call import(this%DL,un)
         call import(this%face,un)
         call import(this%PA_face_BCs,un)
         call import(this%PA_face_implicit_BCs,un)
         call import(this%f_BCs,un)
       end subroutine

       subroutine display_wrap_boundary_conditions(this,dir,name)
         implicit none
         type(boundary_conditions),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_boundary_conditions(this,dir,name)
         implicit none
         type(boundary_conditions),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_boundary_conditions(this,dir,name)
         implicit none
         type(boundary_conditions),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module