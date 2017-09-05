       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module var_mod
       use IO_tools_mod
       use export_field_mod
       use export_lines_mod
       use export_planes_mod
       use iter_solver_params_mod
       use matrix_free_params_mod
       use solver_settings_mod
       use time_marching_params_mod
       implicit none

       private
       public :: var
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_va;    end interface
       interface delete;       module procedure delete_va;       end interface
       interface display;      module procedure display_va;      end interface
       interface display_short;module procedure display_short_va;end interface
       interface display;      module procedure display_wrap_va; end interface
       interface print;        module procedure print_va;        end interface
       interface print_short;  module procedure print_short_va;  end interface
       interface export;       module procedure export_va;       end interface
       interface import;       module procedure import_va;       end interface
       interface export;       module procedure export_wrap_va;  end interface
       interface import;       module procedure import_wrap_va;  end interface

       type var
         integer :: IC = 0
         integer :: BC = 0
         type(solver_settings) :: SS
         type(matrix_free_params) :: MFP
         type(time_marching_params) :: TMP
         type(iter_solver_params) :: ISP
         type(export_lines) :: unsteady_lines
         type(export_planes) :: unsteady_planes
         type(export_field) :: unsteady_field
       end type

       contains

       subroutine init_copy_va(this,that)
         implicit none
         type(var),intent(inout) :: this
         type(var),intent(in) :: that
         call delete(this)
         this%IC = that%IC
         this%BC = that%BC
         call init(this%SS,that%SS)
         call init(this%MFP,that%MFP)
         call init(this%TMP,that%TMP)
         call init(this%ISP,that%ISP)
         call init(this%unsteady_lines,that%unsteady_lines)
         call init(this%unsteady_planes,that%unsteady_planes)
         call init(this%unsteady_field,that%unsteady_field)
       end subroutine

       subroutine delete_va(this)
         implicit none
         type(var),intent(inout) :: this
         this%IC = 0
         this%BC = 0
         call delete(this%SS)
         call delete(this%MFP)
         call delete(this%TMP)
         call delete(this%ISP)
         call delete(this%unsteady_lines)
         call delete(this%unsteady_planes)
         call delete(this%unsteady_field)
       end subroutine

       subroutine display_va(this,un)
         implicit none
         type(var),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- var'
         write(un,*) 'IC              = ',this%IC
         write(un,*) 'BC              = ',this%BC
         call display(this%SS,un)
         call display(this%MFP,un)
         call display(this%TMP,un)
         call display(this%ISP,un)
         call display(this%unsteady_lines,un)
         call display(this%unsteady_planes,un)
         call display(this%unsteady_field,un)
       end subroutine

       subroutine display_short_va(this,un)
         implicit none
         type(var),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'IC              = ',this%IC
         write(un,*) 'BC              = ',this%BC
         call display(this%SS,un)
         call display(this%MFP,un)
         call display(this%TMP,un)
         call display(this%ISP,un)
         call display(this%unsteady_lines,un)
         call display(this%unsteady_planes,un)
         call display(this%unsteady_field,un)
       end subroutine

       subroutine print_va(this)
         implicit none
         type(var),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_va(this)
         implicit none
         type(var),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_va(this,un)
         implicit none
         type(var),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'IC               = ';write(un,*) this%IC
         write(un,*) 'BC               = ';write(un,*) this%BC
         call export(this%SS,un)
         call export(this%MFP,un)
         call export(this%TMP,un)
         call export(this%ISP,un)
         call export(this%unsteady_lines,un)
         call export(this%unsteady_planes,un)
         call export(this%unsteady_field,un)
       end subroutine

       subroutine import_va(this,un)
         implicit none
         type(var),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%IC
         read(un,*); read(un,*) this%BC
         call import(this%SS,un)
         call import(this%MFP,un)
         call import(this%TMP,un)
         call import(this%ISP,un)
         call import(this%unsteady_lines,un)
         call import(this%unsteady_planes,un)
         call import(this%unsteady_field,un)
       end subroutine

       subroutine display_wrap_va(this,dir,name)
         implicit none
         type(var),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_va(this,dir,name)
         implicit none
         type(var),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_va(this,dir,name)
         implicit none
         type(var),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module