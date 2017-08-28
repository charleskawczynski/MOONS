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

       interface init;   module procedure init_var;           end interface
       interface delete; module procedure delete_var;         end interface
       interface display;module procedure display_var;        end interface
       interface display;module procedure display_wrapper_var;end interface
       interface print;  module procedure print_var;          end interface
       interface export; module procedure export_var;         end interface
       interface import; module procedure import_var;         end interface
       interface export; module procedure export_wrapper_var; end interface
       interface import; module procedure import_wrapper_var; end interface

       type var
         integer :: ic = 0
         integer :: bc = 0
         type(solver_settings) :: ss
         type(matrix_free_params) :: mfp
         type(time_marching_params) :: tmp
         type(iter_solver_params) :: isp
         type(export_lines) :: unsteady_lines
         type(export_planes) :: unsteady_planes
         type(export_field) :: unsteady_field
       end type

       contains

       subroutine init_var(this,that)
         implicit none
         type(var),intent(inout) :: this
         type(var),intent(in) :: that
         call delete(this)
         this%ic = that%ic
         this%bc = that%bc
         call init(this%ss,that%ss)
         call init(this%mfp,that%mfp)
         call init(this%tmp,that%tmp)
         call init(this%isp,that%isp)
         call init(this%unsteady_lines,that%unsteady_lines)
         call init(this%unsteady_planes,that%unsteady_planes)
         call init(this%unsteady_field,that%unsteady_field)
       end subroutine

       subroutine delete_var(this)
         implicit none
         type(var),intent(inout) :: this
         this%ic = 0
         this%bc = 0
         call delete(this%ss)
         call delete(this%mfp)
         call delete(this%tmp)
         call delete(this%isp)
         call delete(this%unsteady_lines)
         call delete(this%unsteady_planes)
         call delete(this%unsteady_field)
       end subroutine

       subroutine display_var(this,un)
         implicit none
         type(var),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- var'
         write(un,*) 'ic              = ',this%ic
         write(un,*) 'bc              = ',this%bc
         call display(this%ss,un)
         call display(this%mfp,un)
         call display(this%tmp,un)
         call display(this%isp,un)
         call display(this%unsteady_lines,un)
         call display(this%unsteady_planes,un)
         call display(this%unsteady_field,un)
       end subroutine

       subroutine print_var(this)
         implicit none
         type(var),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_var(this,un)
         implicit none
         type(var),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'ic               = ';write(un,*) this%ic
         write(un,*) 'bc               = ';write(un,*) this%bc
         call export(this%ss,un)
         call export(this%mfp,un)
         call export(this%tmp,un)
         call export(this%isp,un)
         call export(this%unsteady_lines,un)
         call export(this%unsteady_planes,un)
         call export(this%unsteady_field,un)
       end subroutine

       subroutine import_var(this,un)
         implicit none
         type(var),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%ic
         read(un,*); read(un,*) this%bc
         call import(this%ss,un)
         call import(this%mfp,un)
         call import(this%tmp,un)
         call import(this%isp,un)
         call import(this%unsteady_lines,un)
         call import(this%unsteady_planes,un)
         call import(this%unsteady_field,un)
       end subroutine

       subroutine display_wrapper_var(this,dir,name)
         implicit none
         type(var),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_var(this,dir,name)
         implicit none
         type(var),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_var(this,dir,name)
         implicit none
         type(var),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module