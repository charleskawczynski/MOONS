       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_frequency_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use export_frequency_params_mod
       use string_mod
       implicit none

       private
       public :: export_frequency
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_export_frequency;        end interface
       interface delete;           module procedure delete_export_frequency;           end interface
       interface display;          module procedure display_export_frequency;          end interface
       interface display_short;    module procedure display_short_export_frequency;    end interface
       interface display;          module procedure display_wrap_export_frequency;     end interface
       interface print;            module procedure print_export_frequency;            end interface
       interface print_short;      module procedure print_short_export_frequency;      end interface
       interface export;           module procedure export_export_frequency;           end interface
       interface export_primitives;module procedure export_primitives_export_frequency;end interface
       interface export_restart;   module procedure export_restart_export_frequency;   end interface
       interface import;           module procedure import_export_frequency;           end interface
       interface import_restart;   module procedure import_restart_export_frequency;   end interface
       interface import_primitives;module procedure import_primitives_export_frequency;end interface
       interface export;           module procedure export_wrap_export_frequency;      end interface
       interface import;           module procedure import_wrap_export_frequency;      end interface
       interface make_restart_dir; module procedure make_restart_dir_export_frequency; end interface
       interface suppress_warnings;module procedure suppress_warnings_export_frequency;end interface
       interface export;           module procedure export_DN_export_frequency;        end interface
       interface import;           module procedure import_DN_export_frequency;        end interface

       type export_frequency
         type(export_frequency_params) :: info
         type(export_frequency_params) :: unsteady_0D
         type(export_frequency_params) :: unsteady_1D
         type(export_frequency_params) :: unsteady_2D
         type(export_frequency_params) :: unsteady_3D
         type(export_frequency_params) :: final_solution
         type(export_frequency_params) :: restart_files
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_copy_export_frequency(this,that)
         implicit none
         type(export_frequency),intent(inout) :: this
         type(export_frequency),intent(in) :: that
         call delete(this)
         call init(this%info,that%info)
         call init(this%unsteady_0D,that%unsteady_0D)
         call init(this%unsteady_1D,that%unsteady_1D)
         call init(this%unsteady_2D,that%unsteady_2D)
         call init(this%unsteady_3D,that%unsteady_3D)
         call init(this%final_solution,that%final_solution)
         call init(this%restart_files,that%restart_files)
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_export_frequency(this)
         implicit none
         type(export_frequency),intent(inout) :: this
         call delete(this%info)
         call delete(this%unsteady_0D)
         call delete(this%unsteady_1D)
         call delete(this%unsteady_2D)
         call delete(this%unsteady_3D)
         call delete(this%final_solution)
         call delete(this%restart_files)
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_export_frequency(this,un)
         implicit none
         type(export_frequency),intent(in) :: this
         integer,intent(in) :: un
         call display(this%info,un)
         call display(this%unsteady_0D,un)
         call display(this%unsteady_1D,un)
         call display(this%unsteady_2D,un)
         call display(this%unsteady_3D,un)
         call display(this%final_solution,un)
         call display(this%restart_files,un)
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_short_export_frequency(this,un)
         implicit none
         type(export_frequency),intent(in) :: this
         integer,intent(in) :: un
         call display(this%info,un)
         call display(this%unsteady_0D,un)
         call display(this%unsteady_1D,un)
         call display(this%unsteady_2D,un)
         call display(this%unsteady_3D,un)
         call display(this%final_solution,un)
         call display(this%restart_files,un)
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_wrap_export_frequency(this,dir,name)
         implicit none
         type(export_frequency),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_export_frequency(this)
         implicit none
         type(export_frequency),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_export_frequency(this)
         implicit none
         type(export_frequency),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_export_frequency(this,un)
         implicit none
         type(export_frequency),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_export_frequency(this,un)
         implicit none
         type(export_frequency),intent(in) :: this
         integer,intent(in) :: un
         call export(this%info,un)
         call export(this%unsteady_0D,un)
         call export(this%unsteady_1D,un)
         call export(this%unsteady_2D,un)
         call export(this%unsteady_3D,un)
         call export(this%final_solution,un)
         call export(this%restart_files,un)
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_primitives_export_frequency(this,un)
         implicit none
         type(export_frequency),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_export_frequency(this,un)
         implicit none
         type(export_frequency),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%info,un)
         call import(this%unsteady_0D,un)
         call import(this%unsteady_1D,un)
         call import(this%unsteady_2D,un)
         call import(this%unsteady_3D,un)
         call import(this%final_solution,un)
         call import(this%restart_files,un)
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine export_wrap_export_frequency(this,dir,name)
         implicit none
         type(export_frequency),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_export_frequency(this,dir,name)
         implicit none
         type(export_frequency),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine export_DN_export_frequency(this)
         implicit none
         type(export_frequency),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_export_frequency(this)
         implicit none
         type(export_frequency),intent(inout) :: this
         type(string) :: dir,name
         integer :: un
         call init(dir,this%dir)
         call init(name,this%name)
         un = open_to_read(str(dir),str(name))
         call import(this,un)
         call delete(dir)
         call delete(name)
         close(un)
       end subroutine

       subroutine make_restart_dir_export_frequency(this,dir)
         implicit none
         type(export_frequency),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%info,dir//fortran_PS//'info')
         call make_restart_dir(this%unsteady_0D,&
         dir//fortran_PS//'unsteady_0D')
         call make_restart_dir(this%unsteady_1D,&
         dir//fortran_PS//'unsteady_1D')
         call make_restart_dir(this%unsteady_2D,&
         dir//fortran_PS//'unsteady_2D')
         call make_restart_dir(this%unsteady_3D,&
         dir//fortran_PS//'unsteady_3D')
         call make_restart_dir(this%final_solution,&
         dir//fortran_PS//'final_solution')
         call make_restart_dir(this%restart_files,&
         dir//fortran_PS//'restart_files')
       end subroutine

       subroutine export_restart_export_frequency(this,dir)
         implicit none
         type(export_frequency),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%info,dir//fortran_PS//'info')
         call export_restart(this%unsteady_0D,dir//fortran_PS//'unsteady_0D')
         call export_restart(this%unsteady_1D,dir//fortran_PS//'unsteady_1D')
         call export_restart(this%unsteady_2D,dir//fortran_PS//'unsteady_2D')
         call export_restart(this%unsteady_3D,dir//fortran_PS//'unsteady_3D')
         call export_restart(this%final_solution,&
         dir//fortran_PS//'final_solution')
         call export_restart(this%restart_files,&
         dir//fortran_PS//'restart_files')
       end subroutine

       subroutine import_restart_export_frequency(this,dir)
         implicit none
         type(export_frequency),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%info,dir//fortran_PS//'info')
         call import_restart(this%unsteady_0D,dir//fortran_PS//'unsteady_0D')
         call import_restart(this%unsteady_1D,dir//fortran_PS//'unsteady_1D')
         call import_restart(this%unsteady_2D,dir//fortran_PS//'unsteady_2D')
         call import_restart(this%unsteady_3D,dir//fortran_PS//'unsteady_3D')
         call import_restart(this%final_solution,&
         dir//fortran_PS//'final_solution')
         call import_restart(this%restart_files,&
         dir//fortran_PS//'restart_files')
       end subroutine

       subroutine suppress_warnings_export_frequency(this)
         implicit none
         type(export_frequency),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module