       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module dir_group_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use path_mod
       use string_mod
       implicit none

       private
       public :: dir_group
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_dir_group;        end interface
       interface delete;           module procedure delete_dir_group;           end interface
       interface display;          module procedure display_dir_group;          end interface
       interface display_short;    module procedure display_short_dir_group;    end interface
       interface display;          module procedure display_wrap_dir_group;     end interface
       interface print;            module procedure print_dir_group;            end interface
       interface print_short;      module procedure print_short_dir_group;      end interface
       interface export;           module procedure export_dir_group;           end interface
       interface export_primitives;module procedure export_primitives_dir_group;end interface
       interface export_restart;   module procedure export_restart_dir_group;   end interface
       interface import;           module procedure import_dir_group;           end interface
       interface import_restart;   module procedure import_restart_dir_group;   end interface
       interface import_primitives;module procedure import_primitives_dir_group;end interface
       interface export;           module procedure export_wrap_dir_group;      end interface
       interface import;           module procedure import_wrap_dir_group;      end interface
       interface make_restart_dir; module procedure make_restart_dir_dir_group; end interface
       interface suppress_warnings;module procedure suppress_warnings_dir_group;end interface

       type dir_group
         type(path) :: base
         type(path) :: field
         type(path) :: restart
         type(path) :: debug
         type(path) :: energy
         type(path) :: residual
         type(path) :: unsteady
         type(path) :: stats
         type(path) :: BCs
       end type

       contains

       subroutine init_copy_dir_group(this,that)
         implicit none
         type(dir_group),intent(inout) :: this
         type(dir_group),intent(in) :: that
         call delete(this)
         call init(this%base,that%base)
         call init(this%field,that%field)
         call init(this%restart,that%restart)
         call init(this%debug,that%debug)
         call init(this%energy,that%energy)
         call init(this%residual,that%residual)
         call init(this%unsteady,that%unsteady)
         call init(this%stats,that%stats)
         call init(this%BCs,that%BCs)
       end subroutine

       subroutine delete_dir_group(this)
         implicit none
         type(dir_group),intent(inout) :: this
         call delete(this%base)
         call delete(this%field)
         call delete(this%restart)
         call delete(this%debug)
         call delete(this%energy)
         call delete(this%residual)
         call delete(this%unsteady)
         call delete(this%stats)
         call delete(this%BCs)
       end subroutine

       subroutine display_dir_group(this,un)
         implicit none
         type(dir_group),intent(in) :: this
         integer,intent(in) :: un
         call display(this%base,un)
         call display(this%field,un)
         call display(this%restart,un)
         call display(this%debug,un)
         call display(this%energy,un)
         call display(this%residual,un)
         call display(this%unsteady,un)
         call display(this%stats,un)
         call display(this%BCs,un)
       end subroutine

       subroutine display_short_dir_group(this,un)
         implicit none
         type(dir_group),intent(in) :: this
         integer,intent(in) :: un
         call display(this%base,un)
         call display(this%field,un)
         call display(this%restart,un)
         call display(this%debug,un)
         call display(this%energy,un)
         call display(this%residual,un)
         call display(this%unsteady,un)
         call display(this%stats,un)
         call display(this%BCs,un)
       end subroutine

       subroutine display_wrap_dir_group(this,dir,name)
         implicit none
         type(dir_group),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_dir_group(this)
         implicit none
         type(dir_group),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_dir_group(this)
         implicit none
         type(dir_group),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_dir_group(this,un)
         implicit none
         type(dir_group),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_dir_group(this,un)
         implicit none
         type(dir_group),intent(in) :: this
         integer,intent(in) :: un
         call export(this%base,un)
         call export(this%field,un)
         call export(this%restart,un)
         call export(this%debug,un)
         call export(this%energy,un)
         call export(this%residual,un)
         call export(this%unsteady,un)
         call export(this%stats,un)
         call export(this%BCs,un)
       end subroutine

       subroutine import_primitives_dir_group(this,un)
         implicit none
         type(dir_group),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_dir_group(this,un)
         implicit none
         type(dir_group),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%base,un)
         call import(this%field,un)
         call import(this%restart,un)
         call import(this%debug,un)
         call import(this%energy,un)
         call import(this%residual,un)
         call import(this%unsteady,un)
         call import(this%stats,un)
         call import(this%BCs,un)
       end subroutine

       subroutine export_wrap_dir_group(this,dir,name)
         implicit none
         type(dir_group),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_dir_group(this,dir,name)
         implicit none
         type(dir_group),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_dir_group(this,dir)
         implicit none
         type(dir_group),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%base,dir//fortran_PS//'base')
         call make_restart_dir(this%field,dir//fortran_PS//'field')
         call make_restart_dir(this%restart,dir//fortran_PS//'restart')
         call make_restart_dir(this%debug,dir//fortran_PS//'debug')
         call make_restart_dir(this%energy,dir//fortran_PS//'energy')
         call make_restart_dir(this%residual,dir//fortran_PS//'residual')
         call make_restart_dir(this%unsteady,dir//fortran_PS//'unsteady')
         call make_restart_dir(this%stats,dir//fortran_PS//'stats')
         call make_restart_dir(this%BCs,dir//fortran_PS//'BCs')
       end subroutine

       subroutine export_restart_dir_group(this,dir)
         implicit none
         type(dir_group),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%base,dir//fortran_PS//'base')
         call export_restart(this%field,dir//fortran_PS//'field')
         call export_restart(this%restart,dir//fortran_PS//'restart')
         call export_restart(this%debug,dir//fortran_PS//'debug')
         call export_restart(this%energy,dir//fortran_PS//'energy')
         call export_restart(this%residual,dir//fortran_PS//'residual')
         call export_restart(this%unsteady,dir//fortran_PS//'unsteady')
         call export_restart(this%stats,dir//fortran_PS//'stats')
         call export_restart(this%BCs,dir//fortran_PS//'BCs')
       end subroutine

       subroutine import_restart_dir_group(this,dir)
         implicit none
         type(dir_group),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%base,dir//fortran_PS//'base')
         call import_restart(this%field,dir//fortran_PS//'field')
         call import_restart(this%restart,dir//fortran_PS//'restart')
         call import_restart(this%debug,dir//fortran_PS//'debug')
         call import_restart(this%energy,dir//fortran_PS//'energy')
         call import_restart(this%residual,dir//fortran_PS//'residual')
         call import_restart(this%unsteady,dir//fortran_PS//'unsteady')
         call import_restart(this%stats,dir//fortran_PS//'stats')
         call import_restart(this%BCs,dir//fortran_PS//'BCs')
       end subroutine

       subroutine suppress_warnings_dir_group(this)
         implicit none
         type(dir_group),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module