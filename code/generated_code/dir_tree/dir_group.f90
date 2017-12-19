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
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_structured,import_structured,&
       import_primitives,export,import,set_IO_dir,make_IO_dir,&
       suppress_warnings

       interface init;             module procedure init_copy_dir_group;          end interface
       interface delete;           module procedure delete_dir_group;             end interface
       interface display;          module procedure display_dir_group;            end interface
       interface display_short;    module procedure display_short_dir_group;      end interface
       interface display;          module procedure display_wrap_dir_group;       end interface
       interface print;            module procedure print_dir_group;              end interface
       interface print_short;      module procedure print_short_dir_group;        end interface
       interface export;           module procedure export_dir_group;             end interface
       interface export_primitives;module procedure export_primitives_dir_group;  end interface
       interface import;           module procedure import_dir_group;             end interface
       interface export_structured;module procedure export_structured_D_dir_group;end interface
       interface import_structured;module procedure import_structured_D_dir_group;end interface
       interface import_primitives;module procedure import_primitives_dir_group;  end interface
       interface export;           module procedure export_wrap_dir_group;        end interface
       interface import;           module procedure import_wrap_dir_group;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_dir_group;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_dir_group;        end interface
       interface suppress_warnings;module procedure suppress_warnings_dir_group;  end interface

       type dir_group
         type(path) :: base
         type(path) :: field
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

       subroutine export_dir_group(this,un)
         implicit none
         type(dir_group),intent(in) :: this
         integer,intent(in) :: un
         call export(this%base,un)
         call export(this%field,un)
         call export(this%debug,un)
         call export(this%energy,un)
         call export(this%residual,un)
         call export(this%unsteady,un)
         call export(this%stats,un)
         call export(this%BCs,un)
       end subroutine

       subroutine import_dir_group(this,un)
         implicit none
         type(dir_group),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%base,un)
         call import(this%field,un)
         call import(this%debug,un)
         call import(this%energy,un)
         call import(this%residual,un)
         call import(this%unsteady,un)
         call import(this%stats,un)
         call import(this%BCs,un)
       end subroutine

       subroutine export_primitives_dir_group(this,un)
         implicit none
         type(dir_group),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_dir_group(this,un)
         implicit none
         type(dir_group),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
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
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_dir_group(this,dir)
         implicit none
         type(dir_group),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%base,dir//'base'//fortran_PS)
         call set_IO_dir(this%field,dir//'field'//fortran_PS)
         call set_IO_dir(this%debug,dir//'debug'//fortran_PS)
         call set_IO_dir(this%energy,dir//'energy'//fortran_PS)
         call set_IO_dir(this%residual,dir//'residual'//fortran_PS)
         call set_IO_dir(this%unsteady,dir//'unsteady'//fortran_PS)
         call set_IO_dir(this%stats,dir//'stats'//fortran_PS)
         call set_IO_dir(this%BCs,dir//'BCs'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_dir_group(this,dir)
         implicit none
         type(dir_group),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%base,dir//'base'//fortran_PS)
         call make_IO_dir(this%field,dir//'field'//fortran_PS)
         call make_IO_dir(this%debug,dir//'debug'//fortran_PS)
         call make_IO_dir(this%energy,dir//'energy'//fortran_PS)
         call make_IO_dir(this%residual,dir//'residual'//fortran_PS)
         call make_IO_dir(this%unsteady,dir//'unsteady'//fortran_PS)
         call make_IO_dir(this%stats,dir//'stats'//fortran_PS)
         call make_IO_dir(this%BCs,dir//'BCs'//fortran_PS)
       end subroutine

       subroutine export_structured_D_dir_group(this,dir)
         implicit none
         type(dir_group),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%base,dir//'base'//fortran_PS)
         call export_structured(this%field,dir//'field'//fortran_PS)
         call export_structured(this%debug,dir//'debug'//fortran_PS)
         call export_structured(this%energy,dir//'energy'//fortran_PS)
         call export_structured(this%residual,dir//'residual'//fortran_PS)
         call export_structured(this%unsteady,dir//'unsteady'//fortran_PS)
         call export_structured(this%stats,dir//'stats'//fortran_PS)
         call export_structured(this%BCs,dir//'BCs'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_dir_group(this,dir)
         implicit none
         type(dir_group),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         call import_structured(this%base,dir//'base'//fortran_PS)
         call import_structured(this%field,dir//'field'//fortran_PS)
         call import_structured(this%debug,dir//'debug'//fortran_PS)
         call import_structured(this%energy,dir//'energy'//fortran_PS)
         call import_structured(this%residual,dir//'residual'//fortran_PS)
         call import_structured(this%unsteady,dir//'unsteady'//fortran_PS)
         call import_structured(this%stats,dir//'stats'//fortran_PS)
         call import_structured(this%BCs,dir//'BCs'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_dir_group(this)
         implicit none
         type(dir_group),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module