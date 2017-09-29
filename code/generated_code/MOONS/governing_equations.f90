       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module governing_equations_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use energy_mod
       use induction_mod
       use momentum_mod
       use string_mod
       implicit none

       private
       public :: governing_equations
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_governing_equations;          end interface
       interface delete;           module procedure delete_governing_equations;             end interface
       interface display;          module procedure display_governing_equations;            end interface
       interface display_short;    module procedure display_short_governing_equations;      end interface
       interface display;          module procedure display_wrap_governing_equations;       end interface
       interface print;            module procedure print_governing_equations;              end interface
       interface print_short;      module procedure print_short_governing_equations;        end interface
       interface export;           module procedure export_governing_equations;             end interface
       interface export_primitives;module procedure export_primitives_governing_equations;  end interface
       interface import;           module procedure import_governing_equations;             end interface
       interface export_structured;module procedure export_structured_D_governing_equations;end interface
       interface import_structured;module procedure import_structured_D_governing_equations;end interface
       interface import_primitives;module procedure import_primitives_governing_equations;  end interface
       interface export;           module procedure export_wrap_governing_equations;        end interface
       interface import;           module procedure import_wrap_governing_equations;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_governing_equations;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_governing_equations;        end interface
       interface suppress_warnings;module procedure suppress_warnings_governing_equations;  end interface

       type governing_equations
         type(momentum) :: mom
         type(induction) :: ind
         type(energy) :: nrg
       end type

       contains

       subroutine init_copy_governing_equations(this,that)
         implicit none
         type(governing_equations),intent(inout) :: this
         type(governing_equations),intent(in) :: that
         call delete(this)
         call init(this%mom,that%mom)
         call init(this%ind,that%ind)
         call init(this%nrg,that%nrg)
       end subroutine

       subroutine delete_governing_equations(this)
         implicit none
         type(governing_equations),intent(inout) :: this
         call delete(this%mom)
         call delete(this%ind)
         call delete(this%nrg)
       end subroutine

       subroutine display_governing_equations(this,un)
         implicit none
         type(governing_equations),intent(in) :: this
         integer,intent(in) :: un
         call display(this%mom,un)
         call display(this%ind,un)
         call display(this%nrg,un)
       end subroutine

       subroutine display_short_governing_equations(this,un)
         implicit none
         type(governing_equations),intent(in) :: this
         integer,intent(in) :: un
         call display(this%mom,un)
         call display(this%ind,un)
         call display(this%nrg,un)
       end subroutine

       subroutine display_wrap_governing_equations(this,dir,name)
         implicit none
         type(governing_equations),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_governing_equations(this)
         implicit none
         type(governing_equations),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_governing_equations(this)
         implicit none
         type(governing_equations),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_governing_equations(this,un)
         implicit none
         type(governing_equations),intent(in) :: this
         integer,intent(in) :: un
         call export(this%mom,un)
         call export(this%ind,un)
         call export(this%nrg,un)
       end subroutine

       subroutine import_governing_equations(this,un)
         implicit none
         type(governing_equations),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%mom,un)
         call import(this%ind,un)
         call import(this%nrg,un)
       end subroutine

       subroutine export_primitives_governing_equations(this,un)
         implicit none
         type(governing_equations),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_governing_equations(this,un)
         implicit none
         type(governing_equations),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_wrap_governing_equations(this,dir,name)
         implicit none
         type(governing_equations),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_governing_equations(this,dir,name)
         implicit none
         type(governing_equations),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_governing_equations(this,dir)
         implicit none
         type(governing_equations),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%mom,dir//'mom'//fortran_PS)
         call set_IO_dir(this%ind,dir//'ind'//fortran_PS)
         call set_IO_dir(this%nrg,dir//'nrg'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_governing_equations(this,dir)
         implicit none
         type(governing_equations),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%mom,dir//'mom'//fortran_PS)
         call make_IO_dir(this%ind,dir//'ind'//fortran_PS)
         call make_IO_dir(this%nrg,dir//'nrg'//fortran_PS)
       end subroutine

       subroutine export_structured_D_governing_equations(this,dir)
         implicit none
         type(governing_equations),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_structured(this%mom,dir//'mom'//fortran_PS)
         call export_structured(this%ind,dir//'ind'//fortran_PS)
         call export_structured(this%nrg,dir//'nrg'//fortran_PS)
       end subroutine

       subroutine import_structured_D_governing_equations(this,dir)
         implicit none
         type(governing_equations),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_structured(this%mom,dir//'mom'//fortran_PS)
         call import_structured(this%ind,dir//'ind'//fortran_PS)
         call import_structured(this%nrg,dir//'nrg'//fortran_PS)
       end subroutine

       subroutine suppress_warnings_governing_equations(this)
         implicit none
         type(governing_equations),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module