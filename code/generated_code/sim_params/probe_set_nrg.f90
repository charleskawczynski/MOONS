       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module probe_set_nrg_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use probe_mod
       use string_mod
       implicit none

       private
       public :: probe_set_nrg
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_probe_set_nrg;          end interface
       interface delete;           module procedure delete_probe_set_nrg;             end interface
       interface display;          module procedure display_probe_set_nrg;            end interface
       interface display_short;    module procedure display_short_probe_set_nrg;      end interface
       interface display;          module procedure display_wrap_probe_set_nrg;       end interface
       interface print;            module procedure print_probe_set_nrg;              end interface
       interface print_short;      module procedure print_short_probe_set_nrg;        end interface
       interface export;           module procedure export_probe_set_nrg;             end interface
       interface export_primitives;module procedure export_primitives_probe_set_nrg;  end interface
       interface import;           module procedure import_probe_set_nrg;             end interface
       interface export_structured;module procedure export_structured_D_probe_set_nrg;end interface
       interface import_structured;module procedure import_structured_D_probe_set_nrg;end interface
       interface import_primitives;module procedure import_primitives_probe_set_nrg;  end interface
       interface export;           module procedure export_wrap_probe_set_nrg;        end interface
       interface import;           module procedure import_wrap_probe_set_nrg;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_probe_set_nrg;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_probe_set_nrg;        end interface
       interface suppress_warnings;module procedure suppress_warnings_probe_set_nrg;  end interface

       type probe_set_nrg
         type(probe) :: probe_divQ
       end type

       contains

       subroutine init_copy_probe_set_nrg(this,that)
         implicit none
         type(probe_set_nrg),intent(inout) :: this
         type(probe_set_nrg),intent(in) :: that
         call delete(this)
         call init(this%probe_divQ,that%probe_divQ)
       end subroutine

       subroutine delete_probe_set_nrg(this)
         implicit none
         type(probe_set_nrg),intent(inout) :: this
         call delete(this%probe_divQ)
       end subroutine

       subroutine display_probe_set_nrg(this,un)
         implicit none
         type(probe_set_nrg),intent(in) :: this
         integer,intent(in) :: un
         call display(this%probe_divQ,un)
       end subroutine

       subroutine display_short_probe_set_nrg(this,un)
         implicit none
         type(probe_set_nrg),intent(in) :: this
         integer,intent(in) :: un
         call display(this%probe_divQ,un)
       end subroutine

       subroutine display_wrap_probe_set_nrg(this,dir,name)
         implicit none
         type(probe_set_nrg),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_probe_set_nrg(this)
         implicit none
         type(probe_set_nrg),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_probe_set_nrg(this)
         implicit none
         type(probe_set_nrg),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_probe_set_nrg(this,un)
         implicit none
         type(probe_set_nrg),intent(in) :: this
         integer,intent(in) :: un
         call export(this%probe_divQ,un)
       end subroutine

       subroutine import_probe_set_nrg(this,un)
         implicit none
         type(probe_set_nrg),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%probe_divQ,un)
       end subroutine

       subroutine export_primitives_probe_set_nrg(this,un)
         implicit none
         type(probe_set_nrg),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_probe_set_nrg(this,un)
         implicit none
         type(probe_set_nrg),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_wrap_probe_set_nrg(this,dir,name)
         implicit none
         type(probe_set_nrg),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_probe_set_nrg(this,dir,name)
         implicit none
         type(probe_set_nrg),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_probe_set_nrg(this,dir)
         implicit none
         type(probe_set_nrg),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%probe_divQ,dir//'probe_divQ'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_probe_set_nrg(this,dir)
         implicit none
         type(probe_set_nrg),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%probe_divQ,dir//'probe_divQ'//fortran_PS)
       end subroutine

       subroutine export_structured_D_probe_set_nrg(this,dir)
         implicit none
         type(probe_set_nrg),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%probe_divQ,&
         dir//'probe_divQ'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_probe_set_nrg(this,dir)
         implicit none
         type(probe_set_nrg),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         call import_structured(this%probe_divQ,&
         dir//'probe_divQ'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_probe_set_nrg(this)
         implicit none
         type(probe_set_nrg),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module