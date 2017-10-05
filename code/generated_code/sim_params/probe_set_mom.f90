       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module probe_set_mom_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use probe_mod
       use string_mod
       implicit none

       private
       public :: probe_set_mom
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_structured,import_structured,&
       import_primitives,export,import,set_IO_dir,make_IO_dir,&
       suppress_warnings

       interface init;             module procedure init_copy_probe_set_mom;          end interface
       interface delete;           module procedure delete_probe_set_mom;             end interface
       interface display;          module procedure display_probe_set_mom;            end interface
       interface display_short;    module procedure display_short_probe_set_mom;      end interface
       interface display;          module procedure display_wrap_probe_set_mom;       end interface
       interface print;            module procedure print_probe_set_mom;              end interface
       interface print_short;      module procedure print_short_probe_set_mom;        end interface
       interface export;           module procedure export_probe_set_mom;             end interface
       interface export_primitives;module procedure export_primitives_probe_set_mom;  end interface
       interface import;           module procedure import_probe_set_mom;             end interface
       interface export_structured;module procedure export_structured_D_probe_set_mom;end interface
       interface import_structured;module procedure import_structured_D_probe_set_mom;end interface
       interface import_primitives;module procedure import_primitives_probe_set_mom;  end interface
       interface export;           module procedure export_wrap_probe_set_mom;        end interface
       interface import;           module procedure import_wrap_probe_set_mom;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_probe_set_mom;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_probe_set_mom;        end interface
       interface suppress_warnings;module procedure suppress_warnings_probe_set_mom;  end interface

       type probe_set_mom
         type(probe) :: probe_KE
         type(probe) :: probe_KE_2C
         type(probe) :: probe_divU
         type(probe) :: probe_Q
       end type

       contains

       subroutine init_copy_probe_set_mom(this,that)
         implicit none
         type(probe_set_mom),intent(inout) :: this
         type(probe_set_mom),intent(in) :: that
         call delete(this)
         call init(this%probe_KE,that%probe_KE)
         call init(this%probe_KE_2C,that%probe_KE_2C)
         call init(this%probe_divU,that%probe_divU)
         call init(this%probe_Q,that%probe_Q)
       end subroutine

       subroutine delete_probe_set_mom(this)
         implicit none
         type(probe_set_mom),intent(inout) :: this
         call delete(this%probe_KE)
         call delete(this%probe_KE_2C)
         call delete(this%probe_divU)
         call delete(this%probe_Q)
       end subroutine

       subroutine display_probe_set_mom(this,un)
         implicit none
         type(probe_set_mom),intent(in) :: this
         integer,intent(in) :: un
         call display(this%probe_KE,un)
         call display(this%probe_KE_2C,un)
         call display(this%probe_divU,un)
         call display(this%probe_Q,un)
       end subroutine

       subroutine display_short_probe_set_mom(this,un)
         implicit none
         type(probe_set_mom),intent(in) :: this
         integer,intent(in) :: un
         call display(this%probe_KE,un)
         call display(this%probe_KE_2C,un)
         call display(this%probe_divU,un)
         call display(this%probe_Q,un)
       end subroutine

       subroutine display_wrap_probe_set_mom(this,dir,name)
         implicit none
         type(probe_set_mom),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_probe_set_mom(this)
         implicit none
         type(probe_set_mom),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_probe_set_mom(this)
         implicit none
         type(probe_set_mom),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_probe_set_mom(this,un)
         implicit none
         type(probe_set_mom),intent(in) :: this
         integer,intent(in) :: un
         call export(this%probe_KE,un)
         call export(this%probe_KE_2C,un)
         call export(this%probe_divU,un)
         call export(this%probe_Q,un)
       end subroutine

       subroutine import_probe_set_mom(this,un)
         implicit none
         type(probe_set_mom),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%probe_KE,un)
         call import(this%probe_KE_2C,un)
         call import(this%probe_divU,un)
         call import(this%probe_Q,un)
       end subroutine

       subroutine export_primitives_probe_set_mom(this,un)
         implicit none
         type(probe_set_mom),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_probe_set_mom(this,un)
         implicit none
         type(probe_set_mom),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_wrap_probe_set_mom(this,dir,name)
         implicit none
         type(probe_set_mom),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_probe_set_mom(this,dir,name)
         implicit none
         type(probe_set_mom),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_probe_set_mom(this,dir)
         implicit none
         type(probe_set_mom),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%probe_KE,dir//'probe_KE'//fortran_PS)
         call set_IO_dir(this%probe_KE_2C,dir//'probe_KE_2C'//fortran_PS)
         call set_IO_dir(this%probe_divU,dir//'probe_divU'//fortran_PS)
         call set_IO_dir(this%probe_Q,dir//'probe_Q'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_probe_set_mom(this,dir)
         implicit none
         type(probe_set_mom),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%probe_KE,dir//'probe_KE'//fortran_PS)
         call make_IO_dir(this%probe_KE_2C,dir//'probe_KE_2C'//fortran_PS)
         call make_IO_dir(this%probe_divU,dir//'probe_divU'//fortran_PS)
         call make_IO_dir(this%probe_Q,dir//'probe_Q'//fortran_PS)
       end subroutine

       subroutine export_structured_D_probe_set_mom(this,dir)
         implicit none
         type(probe_set_mom),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%probe_KE,dir//'probe_KE'//fortran_PS)
         call export_structured(this%probe_KE_2C,&
         dir//'probe_KE_2C'//fortran_PS)
         call export_structured(this%probe_divU,&
         dir//'probe_divU'//fortran_PS)
         call export_structured(this%probe_Q,dir//'probe_Q'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_probe_set_mom(this,dir)
         implicit none
         type(probe_set_mom),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call delete(this)
         call import_primitives(this,un)
         call import_structured(this%probe_KE,dir//'probe_KE'//fortran_PS)
         call import_structured(this%probe_KE_2C,&
         dir//'probe_KE_2C'//fortran_PS)
         call import_structured(this%probe_divU,&
         dir//'probe_divU'//fortran_PS)
         call import_structured(this%probe_Q,dir//'probe_Q'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_probe_set_mom(this)
         implicit none
         type(probe_set_mom),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module