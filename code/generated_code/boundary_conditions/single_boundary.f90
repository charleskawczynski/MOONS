       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module single_boundary_mod
       use IO_tools_mod
       use bctype_mod
       use bctype_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use grid_field_mod
       use string_mod
       implicit none

       private
       public :: single_boundary
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_single_boundary;        end interface
       interface delete;           module procedure delete_single_boundary;           end interface
       interface display;          module procedure display_single_boundary;          end interface
       interface display_short;    module procedure display_short_single_boundary;    end interface
       interface display;          module procedure display_wrap_single_boundary;     end interface
       interface print;            module procedure print_single_boundary;            end interface
       interface print_short;      module procedure print_short_single_boundary;      end interface
       interface export;           module procedure export_single_boundary;           end interface
       interface export_primitives;module procedure export_primitives_single_boundary;end interface
       interface export_restart;   module procedure export_restart_single_boundary;   end interface
       interface import;           module procedure import_single_boundary;           end interface
       interface import_restart;   module procedure import_restart_single_boundary;   end interface
       interface import_primitives;module procedure import_primitives_single_boundary;end interface
       interface export;           module procedure export_wrap_single_boundary;      end interface
       interface import;           module procedure import_wrap_single_boundary;      end interface
       interface make_restart_dir; module procedure make_restart_dir_single_boundary; end interface
       interface suppress_warnings;module procedure suppress_warnings_single_boundary;end interface

       type single_boundary
         type(bctype) :: bct
         type(grid_field) :: b
         type(grid_field) :: b_modified
         type(grid_field) :: b_total
       end type

       contains

       subroutine init_copy_single_boundary(this,that)
         implicit none
         type(single_boundary),intent(inout) :: this
         type(single_boundary),intent(in) :: that
         call delete(this)
         call init(this%bct,that%bct)
         call init(this%b,that%b)
         call init(this%b_modified,that%b_modified)
         call init(this%b_total,that%b_total)
       end subroutine

       subroutine delete_single_boundary(this)
         implicit none
         type(single_boundary),intent(inout) :: this
         call delete(this%bct)
         call delete(this%b)
         call delete(this%b_modified)
         call delete(this%b_total)
       end subroutine

       subroutine display_single_boundary(this,un)
         implicit none
         type(single_boundary),intent(in) :: this
         integer,intent(in) :: un
         call display(this%bct,un)
         call display(this%b,un)
         call display(this%b_modified,un)
         call display(this%b_total,un)
       end subroutine

       subroutine display_short_single_boundary(this,un)
         implicit none
         type(single_boundary),intent(in) :: this
         integer,intent(in) :: un
         call display(this%bct,un)
         call display(this%b,un)
         call display(this%b_modified,un)
         call display(this%b_total,un)
       end subroutine

       subroutine display_wrap_single_boundary(this,dir,name)
         implicit none
         type(single_boundary),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_single_boundary(this)
         implicit none
         type(single_boundary),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_single_boundary(this)
         implicit none
         type(single_boundary),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_single_boundary(this,un)
         implicit none
         type(single_boundary),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_single_boundary(this,un)
         implicit none
         type(single_boundary),intent(in) :: this
         integer,intent(in) :: un
         call export(this%bct,un)
         call export(this%b,un)
         call export(this%b_modified,un)
         call export(this%b_total,un)
       end subroutine

       subroutine import_primitives_single_boundary(this,un)
         implicit none
         type(single_boundary),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_single_boundary(this,un)
         implicit none
         type(single_boundary),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%bct,un)
         call import(this%b,un)
         call import(this%b_modified,un)
         call import(this%b_total,un)
       end subroutine

       subroutine export_wrap_single_boundary(this,dir,name)
         implicit none
         type(single_boundary),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_single_boundary(this,dir,name)
         implicit none
         type(single_boundary),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_single_boundary(this,dir)
         implicit none
         type(single_boundary),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%bct,dir//'bct'//fortran_PS)
         call make_restart_dir(this%b,dir//'b'//fortran_PS)
         call make_restart_dir(this%b_modified,dir//'b_modified'//fortran_PS)
         call make_restart_dir(this%b_total,dir//'b_total'//fortran_PS)
       end subroutine

       subroutine export_restart_single_boundary(this,dir)
         implicit none
         type(single_boundary),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%bct,dir//'bct'//fortran_PS)
         call export_restart(this%b,dir//'b'//fortran_PS)
         call export_restart(this%b_modified,dir//'b_modified'//fortran_PS)
         call export_restart(this%b_total,dir//'b_total'//fortran_PS)
       end subroutine

       subroutine import_restart_single_boundary(this,dir)
         implicit none
         type(single_boundary),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%bct,dir//'bct'//fortran_PS)
         call import_restart(this%b,dir//'b'//fortran_PS)
         call import_restart(this%b_modified,dir//'b_modified'//fortran_PS)
         call import_restart(this%b_total,dir//'b_total'//fortran_PS)
       end subroutine

       subroutine suppress_warnings_single_boundary(this)
         implicit none
         type(single_boundary),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module