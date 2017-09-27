       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module physical_sub_domain_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       use sub_domain_mod
       implicit none

       private
       public :: physical_sub_domain
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_physical_sub_domain;        end interface
       interface delete;           module procedure delete_physical_sub_domain;           end interface
       interface display;          module procedure display_physical_sub_domain;          end interface
       interface display_short;    module procedure display_short_physical_sub_domain;    end interface
       interface display;          module procedure display_wrap_physical_sub_domain;     end interface
       interface print;            module procedure print_physical_sub_domain;            end interface
       interface print_short;      module procedure print_short_physical_sub_domain;      end interface
       interface export;           module procedure export_physical_sub_domain;           end interface
       interface export_primitives;module procedure export_primitives_physical_sub_domain;end interface
       interface export_restart;   module procedure export_restart_physical_sub_domain;   end interface
       interface import;           module procedure import_physical_sub_domain;           end interface
       interface import_restart;   module procedure import_restart_physical_sub_domain;   end interface
       interface import_primitives;module procedure import_primitives_physical_sub_domain;end interface
       interface export;           module procedure export_wrap_physical_sub_domain;      end interface
       interface import;           module procedure import_wrap_physical_sub_domain;      end interface
       interface make_restart_dir; module procedure make_restart_dir_physical_sub_domain; end interface
       interface suppress_warnings;module procedure suppress_warnings_physical_sub_domain;end interface

       type physical_sub_domain
         type(sub_domain) :: total
         type(sub_domain) :: physical
         logical :: defined = .false.
       end type

       contains

       subroutine init_copy_physical_sub_domain(this,that)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         type(physical_sub_domain),intent(in) :: that
         call delete(this)
         call init(this%total,that%total)
         call init(this%physical,that%physical)
         this%defined = that%defined
       end subroutine

       subroutine delete_physical_sub_domain(this)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         call delete(this%total)
         call delete(this%physical)
         this%defined = .false.
       end subroutine

       subroutine display_physical_sub_domain(this,un)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         integer,intent(in) :: un
         call display(this%total,un)
         call display(this%physical,un)
         write(un,*) 'defined  = ',this%defined
       end subroutine

       subroutine display_short_physical_sub_domain(this,un)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         integer,intent(in) :: un
         call display(this%total,un)
         call display(this%physical,un)
         write(un,*) 'defined  = ',this%defined
       end subroutine

       subroutine display_wrap_physical_sub_domain(this,dir,name)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_physical_sub_domain(this)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_physical_sub_domain(this)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_physical_sub_domain(this,un)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'defined   = ';write(un,*) this%defined
       end subroutine

       subroutine export_physical_sub_domain(this,un)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         integer,intent(in) :: un
         call export(this%total,un)
         call export(this%physical,un)
         write(un,*) 'defined   = ';write(un,*) this%defined
       end subroutine

       subroutine import_primitives_physical_sub_domain(this,un)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%defined
       end subroutine

       subroutine import_physical_sub_domain(this,un)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%total,un)
         call import(this%physical,un)
         read(un,*); read(un,*) this%defined
       end subroutine

       subroutine export_wrap_physical_sub_domain(this,dir,name)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_physical_sub_domain(this,dir,name)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_physical_sub_domain(this,dir)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%total,dir//fortran_PS//'total')
         call make_restart_dir(this%physical,dir//fortran_PS//'physical')
       end subroutine

       subroutine export_restart_physical_sub_domain(this,dir)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%total,dir//fortran_PS//'total')
         call export_restart(this%physical,dir//fortran_PS//'physical')
       end subroutine

       subroutine import_restart_physical_sub_domain(this,dir)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%total,dir//fortran_PS//'total')
         call import_restart(this%physical,dir//fortran_PS//'physical')
       end subroutine

       subroutine suppress_warnings_physical_sub_domain(this)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module