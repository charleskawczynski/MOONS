       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module mesh_domain_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use mesh_mod
       use physical_domain_mod
       use string_mod
       implicit none

       private
       public :: mesh_domain
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_mesh_domain;          end interface
       interface delete;           module procedure delete_mesh_domain;             end interface
       interface display;          module procedure display_mesh_domain;            end interface
       interface display_short;    module procedure display_short_mesh_domain;      end interface
       interface display;          module procedure display_wrap_mesh_domain;       end interface
       interface print;            module procedure print_mesh_domain;              end interface
       interface print_short;      module procedure print_short_mesh_domain;        end interface
       interface export;           module procedure export_mesh_domain;             end interface
       interface export_primitives;module procedure export_primitives_mesh_domain;  end interface
       interface import;           module procedure import_mesh_domain;             end interface
       interface export_structured;module procedure export_structured_D_mesh_domain;end interface
       interface import_structured;module procedure import_structured_D_mesh_domain;end interface
       interface import_primitives;module procedure import_primitives_mesh_domain;  end interface
       interface export;           module procedure export_wrap_mesh_domain;        end interface
       interface import;           module procedure import_wrap_mesh_domain;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_mesh_domain;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_mesh_domain;        end interface
       interface suppress_warnings;module procedure suppress_warnings_mesh_domain;  end interface

       type mesh_domain
         type(physical_domain) :: D
         type(mesh) :: m_R1
         type(mesh) :: m_R2
       end type

       contains

       subroutine init_copy_mesh_domain(this,that)
         implicit none
         type(mesh_domain),intent(inout) :: this
         type(mesh_domain),intent(in) :: that
         call delete(this)
         call init(this%D,that%D)
         call init(this%m_R1,that%m_R1)
         call init(this%m_R2,that%m_R2)
       end subroutine

       subroutine delete_mesh_domain(this)
         implicit none
         type(mesh_domain),intent(inout) :: this
         call delete(this%D)
         call delete(this%m_R1)
         call delete(this%m_R2)
       end subroutine

       subroutine display_mesh_domain(this,un)
         implicit none
         type(mesh_domain),intent(in) :: this
         integer,intent(in) :: un
         call display(this%D,un)
         call display(this%m_R1,un)
         call display(this%m_R2,un)
       end subroutine

       subroutine display_short_mesh_domain(this,un)
         implicit none
         type(mesh_domain),intent(in) :: this
         integer,intent(in) :: un
         call display(this%D,un)
         call display(this%m_R1,un)
         call display(this%m_R2,un)
       end subroutine

       subroutine display_wrap_mesh_domain(this,dir,name)
         implicit none
         type(mesh_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_mesh_domain(this)
         implicit none
         type(mesh_domain),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_mesh_domain(this)
         implicit none
         type(mesh_domain),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_mesh_domain(this,un)
         implicit none
         type(mesh_domain),intent(in) :: this
         integer,intent(in) :: un
         call export(this%D,un)
         call export(this%m_R1,un)
         call export(this%m_R2,un)
       end subroutine

       subroutine import_mesh_domain(this,un)
         implicit none
         type(mesh_domain),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%D,un)
         call import(this%m_R1,un)
         call import(this%m_R2,un)
       end subroutine

       subroutine export_primitives_mesh_domain(this,un)
         implicit none
         type(mesh_domain),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_mesh_domain(this,un)
         implicit none
         type(mesh_domain),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_wrap_mesh_domain(this,dir,name)
         implicit none
         type(mesh_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_mesh_domain(this,dir,name)
         implicit none
         type(mesh_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_mesh_domain(this,dir)
         implicit none
         type(mesh_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%D,dir//'D'//fortran_PS)
         call set_IO_dir(this%m_R1,dir//'m_R1'//fortran_PS)
         call set_IO_dir(this%m_R2,dir//'m_R2'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_mesh_domain(this,dir)
         implicit none
         type(mesh_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir(dir)
         call make_IO_dir(this%D,dir//'D'//fortran_PS)
         call make_IO_dir(this%m_R1,dir//'m_R1'//fortran_PS)
         call make_IO_dir(this%m_R2,dir//'m_R2'//fortran_PS)
       end subroutine

       subroutine export_structured_D_mesh_domain(this,dir)
         implicit none
         type(mesh_domain),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Exporting mesh_domain structured'
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_structured(this%D,dir//'D'//fortran_PS)
         call export_structured(this%m_R1,dir//'m_R1'//fortran_PS)
         call export_structured(this%m_R2,dir//'m_R2'//fortran_PS)
       end subroutine

       subroutine import_structured_D_mesh_domain(this,dir)
         implicit none
         type(mesh_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Importing mesh_domain structured'
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_structured(this%D,dir//'D'//fortran_PS)
         call import_structured(this%m_R1,dir//'m_R1'//fortran_PS)
         call import_structured(this%m_R2,dir//'m_R2'//fortran_PS)
       end subroutine

       subroutine suppress_warnings_mesh_domain(this)
         implicit none
         type(mesh_domain),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module