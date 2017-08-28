       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module mesh_domain_mod
       use IO_tools_mod
       use mesh_mod
       use physical_domain_mod
       implicit none

       private
       public :: mesh_domain
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_mesh_domain;           end interface
       interface delete; module procedure delete_mesh_domain;         end interface
       interface display;module procedure display_mesh_domain;        end interface
       interface display;module procedure display_wrapper_mesh_domain;end interface
       interface print;  module procedure print_mesh_domain;          end interface
       interface export; module procedure export_mesh_domain;         end interface
       interface import; module procedure import_mesh_domain;         end interface
       interface export; module procedure export_wrapper_mesh_domain; end interface
       interface import; module procedure import_wrapper_mesh_domain; end interface

       type mesh_domain
         type(physical_domain) :: d
         type(mesh) :: m_r1
         type(mesh) :: m_r2
       end type

       contains

       subroutine init_mesh_domain(this,that)
         implicit none
         type(mesh_domain),intent(inout) :: this
         type(mesh_domain),intent(in) :: that
         call delete(this)
         call init(this%d,that%d)
         call init(this%m_r1,that%m_r1)
         call init(this%m_r2,that%m_r2)
       end subroutine

       subroutine delete_mesh_domain(this)
         implicit none
         type(mesh_domain),intent(inout) :: this
         call delete(this%d)
         call delete(this%m_r1)
         call delete(this%m_r2)
       end subroutine

       subroutine display_mesh_domain(this,un)
         implicit none
         type(mesh_domain),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- mesh_domain'
         call display(this%d,un)
         call display(this%m_r1,un)
         call display(this%m_r2,un)
       end subroutine

       subroutine print_mesh_domain(this)
         implicit none
         type(mesh_domain),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_mesh_domain(this,un)
         implicit none
         type(mesh_domain),intent(in) :: this
         integer,intent(in) :: un
         call export(this%d,un)
         call export(this%m_r1,un)
         call export(this%m_r2,un)
       end subroutine

       subroutine import_mesh_domain(this,un)
         implicit none
         type(mesh_domain),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%d,un)
         call import(this%m_r1,un)
         call import(this%m_r2,un)
       end subroutine

       subroutine display_wrapper_mesh_domain(this,dir,name)
         implicit none
         type(mesh_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_mesh_domain(this,dir,name)
         implicit none
         type(mesh_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_mesh_domain(this,dir,name)
         implicit none
         type(mesh_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module