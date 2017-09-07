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
       public :: display_short,print_short

       interface init;         module procedure init_copy_me;    end interface
       interface delete;       module procedure delete_me;       end interface
       interface display;      module procedure display_me;      end interface
       interface display_short;module procedure display_short_me;end interface
       interface display;      module procedure display_wrap_me; end interface
       interface print;        module procedure print_me;        end interface
       interface print_short;  module procedure print_short_me;  end interface
       interface export;       module procedure export_me;       end interface
       interface import;       module procedure import_me;       end interface
       interface export;       module procedure export_wrap_me;  end interface
       interface import;       module procedure import_wrap_me;  end interface

       type mesh_domain
         type(physical_domain) :: D
         type(mesh) :: m_R1
         type(mesh) :: m_R2
       end type

       contains

       subroutine init_copy_me(this,that)
         implicit none
         type(mesh_domain),intent(inout) :: this
         type(mesh_domain),intent(in) :: that
         call delete(this)
         call init(this%D,that%D)
         call init(this%m_R1,that%m_R1)
         call init(this%m_R2,that%m_R2)
       end subroutine

       subroutine delete_me(this)
         implicit none
         type(mesh_domain),intent(inout) :: this
         call delete(this%D)
         call delete(this%m_R1)
         call delete(this%m_R2)
       end subroutine

       subroutine display_me(this,un)
         implicit none
         type(mesh_domain),intent(in) :: this
         integer,intent(in) :: un
         call display(this%D,un)
         call display(this%m_R1,un)
         call display(this%m_R2,un)
       end subroutine

       subroutine display_short_me(this,un)
         implicit none
         type(mesh_domain),intent(in) :: this
         integer,intent(in) :: un
         call display(this%D,un)
         call display(this%m_R1,un)
         call display(this%m_R2,un)
       end subroutine

       subroutine print_me(this)
         implicit none
         type(mesh_domain),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_me(this)
         implicit none
         type(mesh_domain),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_me(this,un)
         implicit none
         type(mesh_domain),intent(in) :: this
         integer,intent(in) :: un
         call export(this%D,un)
         call export(this%m_R1,un)
         call export(this%m_R2,un)
       end subroutine

       subroutine import_me(this,un)
         implicit none
         type(mesh_domain),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%D,un)
         call import(this%m_R1,un)
         call import(this%m_R2,un)
       end subroutine

       subroutine display_wrap_me(this,dir,name)
         implicit none
         type(mesh_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_me(this,dir,name)
         implicit none
         type(mesh_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_me(this,dir,name)
         implicit none
         type(mesh_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module