       module mesh_domain_mod
       use IO_tools_mod
       use domain_mod
       use subdomain_mod
       use mesh_mod
       implicit none

       private
       public :: mesh_domain
       public :: init,delete,display,print,export,import ! Essentials
       public :: add,init_other

       interface init;        module procedure init_mesh_domain;           end interface
       interface init;        module procedure init_mesh_domain_copy;      end interface
       interface delete;      module procedure delete_mesh_domain;         end interface
       interface display;     module procedure display_mesh_domain;        end interface
       interface print;       module procedure print_mesh_domain;          end interface
       interface export;      module procedure export_mesh_domain;         end interface
       interface import;      module procedure import_mesh_domain;         end interface
       interface export;      module procedure export_mesh_domain_wrapper; end interface
       interface import;      module procedure import_mesh_domain_wrapper; end interface

       interface add;         module procedure add_subdomain;              end interface
       interface init_other;  module procedure init_other_mesh_domain;     end interface

       type mesh_domain
         type(domain) :: D
         type(mesh) :: m_R1,m_R2
       end type

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_mesh_domain(MD,m_R1,m_R2)
         implicit none
         type(mesh_domain),intent(inout) :: MD
         type(mesh),intent(in) :: m_R1,m_R2
         call init(MD%D,m_R1,m_R2)
         call init(MD%m_R1,m_R1)
         call init(MD%m_R2,m_R2)
       end subroutine

       subroutine init_mesh_domain_copy(MD_out,MD_in)
         implicit none
         type(mesh_domain),intent(inout) :: MD_out
         type(mesh_domain),intent(in) :: MD_in
         call init(MD_out%D,MD_in%D)
         call init(MD_out%m_R1,MD_in%m_R1)
         call init(MD_out%m_R2,MD_in%m_R2)
       end subroutine

       subroutine delete_mesh_domain(MD)
         implicit none
         type(mesh_domain),intent(inout) :: MD
         call delete(MD%D)
         call delete(MD%m_R1)
         call delete(MD%m_R2)
       end subroutine

       subroutine print_mesh_domain(MD,name)
         implicit none
         type(mesh_domain),intent(in) :: MD
         character(len=*),intent(in) :: name
         call print(MD%D,name)
       end subroutine

       subroutine display_mesh_domain(MD,dir,name)
         implicit none
         type(mesh_domain),intent(inout) :: MD
         character(len=*),intent(in) :: dir,name
         call display(MD%D,dir,name)
       end subroutine

       subroutine export_mesh_domain(MD,un)
         implicit none
         type(mesh_domain),intent(in) :: MD
         integer,intent(in) :: un
         call export(MD%D,un)
         call export(MD%m_R2,un)
         call export(MD%m_R1,un)
       end subroutine

       subroutine export_mesh_domain_wrapper(MD,dir,name)
         implicit none
         type(mesh_domain),intent(in) :: MD
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(MD,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_mesh_domain(MD,un)
         implicit none
         type(mesh_domain),intent(inout) :: MD
         integer,intent(in) :: un
         call import(MD%D,un)
         call import(MD%m_R2,un)
         call import(MD%m_R1,un)
       end subroutine

       subroutine import_mesh_domain_wrapper(MD,dir,name)
         implicit none
         type(mesh_domain),intent(inout) :: MD
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(MD,un)
         call close_and_message(un,dir,name)
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine add_subdomain(MD,sd)
         implicit none
         type(mesh_domain),intent(inout) :: MD
         type(subdomain),intent(in) :: sd
         call add(MD%D,sd)
       end subroutine

       subroutine init_other_mesh_domain(m_other,m,MD)
         implicit none
         type(mesh),intent(inout) :: m_other
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         call delete(m_other)
         if (compare(m,MD%m_R1)) then;     call init(m_other,MD%m_R2)
         elseif (compare(m,MD%m_R2)) then; call init(m_other,MD%m_R1)
         else; stop 'Error: case not found in init_other_mesh_domain in mesh_domain.f90'
         endif
       end subroutine

       end module