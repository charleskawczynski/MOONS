       module mesh_domain_mod
       use IO_tools_mod
       use physical_domain_mod
       use physical_domain_extend_mod
       use physical_sub_domain_mod
       use physical_sub_domain_extend_mod
       use block_field_mod
       use mesh_mod
       implicit none

       private
       public :: mesh_domain
       public :: init,delete,display,print,export,import ! Essentials
       public :: add,init_other

       public :: prolongate

       interface init;             module procedure init_MD;               end interface
       interface init;             module procedure init_MD_copy;          end interface
       interface delete;           module procedure delete_MD;             end interface
       interface display;          module procedure display_MD;            end interface
       interface print;            module procedure print_MD;              end interface
       interface export;           module procedure export_MD;             end interface
       interface import;           module procedure import_MD;             end interface
       interface export;           module procedure export_MD_wrapper;     end interface
       interface import;           module procedure import_MD_wrapper;     end interface

       interface add;              module procedure add_subdomain;         end interface
       interface init_other;       module procedure init_other_MD;         end interface

       interface prolongate;       module procedure prolongate_MD;         end interface

       type mesh_domain
         type(physical_domain) :: D
         type(mesh) :: m_R1,m_R2
       end type

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_MD(MD,m_R1,m_R2)
         implicit none
         type(mesh_domain),intent(inout) :: MD
         type(mesh),intent(in) :: m_R1,m_R2
         call init(MD%D,m_R1,m_R2)
         call init(MD%m_R1,m_R1)
         call init(MD%m_R2,m_R2)
       end subroutine

       subroutine init_MD_copy(MD_out,MD_in)
         implicit none
         type(mesh_domain),intent(inout) :: MD_out
         type(mesh_domain),intent(in) :: MD_in
         call init(MD_out%D,MD_in%D)
         call init(MD_out%m_R1,MD_in%m_R1)
         call init(MD_out%m_R2,MD_in%m_R2)
       end subroutine

       subroutine delete_MD(MD)
         implicit none
         type(mesh_domain),intent(inout) :: MD
         call delete(MD%D)
         call delete(MD%m_R1)
         call delete(MD%m_R2)
       end subroutine

       subroutine print_MD(MD,name)
         implicit none
         type(mesh_domain),intent(in) :: MD
         character(len=*),intent(in) :: name
         call print(MD%D,name)
       end subroutine

       subroutine display_MD(MD,un)
         implicit none
         type(mesh_domain),intent(inout) :: MD
         integer,intent(in) :: un
         call display(MD%D,un)
       end subroutine

       subroutine export_MD(MD,un)
         implicit none
         type(mesh_domain),intent(in) :: MD
         integer,intent(in) :: un
         call export(MD%D,un)
         call export(MD%m_R2,un)
         call export(MD%m_R1,un)
       end subroutine

       subroutine export_MD_wrapper(MD,dir,name)
         implicit none
         type(mesh_domain),intent(in) :: MD
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(MD,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_MD(MD,un)
         implicit none
         type(mesh_domain),intent(inout) :: MD
         integer,intent(in) :: un
         call import(MD%D,un)
         call import(MD%m_R2,un)
         call import(MD%m_R1,un)
       end subroutine

       subroutine import_MD_wrapper(MD,dir,name)
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
         type(physical_sub_domain),intent(in) :: sd
         call add(MD%D,sd)
       end subroutine

       subroutine init_other_MD(m_other,m,MD)
         implicit none
         type(mesh),intent(inout) :: m_other
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         call delete(m_other)
         if (compare(m,MD%m_R1)) then;     call init(m_other,MD%m_R2)
         elseif (compare(m,MD%m_R2)) then; call init(m_other,MD%m_R1)
         else; stop 'Error: case not found in init_other_MD in mesh_domain.f90'
         endif
       end subroutine

       subroutine prolongate_MD(MD,dir)
         implicit none
         type(mesh_domain),intent(inout) :: MD
         integer,intent(in) :: dir
         call prolongate(MD%m_R1,dir)
         call prolongate(MD%m_R2,dir)
         call init(MD%D,MD%m_R1,MD%m_R2)
       end subroutine

       end module