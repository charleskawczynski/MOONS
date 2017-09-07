       module mesh_domain_extend_mod
       use mesh_domain_mod
       use IO_tools_mod
       use physical_domain_mod
       use physical_domain_extend_mod
       use physical_sub_domain_mod
       use physical_sub_domain_extend_mod
       use block_field_mod
       use block_field_extend_mod
       use mesh_extend_mod
       implicit none

       private
       public :: mesh_domain
       public :: init,delete,display,print,export,import ! Essentials
       public :: print_D
       public :: add,init_other

       public :: prolongate

       interface init;       module procedure init_MD;       end interface
       interface print_D;    module procedure print_MD;      end interface

       interface add;        module procedure add_subdomain; end interface
       interface init_other; module procedure init_other_MD; end interface

       interface prolongate; module procedure prolongate_MD; end interface

       contains

       subroutine init_MD(MD,m_R1,m_R2)
         implicit none
         type(mesh_domain),intent(inout) :: MD
         type(mesh),intent(in) :: m_R1,m_R2
         call init(MD%D,m_R1,m_R2)
         call init(MD%m_R1,m_R1)
         call init(MD%m_R2,m_R2)
       end subroutine

       subroutine print_MD(MD,name)
         implicit none
         type(mesh_domain),intent(in) :: MD
         character(len=*),intent(in) :: name
         call print(MD%D,name)
       end subroutine

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