       module domain_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use subdomain_mod
       use mesh_mod
       implicit none

       private
       public :: domain
       public :: init,delete,display,print,export,import ! Essentials
       public :: add,init_other

       interface init;        module procedure init_domain;           end interface
       interface init;        module procedure init_domain_copy;      end interface
       interface delete;      module procedure delete_domain;         end interface
       interface display;     module procedure display_domain;        end interface
       interface print;       module procedure print_domain;          end interface
       interface export;      module procedure export_domain;         end interface
       interface import;      module procedure import_domain;         end interface
       interface export;      module procedure export_domain_wrapper; end interface
       interface import;      module procedure import_domain_wrapper; end interface

       interface add;         module procedure add_subdomain;         end interface
       interface init_other;  module procedure init_other_domain;     end interface

       type domain
         integer :: s ! Number of subdomains
         type(subdomain),dimension(:),allocatable :: sd
         type(mesh) :: m_R1,m_R2
       end type

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_domain(D,m_R1,m_R2)
         implicit none
         type(domain),intent(inout) :: D
         type(mesh),intent(in) :: m_R1,m_R2
         type(subdomain) :: temp
         integer :: j,k
         call delete(D)

         ! Initialize mesh:
         call init(D%m_R1,m_R1)
         call init(D%m_R2,m_R2)
         ! Make all possible/necessary subdomains:
         if (m_R2%s.gt.m_R1%s) then
           do k=1,m_R2%s; do j=1,m_R1%s
             call init(temp,m_R1%g(j),m_R2%g(k),j,k)
             if (all(temp%defined)) call add(D,temp)
           enddo; enddo
         else
           do k=1,m_R1%s; do j=1,m_R2%s
             call init(temp,m_R1%g(k),m_R2%g(j),k,j)
             if (all(temp%defined)) call add(D,temp)
           enddo; enddo
         endif
       end subroutine

       subroutine init_domain_copy(D_out,D_in)
         implicit none
         type(domain),intent(inout) :: D_out
         type(domain),intent(in) :: D_in
         integer :: i
         if (allocated(D_in%sd)) then
           call delete(D_out)
           D_out%s = D_in%s
           allocate(D_out%sd(D_out%s))
           do i=1,D_in%s; call init(D_out%sd(i),D_in%sd(i)); enddo
         else; stop 'Error: trying to copy un-initialized subdomain in domain.f90'
         endif
         call init(D_out%m_R1,D_in%m_R1)
         call init(D_out%m_R2,D_in%m_R2)
       end subroutine

       subroutine delete_domain(D)
         implicit none
         type(domain),intent(inout) :: D
         integer :: i
         if (allocated(D%sd)) then
           do i=1,D%s; call delete(D%sd(i)); enddo
           deallocate(D%sd)
         endif
         call delete(D%m_R1)
         call delete(D%m_R2)
         D%s = 0
       end subroutine

       subroutine print_domain(D,name)
         implicit none
         type(domain),intent(in) :: D
         character(len=*),intent(in) :: name
         integer :: i
         write(*,*) 'N-subdomains = ',D%s
         do i=1,D%s; call print(D%sd(i),name//'_'//int2str(i)); enddo
       end subroutine

       subroutine display_domain(D,dir,name)
         implicit none
         type(domain),intent(inout) :: D
         character(len=*),intent(in) :: dir,name
         integer :: i,NU
         NU = new_and_open(dir,name)
         do i=1,D%s; call display(D%sd(i),name//'_'//int2str(i),NU); enddo
       end subroutine

       subroutine export_domain(D,un)
         implicit none
         type(domain),intent(in) :: D
         integer,intent(in) :: un
         integer :: i
         write(un,*) 'D%s'
         write(un,*) D%s
         do i=1,D%s; call export(D%sd(i),un); enddo
         call export(D%m_R2,un)
         call export(D%m_R1,un)
       end subroutine

       subroutine export_domain_wrapper(D,dir,name)
         implicit none
         type(domain),intent(in) :: D
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(D,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_domain(D,un)
         implicit none
         type(domain),intent(inout) :: D
         integer,intent(in) :: un
         integer :: i
         call delete(D)
         read(un,*)
         read(un,*) D%s
         allocate(D%sd(D%s))
         do i=1,D%s; call import(D%sd(i),un); enddo
         call import(D%m_R2,un)
         call import(D%m_R1,un)
       end subroutine

       subroutine import_domain_wrapper(D,dir,name)
         implicit none
         type(domain),intent(inout) :: D
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(D,un)
         call close_and_message(un,dir,name)
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine add_subdomain(D,sd)
         implicit none
         type(domain),intent(inout) :: D
         type(subdomain),intent(in) :: sd
         type(domain) :: temp
         integer :: i
         if (.not.allocated(D%sd)) then
           D%s = 1
           allocate(D%sd(D%s))
           call init(D%sd(D%s),sd)
         else
           call init(temp,D)
           call delete(D)
           call init(D%m_R1,temp%m_R1)
           call init(D%m_R2,temp%m_R2)
           D%s = temp%s + 1
           allocate(D%sd(D%s))
           do i=1,D%s-1; call init(D%sd(i),temp%sd(i)); enddo
           call init(D%sd(D%s),sd)
           call delete(temp)
         endif
       end subroutine

       subroutine init_other_domain(m_other,m,D)
         implicit none
         type(mesh),intent(inout) :: m_other
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D
         call delete(m_other)
         if (compare(m,D%m_R1)) then;     call init(m_other,D%m_R2)
         elseif (compare(m,D%m_R2)) then; call init(m_other,D%m_R1)
         else; stop 'Error: case not found in init_other in domain.f90'
         endif
       end subroutine

       end module