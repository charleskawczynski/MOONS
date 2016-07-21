       module domain_mod
       use current_precision_mod
       use IO_tools_mod
       use subdomain_mod
       use mesh_mod
       implicit none

       private
       public :: domain
       public :: init,delete,display,print,export,import ! Essentials
       public :: add

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

       type domain
         integer :: s ! Number of subdomains
         type(subdomain),dimension(:),allocatable :: sd
         type(mesh) :: m_in,m_tot
       end type

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_domain(D,m_in,m_tot)
         implicit none
         type(domain),intent(inout) :: D
         type(mesh),intent(in) :: m_in,m_tot
         type(subdomain) :: temp
         integer :: j,k
         call delete(D)

         ! Initialize mesh:
         call init(D%m_in,m_in)
         call init(D%m_tot,m_tot)
         ! Make all possible/necessary subdomains:
         if (m_tot%s.gt.m_in%s) then
           do k=1,m_tot%s; do j=1,m_in%s
             call init(temp,m_in%g(j),m_tot%g(k),j,k)
             if (all(temp%defined)) then
              call add(D,temp)
             endif
           enddo; enddo
         else
           do k=1,m_in%s; do j=1,m_tot%s
             call init(temp,m_in%g(k),m_tot%g(j),k,j)
             if (all(temp%defined)) then
              call add(D,temp)
             endif
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
         call init(D_out%m_in,D_in%m_in)
         call init(D_out%m_tot,D_in%m_tot)
       end subroutine

       subroutine delete_domain(D)
         implicit none
         type(domain),intent(inout) :: D
         integer :: i
         if (allocated(D%sd)) then
           do i=1,D%s; call delete(D%sd(i)); enddo
           deallocate(D%sd)
         endif
         call delete(D%m_in)
         call delete(D%m_tot)
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
         NU = newAndOpen(dir,name)
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
         call export(D%m_tot,un)
         call export(D%m_in,un)
       end subroutine

       subroutine export_domain_wrapper(D,dir,name)
         implicit none
         type(domain),intent(in) :: D
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = newAndOpen(dir,name)
         call export(D,un)
         call closeAndMessage(un,name,dir)
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
         call import(D%m_tot,un)
         call import(D%m_in,un)
       end subroutine

       subroutine import_domain_wrapper(D,dir,name)
         implicit none
         type(domain),intent(inout) :: D
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = openToRead(dir,name)
         call import(D,un)
         call closeAndMessage(un,name,dir)
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
           call init(D%m_in,temp%m_in)
           call init(D%m_tot,temp%m_tot)
           D%s = temp%s + 1
           allocate(D%sd(D%s))
           do i=1,D%s-1; call init(D%sd(i),temp%sd(i)); enddo
           call init(D%sd(D%s),sd)
           call delete(temp)
         endif
       end subroutine

       end module