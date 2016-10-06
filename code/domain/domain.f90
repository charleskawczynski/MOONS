       module domain_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use subdomain_mod
       use grid_mod
       use mesh_mod
       implicit none

       private
       public :: domain
       public :: init,delete,display,print,export,import ! Essentials
       public :: add

       interface init;        module procedure init_domain_mesh;      end interface
       interface init;        module procedure init_domain_grid;      end interface
       interface init;        module procedure init_domain_copy;      end interface
       interface delete;      module procedure delete_domain;         end interface
       interface display;     module procedure display_domain;        end interface
       interface display;     module procedure display_domain_wrapper;end interface
       interface print;       module procedure print_domain;          end interface
       interface export;      module procedure export_domain;         end interface
       interface import;      module procedure import_domain;         end interface
       interface export;      module procedure export_domain_wrapper; end interface
       interface import;      module procedure import_domain_wrapper; end interface

       interface add;         module procedure add_subdomain;         end interface
       interface add;         module procedure add_domain_grid;       end interface

       type domain
         integer :: s ! Number of subdomains
         type(subdomain),dimension(:),allocatable :: sd
       end type

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_domain_mesh(D,m_R1,m_R2)
         implicit none
         type(domain),intent(inout) :: D
         type(mesh),intent(in) :: m_R1,m_R2
         type(subdomain) :: temp
         integer :: j,k
         call delete(D)
         ! Make all possible/necessary subdomains:
         if (m_R2%s.gt.m_R1%s) then
           do k=1,m_R2%s; do j=1,m_R1%s
             call init(temp,m_R1%B(j)%g,m_R2%B(k)%g,j,k)
             if (all(temp%defined)) call add(D,temp)
           enddo; enddo
         else
           do k=1,m_R1%s; do j=1,m_R2%s
             call init(temp,m_R1%B(k)%g,m_R2%B(j)%g,k,j)
             if (all(temp%defined)) call add(D,temp)
           enddo; enddo
         endif
       end subroutine

       subroutine init_domain_grid(D,g_R1,g_R2,g_id_1,g_id_2)
         implicit none
         type(domain),intent(inout) :: D
         type(grid),intent(in) :: g_R1,g_R2
         integer,intent(in) :: g_id_1,g_id_2
         call delete(D)
         call add(D,g_R1,g_R2,g_id_1,g_id_2)
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
       end subroutine

       subroutine delete_domain(D)
         implicit none
         type(domain),intent(inout) :: D
         integer :: i
         if (allocated(D%sd)) then
           do i=1,D%s; call delete(D%sd(i)); enddo
           deallocate(D%sd)
         endif
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

       subroutine display_domain(D,un)
         implicit none
         type(domain),intent(inout) :: D
         integer,intent(in) :: un
         integer :: i
         do i=1,D%s; call display(D%sd(i),'SD_'//int2str(i),un); enddo
       end subroutine

       subroutine display_domain_wrapper(D,dir,name)
         implicit none
         type(domain),intent(inout) :: D
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(D,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine export_domain(D,un)
         implicit none
         type(domain),intent(in) :: D
         integer,intent(in) :: un
         integer :: i
         write(un,*) 'D%s'
         write(un,*) D%s
         do i=1,D%s; call export(D%sd(i),un); enddo
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

       subroutine add_domain_grid(D,g_R1,g_R2,g_id_1,g_id_2)
         implicit none
         type(domain),intent(inout) :: D
         type(grid),intent(in) :: g_R1,g_R2
         integer,intent(in) :: g_id_1,g_id_2
         type(subdomain) :: temp
         call init(temp,g_R1,g_R2,g_id_1,g_id_2)
         if (all(temp%defined)) call add(D,temp)
         call delete(temp)
       end subroutine

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
           D%s = temp%s + 1
           allocate(D%sd(D%s))
           do i=1,D%s-1; call init(D%sd(i),temp%sd(i)); enddo
           call init(D%sd(D%s),sd)
           call delete(temp)
         endif
       end subroutine

       end module