       module domain_mod
       use subdomain_mod
       use mesh_mod

       implicit none

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: domain
       public :: init,delete

       interface init;               module procedure init_domain;        end interface
       interface init;               module procedure init_domain_copy;   end interface
       interface delete;             module procedure delete_domain;      end interface

       type domain
         integer :: s ! Number of subdomains
         type(subdomain),dimension(:),allocatable :: sd
         type(mesh) :: m_in,m_out
       end type

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************** INIT / DELETE **********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine init_domain(D,m_in,m_tot)
         implicit none
         type(domain),intent(inout) :: D
         type(mesh),intent(in) :: m_in,m_tot
         integer :: i
         if (allocated(D%sd)) call delete(D)
         D%s = m_in%s
         call init(D%m_in,m_in)
         call init(D%m_out,m_in)
         allocate(D%sd(D%s))
         do i=1,D%s; call init(D%sd(i),m_in%g(i),m_tot%g(i)); enddo
       end subroutine

       subroutine init_domain_copy(D_out,D_in)
         implicit none
         type(domain),intent(inout) :: D_out
         type(domain),intent(in) :: D_in
         integer :: i
         do i=1,D_in%s; call init(D_out%sd(i),D_in%sd(i)); enddo
       end subroutine

       subroutine delete_domain(D)
         implicit none
         type(domain),intent(inout) :: D
         integer :: i
         do i=1,D%s; call delete(D%sd(i)); enddo
       end subroutine


       end module