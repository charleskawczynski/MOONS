      module triDiag_mod
      ! Stores elements of a triDiag matrix A
      ! 
      ! type(triDiag) :: A
      ! call init(A,L,diag,U) ! Sets L,D,U
      ! call delete(A)
      ! 
      ! Contains:
      !            A%L
      !            A%D
      !            A%U

      implicit none

      private
      public :: triDiag
      public :: init,delete
      public :: initL,initD,initU

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif      

      type triDiag
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: s
      end type

      interface init;    module procedure initTriDiag;            end interface
      interface initL;   module procedure init_TriDiagL;          end interface
      interface initD;   module procedure init_TriDiagD;          end interface
      interface initU;   module procedure init_TriDiagU;          end interface
      interface delete;  module procedure deleteTriDiag;          end interface

      contains

      subroutine initTriDiag(T,L,D,U)
        implicit none
        type(triDiag),intent(inout) :: T
        real(cp),dimension(:),intent(in) :: L,D,U
        T%s = size(D)
        call delete(T)
        allocate(T%L(size(L))); allocate(T%D(T%s)); allocate(T%U(size(U)))
        T%L = L; T%D = D; T%U = U
      end subroutine

      subroutine init_TriDiagL(T,L)
        implicit none
        type(triDiag),intent(inout) :: T
        real(cp),dimension(:),intent(in) :: L
        if (allocated(T%L)) deallocate(T%L)
        allocate(T%L(size(L)))
        T%L = L
      end subroutine

      subroutine init_TriDiagD(T,D)
        implicit none
        type(triDiag),intent(inout) :: T
        real(cp),dimension(:),intent(in) :: D
        if (allocated(T%D)) deallocate(T%D)
        allocate(T%D(size(D)))
        T%D = D
      end subroutine

      subroutine init_TriDiagU(T,U)
        implicit none
        type(triDiag),intent(inout) :: T
        real(cp),dimension(:),intent(in) :: U
        if (allocated(T%U)) deallocate(T%U)
        allocate(T%U(size(U)))
        T%U = U
      end subroutine

      subroutine deleteTriDiag(T)
        implicit none
        type(triDiag),intent(inout) :: T
        if (allocated(T%L)) deallocate(T%L)
        if (allocated(T%D)) deallocate(T%D)
        if (allocated(T%U)) deallocate(T%U)
        T%s = 0
      end subroutine

      end module
