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
      public :: print

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
        integer :: s,sL,sD,sU
      end type

      interface init;    module procedure initTriDiag;            end interface
      interface initL;   module procedure init_TriDiagL;          end interface
      interface initD;   module procedure init_TriDiagD;          end interface
      interface initU;   module procedure init_TriDiagU;          end interface
      interface delete;  module procedure deleteTriDiag;          end interface
      interface print;   module procedure printTriDiag;           end interface

      contains

      subroutine initTriDiag(T,L,D,U)
        implicit none
        type(triDiag),intent(inout) :: T
        real(cp),dimension(:),intent(in) :: L,D,U
        T%s = size(D)
        T%sD = T%s
        call delete(T)
        allocate(T%L(size(L))); allocate(T%D(T%s)); allocate(T%U(size(U)))
        T%L = L; T%D = D; T%U = U
      end subroutine

      subroutine init_TriDiagL(T,L)
        implicit none
        type(triDiag),intent(inout) :: T
        real(cp),dimension(:),intent(in) :: L
        if (allocated(T%L)) deallocate(T%L)
        T%sL = size(L); T%s = T%sL
        allocate(T%L(T%sL))
        T%L = L
      end subroutine

      subroutine init_TriDiagD(T,D)
        implicit none
        type(triDiag),intent(inout) :: T
        real(cp),dimension(:),intent(in) :: D
        if (allocated(T%D)) deallocate(T%D)
        T%sD = size(D); T%s = T%sD
        allocate(T%D(T%sD))
        T%D = D
      end subroutine

      subroutine init_TriDiagU(T,U)
        implicit none
        type(triDiag),intent(inout) :: T
        real(cp),dimension(:),intent(in) :: U
        if (allocated(T%U)) deallocate(T%U)
        T%sU = size(U); T%s = T%sU
        allocate(T%U(T%sU))
        T%U = U
      end subroutine

      subroutine deleteTriDiag(T)
        implicit none
        type(triDiag),intent(inout) :: T
        if (allocated(T%L)) deallocate(T%L)
        if (allocated(T%D)) deallocate(T%D)
        if (allocated(T%U)) deallocate(T%U)
        T%s = 0; T%sL = 0; T%sD = 0; T%sU = 0
      end subroutine

      subroutine printTridiag(T)
        implicit none
        type(triDiag),intent(in) :: T
        write(*,*) 's,sL,sD,sU = ',T%s,T%sL,T%sD,T%sU
        if (all((/allocated(T%L),allocated(T%D),allocated(T%U)/))) then
          write(*,*) 'The sum of the stencils should be zero:'
          write(*,*) 'L+D+U = ',T%L+T%D+T%U
        else
          if (allocated(T%D).and.allocated(T%U)) then
            write(*,*) 'The sum of the stencils should be zero:'
            write(*,*) 'D+U = ',T%D+T%U
          endif
        endif
      end subroutine

      end module
