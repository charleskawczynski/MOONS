      module triDiag_mod
      use current_precision_mod
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
      public :: print,check

      type triDiag
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: s,sL,sD,sU
      end type

      interface init;    module procedure initTriDiag;            end interface
      interface init;    module procedure init_Copy;              end interface
      interface init;    module procedure init_Copy_array;        end interface
      interface initL;   module procedure init_TriDiagL;          end interface
      interface initD;   module procedure init_TriDiagD;          end interface
      interface initU;   module procedure init_TriDiagU;          end interface
      interface delete;  module procedure delete_TriDiag;         end interface
      interface delete;  module procedure delete_TriDiag_array;   end interface
      interface print;   module procedure printTriDiag;           end interface
      interface check;   module procedure check_Tridiag;          end interface

      contains

      subroutine initTriDiag(T,L,D,U)
        implicit none
        type(triDiag),intent(inout) :: T
        real(cp),dimension(:),intent(in) :: L,D,U
        call delete(T)
        T%sL = size(L); T%sU = size(U); T%sD = size(D)
        T%s = T%sD
        allocate(T%L(size(L))); allocate(T%D(T%s)); allocate(T%U(size(U)))
        T%L = L; T%D = D; T%U = U
      end subroutine

      subroutine init_Copy(T_out,T_in)
        implicit none
        type(triDiag),intent(inout) :: T_out
        type(triDiag),intent(in) :: T_in
        logical,dimension(3) :: L
        L(1) = allocated(T_in%L)
        L(2) = allocated(T_in%D)
        L(3) = allocated(T_in%U)
        if (all(L)) then
          call init(T_out,T_in%L,T_in%D,T_in%U)
        else
          if (L(1)) call initL(T_out,T_in%L)
          if (L(2)) call initD(T_out,T_in%D)
          if (L(3)) call initU(T_out,T_in%U)
        endif
      end subroutine

      subroutine init_Copy_array(T_out,T_in)
        implicit none
        type(triDiag),dimension(:),intent(inout) :: T_out
        type(triDiag),dimension(:),intent(in) :: T_in
        integer :: i
        do i=1,size(T_out); call init(T_out(i),T_in(i)%L,T_in(i)%D,T_in(i)%U); enddo
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

      subroutine delete_TriDiag(T)
        implicit none
        type(triDiag),intent(inout) :: T
        if (allocated(T%L)) deallocate(T%L)
        if (allocated(T%D)) deallocate(T%D)
        if (allocated(T%U)) deallocate(T%U)
        T%s = 0; T%sL = 0; T%sD = 0; T%sU = 0
      end subroutine

      subroutine delete_TriDiag_array(T)
        implicit none
        type(triDiag),dimension(:),intent(inout) :: T
        integer :: i
        do i=1,size(T); call delete(T(i)); enddo
      end subroutine

      subroutine printTridiag(T)
        implicit none
        type(triDiag),intent(in) :: T
        integer :: i
        write(*,*) 's,sL,sD,sU = ',T%s,T%sL,T%sD,T%sU
        if (all((/allocated(T%L),allocated(T%D),allocated(T%U)/))) then
          write(*,*) 'L,D,U = '
          do i=1,size(T%D); write(*,*) T%L(i),T%D(i),T%U(i); enddo
        elseif (allocated(T%D).and.allocated(T%U)) then
          write(*,*) 'D,U = '
          do i=1,size(T%D); write(*,*) T%D(i),T%U(i); enddo
        endif
      end subroutine

      subroutine check_Tridiag(T)
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
