      module sparse_extend_mod
      use sparse_mod
      use array_mod
      use array_extend_mod
      use current_precision_mod
      implicit none
      private
      public :: sparse
      public :: init

      public :: assign
      public :: multiply

      public :: init_L,init_D,init_U
      public :: check
      public :: insist_allocated

      interface init;                module procedure init_sparse_size;           end interface
      interface init;                module procedure init_sparse_3;              end interface
      interface init;                module procedure init_sparse_3_array;        end interface
      interface init;                module procedure init_sparse_1;              end interface
      interface init_L;              module procedure init_sparse_L;              end interface
      interface init_D;              module procedure init_sparse_D;              end interface
      interface init_U;              module procedure init_sparse_U;              end interface

      interface check;               module procedure check_sparse;               end interface
      interface insist_allocated;    module procedure insist_allocated_sparse;    end interface

      interface assign;              module procedure assign_sparse;              end interface
      interface multiply;            module procedure multiply_sparse;            end interface

      contains

      subroutine init_sparse_size(S,N)
        implicit none
        type(sparse),intent(inout) :: S
        integer,intent(in) :: N
        call delete(S)
        call init(S%L,N)
        call init(S%D,N)
        call init(S%U,N)
        S%staggered = S%L%N.eq.1
      end subroutine

      subroutine init_sparse_3_array(S,L,D,U)
        implicit none
        type(sparse),intent(inout) :: S
        type(array),intent(in) :: L,D,U
        call delete(S)
        call init(S%L,L)
        call init(S%D,D)
        call init(S%U,U)
        S%staggered = S%L%N.eq.1
      end subroutine

      subroutine init_sparse_3(S,L,D,U,N_L,N_D,N_U)
        implicit none
        type(sparse),intent(inout) :: S
        integer,intent(in) :: N_L,N_D,N_U
        real(cp),dimension(N_L),intent(in) :: L
        real(cp),dimension(N_D),intent(in) :: D
        real(cp),dimension(N_U),intent(in) :: U
        call delete(S)
        call init(S%L,L,N_L)
        call init(S%D,D,N_D)
        call init(S%U,U,N_U)
        S%staggered = S%L%N.eq.1
      end subroutine

      subroutine init_sparse_1(S,L,D,U,N)
        implicit none
        type(sparse),intent(inout) :: S
        integer,intent(in) :: N
        real(cp),dimension(N),intent(in) :: L
        real(cp),dimension(N),intent(in) :: D
        real(cp),dimension(N),intent(in) :: U
        call delete(S)
        call init(S%L,L,N)
        call init(S%D,D,N)
        call init(S%U,U,N)
        S%staggered = S%L%N.eq.1
      end subroutine

      subroutine init_sparse_L(S,L,N)
        implicit none
        type(sparse),intent(inout) :: S
        integer,intent(in) :: N
        real(cp),dimension(N),intent(in) :: L
        call init(S%L,L,N)
        S%staggered = S%L%N.eq.1
      end subroutine

      subroutine init_sparse_D(S,D,N)
        implicit none
        type(sparse),intent(inout) :: S
        integer,intent(in) :: N
        real(cp),dimension(N),intent(in) :: D
        call init(S%D,D,N)
      end subroutine

      subroutine init_sparse_U(S,U,N)
        implicit none
        type(sparse),intent(inout) :: S
        integer,intent(in) :: N
        real(cp),dimension(N),intent(in) :: U
        call init(S%U,U,N)
      end subroutine

      ! *********************************************************************

      subroutine check_sparse(S)
        implicit none
        type(sparse),intent(in) :: S
        logical,dimension(3) :: L
        L(1) = allocated(S%L%f)
        L(2) = allocated(S%D%f)
        L(3) = allocated(S%U%f)
        if (all(L)) then
          write(*,*) 'The sum of the stencils should be zero:'
          call print(S%L)
          call print(S%D)
          call print(S%U)
        elseif (L(2).and.L(3)) then
            write(*,*) 'The sum of the stencils should be zero:'
            call print(S%D)
            call print(S%U)
        else; stop 'Error: case not found in check_sparse in sparse.f90'
        endif
      end subroutine

      subroutine insist_allocated_sparse(S,caller)
        implicit none
        type(sparse),intent(in) :: S
        character(len=*),intent(in) :: caller
        call insist_allocated(S%L,caller//' sparse(L)')
        call insist_allocated(S%D,caller//' sparse(D)')
        call insist_allocated(S%U,caller//' sparse(U)')
      end subroutine

      subroutine assign_sparse(S,val)
        implicit none
        type(sparse),intent(inout) :: S
        real(cp),intent(in) :: val
        call assign(S%L,val)
        call assign(S%D,val)
        call assign(S%U,val)
      end subroutine

      subroutine multiply_sparse(S,val)
        implicit none
        type(sparse),intent(inout) :: S
        real(cp),intent(in) :: val
        call multiply(S%L,val)
        call multiply(S%D,val)
        call multiply(S%U,val)
      end subroutine

      end module