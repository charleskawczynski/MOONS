      module sparse_mod
      use array_mod
      use current_precision_mod
      implicit none
      private
      public :: sparse
      public :: init,delete,display,print,export,import

      public :: init_L,init_D,init_U
      public :: check
      public :: insist_allocated

      type sparse
        type(array) :: L,D,U
      end type

      interface init;                module procedure init_sparse_size;         end interface
      interface init;                module procedure init_sparse;              end interface
      interface init;                module procedure init_Copy;                end interface
      interface init_L;              module procedure init_sparse_L;            end interface
      interface init_D;              module procedure init_sparse_D;            end interface
      interface init_U;              module procedure init_sparse_U;            end interface
      interface delete;              module procedure delete_sparse;            end interface
      interface print;               module procedure print_sparse;             end interface
      interface display;             module procedure display_sparse;           end interface
      interface import;              module procedure import_sparse;            end interface
      interface export;              module procedure export_sparse;            end interface

      interface check;               module procedure check_sparse;             end interface
      interface insist_allocated;    module procedure insist_allocated_sparse;  end interface

      contains

      subroutine init_sparse_size(S,N)
        implicit none
        type(sparse),intent(inout) :: S
        integer,intent(in) :: N
        call delete(S)
        call init(S%L,N)
        call init(S%D,N)
        call init(S%U,N)
      end subroutine

      subroutine init_sparse(S,L,D,U,N_L,N_D,N_U)
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
      end subroutine

      subroutine init_Copy(S,S_in)
        implicit none
        type(sparse),intent(inout) :: S
        type(sparse),intent(in) :: S_in
        call delete(S)
        call init(S%L,S_in%L)
        call init(S%D,S_in%D)
        call init(S%U,S_in%U)
      end subroutine

      subroutine init_sparse_L(S,L,N)
        implicit none
        type(sparse),intent(inout) :: S
        integer,intent(in) :: N
        real(cp),dimension(N),intent(in) :: L
        call init(S%L,L,N)
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

      subroutine delete_sparse(S)
        implicit none
        type(sparse),intent(inout) :: S
        call delete(S%L)
        call delete(S%D)
        call delete(S%U)
      end subroutine

      subroutine print_sparse(S)
        implicit none
        type(sparse),intent(in) :: S
        call display(S%L,6)
        call display(S%D,6)
        call display(S%U,6)
      end subroutine

      subroutine display_sparse(S,un)
        implicit none
        type(sparse),intent(in) :: S
        integer,intent(in) :: un
        call display(S%L,un)
        call display(S%D,un)
        call display(S%U,un)
      end subroutine

      subroutine export_sparse(S,un)
        implicit none
        type(sparse),intent(in) :: S
        integer,intent(in) :: un
        call export(S%L,un)
        call export(S%D,un)
        call export(S%U,un)
      end subroutine

      subroutine import_sparse(S,un)
        implicit none
        type(sparse),intent(inout) :: S
        integer,intent(in) :: un
        call import(S%L,un)
        call import(S%D,un)
        call import(S%U,un)
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

      end module