      module sparse_mod
      use array_mod
      use current_precision_mod
      implicit none
      private
      public :: sparse
      public :: init,delete,display,print,export,import

      public :: consecutive_stag_CC
      public :: consecutive_stag_N
      public :: assign

      public :: init_L,init_D,init_U
      public :: check
      public :: insist_allocated

      type sparse
        type(array) :: L,D,U
      end type

      interface init;                module procedure init_sparse_size;           end interface
      interface init;                module procedure init_sparse_3;              end interface
      interface init;                module procedure init_sparse_3_array;        end interface
      interface init;                module procedure init_sparse_1;              end interface
      interface init;                module procedure init_Copy;                  end interface
      interface init_L;              module procedure init_sparse_L;              end interface
      interface init_D;              module procedure init_sparse_D;              end interface
      interface init_U;              module procedure init_sparse_U;              end interface
      interface delete;              module procedure delete_sparse;              end interface
      interface print;               module procedure print_sparse;               end interface
      interface display;             module procedure display_sparse;             end interface
      interface import;              module procedure import_sparse;              end interface
      interface export;              module procedure export_sparse;              end interface

      interface check;               module procedure check_sparse;               end interface
      interface insist_allocated;    module procedure insist_allocated_sparse;    end interface

      interface consecutive_stag_CC; module procedure consecutive_stag_CC_sparse; end interface
      interface consecutive_stag_N;  module procedure consecutive_stag_N_sparse;  end interface
      interface assign;              module procedure assign_sparse;              end interface

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

      subroutine init_sparse_3_array(S,L,D,U)
        implicit none
        type(sparse),intent(inout) :: S
        type(array),intent(in) :: L,D,U
        call delete(S)
        call init(S%L,L)
        call init(S%D,D)
        call init(S%U,U)
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
        call display_sparse(S,6)
      end subroutine

      subroutine display_sparse(S,un)
        implicit none
        type(sparse),intent(in) :: S
        integer,intent(in) :: un
        write(un,*) '---------------------------- L'; call display(S%L,un)
        write(un,*) '---------------------------- D'; call display(S%D,un)
        write(un,*) '---------------------------- U'; call display(S%U,un)
        write(un,*) '----------------------------'
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

      subroutine assign_sparse(S,val)
        implicit none
        type(sparse),intent(inout) :: S
        real(cp),intent(in) :: val
        call assign(S%L,val)
        call assign(S%D,val)
        call assign(S%U,val)
      end subroutine

      subroutine consecutive_stag_CC_sparse(S,CC,N)
        implicit none
        type(sparse),intent(inout) :: S
        type(sparse),intent(in) :: CC,N
        type(array) :: L_CC,U_CC
        type(array) :: L_N,U_N
        type(array),dimension(2) :: D_CC,D_N,D_combined
        type(array) :: L,D,U
        integer :: e

        call init(L_CC,CC%D%f,CC%D%N)
        call init(D_CC(1),(/CC%D%f,0.0_cp/),CC%D%N+1)
        call init(D_CC(2),(/0.0_cp,CC%U%f/),CC%U%N+1)
        call init(U_CC,CC%U%f,CC%U%N)

        e = N%D%N; call init(L_N   ,N%D%f(2:e),N%D%N-1)
        e = N%D%N; call init(D_N(1),N%D%f(2:e),N%D%N-1)
        e = N%U%N; call init(D_N(2),N%U%f(1:e-1),N%U%N-1)
        e = N%U%N; call init(U_N   ,N%U%f(1:e-1),N%U%N-1)

        call multiply(L,L_CC,L_N)
        call multiply(U,U_CC,U_N)

        call multiply(D_combined(1),D_CC(1),D_N(1))
        call multiply(D_combined(2),D_CC(2),D_N(2))
        call add(D,D_combined(1),D_combined(2))

        call init(S,L,D,U)
      end subroutine

      subroutine consecutive_stag_N_sparse(S,CC,N)
        implicit none
        type(sparse),intent(inout) :: S
        type(sparse),intent(in) :: CC,N
        type(array) :: L_CC,U_CC
        type(array) :: L_N,U_N
        type(array),dimension(2) :: D_CC,D_N,D_combined
        type(array) :: L,D,U

        call init(L_CC,CC%D)
        call init(D_CC(1),CC%D); call append(D_CC(1),0.0_cp)
        call init(D_CC(2),CC%U); call insert(D_CC(2),0.0_cp)
        call init(U_CC,CC%U)

        call init(L_N,N%D); call snip(L_N)
        call init(D_N(1),N%D); call snip(D_N(1))
        call init(D_N(2),N%U); call pop(D_N(2))
        call init(U_N,N%U); call pop(U_N)

        call multiply(L,L_CC,L_N)
        call multiply(U,U_CC,U_N)

        call multiply(D_combined(1),D_CC(1),D_N(1))
        call multiply(D_combined(2),D_CC(2),D_N(2))
        call add(D,D_combined(1),D_combined(2))

        call init(S,L,D,U)
      end subroutine

      end module