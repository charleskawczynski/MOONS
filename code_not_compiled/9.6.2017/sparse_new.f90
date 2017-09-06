      module sparse_new_mod
      use array_mod
      use current_precision_mod
      implicit none
      private
      public :: sparse
      public :: init,delete,display,print,export,import

      public :: init_L,init_D,init_U
      public :: check

      public :: sparse_mult
      public :: assign_C2N_LDU, add_C2N_LDU, subtract_C2N_LDU
      public :: assign_N2C_LDU, add_N2C_LDU, subtract_N2C_LDU
      public :: assign_C2N_DU,  add_C2N_DU,  subtract_C2N_DU
      public :: assign_N2C_DU,  add_N2C_DU,  subtract_N2C_DU

      type sparse
        private
        type(array) :: L,D,U
      end type

      abstract interface
        pure subroutine sparse_mult(Ax,x,A,N_x,N_Ax)
          import :: sparse,cp
          implicit none
          integer,intent(in) :: N_x,N_Ax
          real(cp),dimension(N_Ax),intent(inout) :: Ax
          real(cp),dimension(N_x),intent(in) :: x
          type(sparse),intent(in) :: A
        end subroutine
      end interface

      interface init;    module procedure init_sparse;            end interface
      interface init;    module procedure init_Copy;              end interface
      interface init_L;  module procedure init_sparse_L;          end interface
      interface init_D;  module procedure init_sparse_D;          end interface
      interface init_U;  module procedure init_sparse_U;          end interface
      interface delete;  module procedure delete_sparse;          end interface
      interface print;   module procedure print_sparse;           end interface

      interface check;   module procedure check_sparse;           end interface

      contains

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

      ! *********************************************************************
      ! **************************** SPARSE MULT ****************************
      ! *********************************************************************

      pure subroutine assign_C2N_LDU(Ax,x,A,N_x,N_Ax)
        implicit none
        integer,intent(in) :: N_x,N_Ax
        real(cp),dimension(N_Ax),intent(inout) :: Ax
        real(cp),dimension(N_x),intent(in) :: x
        type(sparse),intent(in) :: A
        integer :: i
        Ax(1) = 0.0_cp; Ax(N_Ax) = 0.0_cp
        do i=2,N_x-1
          Ax(i) = x(i-1)*A%L%f(i) + &
                  x( i )*A%D%f(i) + &
                  x(i+1)*A%U%f(i)
        enddo
      end subroutine
      pure subroutine assign_N2C_LDU(Ax,x,A,N_x,N_Ax)
        implicit none
        integer,intent(in) :: N_x,N_Ax
        real(cp),dimension(N_Ax),intent(inout) :: Ax
        real(cp),dimension(N_x),intent(in) :: x
        type(sparse),intent(in) :: A
        integer :: i
        Ax(1) = 0.0_cp; Ax(N_Ax) = 0.0_cp
        do i=2,N_x-1
          Ax(i) = x(i-1)*A%L%f(i) + &
                  x( i )*A%D%f(i) + &
                  x(i+1)*A%U%f(i)
        enddo
      end subroutine
      pure subroutine assign_C2N_DU(Ax,x,A,N_x,N_Ax)
        implicit none
        integer,intent(in) :: N_x,N_Ax
        real(cp),dimension(N_Ax),intent(inout) :: Ax
        real(cp),dimension(N_x),intent(in) :: x
        type(sparse),intent(in) :: A
        integer :: i
        Ax(1) = 0.0_cp; Ax(N_Ax) = 0.0_cp
        do i=1,N_x-1
          Ax(i) = x( i )*A%D%f(i) + &
                  x(i+1)*A%U%f(i)
        enddo
      end subroutine
      pure subroutine assign_N2C_DU(Ax,x,A,N_x,N_Ax)
        implicit none
        integer,intent(in) :: N_x,N_Ax
        real(cp),dimension(N_Ax),intent(inout) :: Ax
        real(cp),dimension(N_x),intent(in) :: x
        type(sparse),intent(in) :: A
        integer :: i
        Ax(1) = 0.0_cp; Ax(N_Ax) = 0.0_cp
        do i=1,N_x-1
          Ax(i) = x( i )*A%D%f(i) + &
                  x(i+1)*A%U%f(i)
        enddo
      end subroutine

      pure subroutine add_C2N_LDU(Ax,x,A,N_x,N_Ax)
        implicit none
        integer,intent(in) :: N_x,N_Ax
        real(cp),dimension(N_Ax),intent(inout) :: Ax
        real(cp),dimension(N_x),intent(in) :: x
        type(sparse),intent(in) :: A
        integer :: i
        Ax(1) = 0.0_cp; Ax(N_Ax) = 0.0_cp
        do i=2,N_x-1
          Ax(i) = Ax(i)+ x(i-1)*A%L%f(i) + &
                         x( i )*A%D%f(i) + &
                         x(i+1)*A%U%f(i)
        enddo
      end subroutine
      pure subroutine add_N2C_LDU(Ax,x,A,N_x,N_Ax)
        implicit none
        integer,intent(in) :: N_x,N_Ax
        real(cp),dimension(N_Ax),intent(inout) :: Ax
        real(cp),dimension(N_x),intent(in) :: x
        type(sparse),intent(in) :: A
        integer :: i
        Ax(1) = 0.0_cp; Ax(N_Ax) = 0.0_cp
        do i=2,N_x-1
          Ax(i) = Ax(i)+ x(i-1)*A%L%f(i) + &
                         x( i )*A%D%f(i) + &
                         x(i+1)*A%U%f(i)
        enddo
      end subroutine
      pure subroutine add_C2N_DU(Ax,x,A,N_x,N_Ax)
        implicit none
        integer,intent(in) :: N_x,N_Ax
        real(cp),dimension(N_Ax),intent(inout) :: Ax
        real(cp),dimension(N_x),intent(in) :: x
        type(sparse),intent(in) :: A
        integer :: i
        Ax(1) = 0.0_cp; Ax(N_Ax) = 0.0_cp
        do i=1,N_x-1
          Ax(i) = Ax(i)+ x( i )*A%D%f(i) + &
                         x(i+1)*A%U%f(i)
        enddo
      end subroutine
      pure subroutine add_N2C_DU(Ax,x,A,N_x,N_Ax)
        implicit none
        integer,intent(in) :: N_x,N_Ax
        real(cp),dimension(N_Ax),intent(inout) :: Ax
        real(cp),dimension(N_x),intent(in) :: x
        type(sparse),intent(in) :: A
        integer :: i
        Ax(1) = 0.0_cp; Ax(N_Ax) = 0.0_cp
        do i=1,N_x-1
          Ax(i) = Ax(i)+ x( i )*A%D%f(i) + &
                         x(i+1)*A%U%f(i)
        enddo
      end subroutine

      pure subroutine subtract_C2N_LDU(Ax,x,A,N_x,N_Ax)
        implicit none
        integer,intent(in) :: N_x,N_Ax
        real(cp),dimension(N_Ax),intent(inout) :: Ax
        real(cp),dimension(N_x),intent(in) :: x
        type(sparse),intent(in) :: A
        integer :: i
        Ax(1) = 0.0_cp; Ax(N_Ax) = 0.0_cp
        do i=2,N_x-1
          Ax(i) = Ax(i) - (x(i-1)*A%L%f(i) + &
                           x( i )*A%D%f(i) + &
                           x(i+1)*A%U%f(i))
        enddo
      end subroutine
      pure subroutine subtract_N2C_LDU(Ax,x,A,N_x,N_Ax)
        implicit none
        integer,intent(in) :: N_x,N_Ax
        real(cp),dimension(N_Ax),intent(inout) :: Ax
        real(cp),dimension(N_x),intent(in) :: x
        type(sparse),intent(in) :: A
        integer :: i
        Ax(1) = 0.0_cp; Ax(N_Ax) = 0.0_cp
        do i=2,N_x-1
          Ax(i) = Ax(i) - (x(i-1)*A%L%f(i) + &
                           x( i )*A%D%f(i) + &
                           x(i+1)*A%U%f(i))
        enddo
      end subroutine
      pure subroutine subtract_C2N_DU(Ax,x,A,N_x,N_Ax)
        implicit none
        integer,intent(in) :: N_x,N_Ax
        real(cp),dimension(N_Ax),intent(inout) :: Ax
        real(cp),dimension(N_x),intent(in) :: x
        type(sparse),intent(in) :: A
        integer :: i
        Ax(1) = 0.0_cp; Ax(N_Ax) = 0.0_cp
        do i=1,N_x-1
          Ax(i) = Ax(i) - (x( i )*A%D%f(i) + &
                           x(i+1)*A%U%f(i))
        enddo
      end subroutine
      pure subroutine subtract_N2C_DU(Ax,x,A,N_x,N_Ax)
        implicit none
        integer,intent(in) :: N_x,N_Ax
        real(cp),dimension(N_Ax),intent(inout) :: Ax
        real(cp),dimension(N_x),intent(in) :: x
        type(sparse),intent(in) :: A
        integer :: i
        Ax(1) = 0.0_cp; Ax(N_Ax) = 0.0_cp
        do i=1,N_x-1
          Ax(i) = Ax(i) - (x( i )*A%D%f(i) + &
                           x(i+1)*A%U%f(i))
        enddo
      end subroutine

      end module