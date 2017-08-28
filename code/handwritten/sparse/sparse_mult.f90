      module sparse_mult_mod
      use current_precision_mod
      use sparse_mod
      implicit none
      private

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

      public :: sparse_mult
      public :: assign_C2N_LDU, add_C2N_LDU, subtract_C2N_LDU
      public :: assign_N2C_LDU, add_N2C_LDU, subtract_N2C_LDU
      public :: assign_C2N_DU,  add_C2N_DU,  subtract_C2N_DU
      public :: assign_N2C_DU,  add_N2C_DU,  subtract_N2C_DU

      contains

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