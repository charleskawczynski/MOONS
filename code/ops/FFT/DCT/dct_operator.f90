      module ops_dct_mod
      use current_precision_mod
      use grid_mod
      use ops_fft_mod
      implicit none

      private
      public :: dct_operate
      interface dct_operate;    module procedure dct_3D;    end interface

      real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

      contains

      subroutine dct_1D(x,e,xx,N)
        ! DCT    Discrete cosine transform of type II
        !
        !        Y = dct(X) returns the discrete cosine transform of X,
        !        based on the staggered-grid definition
        !                    N
        !            Y(k) = sum X(j) cos (pi*(k-1)*(j-1/2)/N)
        !                   j=1
        !        The vector Y is the same size as X and contains the
        !        discrete cosine transform coefficients.
        !
        integer,intent(in) :: N
        real(cp),    dimension(N), intent(inout) :: x
        complex(cp), dimension(N), intent(inout) :: e
        complex(cp), dimension(2*N), intent(inout) :: xx
        complex(cp),intent(in) :: j ! j = cmplx(0.0_cp,1.0_cp,cp)
        xx = 0.0_cp
        e = 0.0_cp
        xx(1:N) = cmplx(x,0.0_cp,cp)
        do k=1,N
          xx(N+k) = cmplx(x(N-k+1),0.0_cp,cp)
        enddo
        call fft1D(xx)
        do k=1,N
          e(k) = 0.5_cp*exp(-j*0.5_cp*PI*real(k-1,cp)/real(N,cp))
        enddo
        do k=1,N
          x(k) = real(xx(k)*e(k),cp)
        enddo
      end subroutine

      subroutine dct_3D(x,e,tempX,dir,pad)
        implicit none
        type(grid_field),intent(inout) :: x
        type(grid_field_complex),intent(inout) :: e,tempX
        integer,intent(in) :: dir,pad
        integer,dimension(3) :: s
        integer :: i,j,k

        select case (dir)
        case (1)
          !$OMP PARALLEL DO
          do k=1+pad,x%s(3)-pad; do j=1+pad,x%s(2)-pad
            call dct_1D(x%f(:,j,k),e%f(:,j,k),tempX%f(:,j,k),x%s(1))
          enddo; enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO
          do k=1+pad,x%s(3)-pad; do i=1+pad,x%s(1)-pad
            call dct_1D(x%f(i,:,k),e%f(i,:,k),tempX%f(i,:,k),x%s(2))
          enddo; enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO
          do j=1+pad,x%s(2)-pad; do i=1+pad,x%s(1)-pad
            call dct_1D(x%f(i,j,:),e%f(i,j,:),tempX%f(i,j,:),x%s(3))
          enddo; enddo
          !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in idct_3D in idct_operator.f90.'
        end select
      end subroutine

      end module