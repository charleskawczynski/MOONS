      module idct_operator_mod
      use current_precision_mod
      use grid_field_mod
      use grid_field_complex_mod
      implicit none

      private
      public :: idc_operate
      interface idc_operate;    module procedure idct_3D;    end interface

      real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

      contains

      subroutine idct_1D(x,e,xx,xx2,N)
        ! iDCT    Inverse Discrete cosine transform of type II
        !
        !        Y = idct(X) returns the discrete cosine transform of X,
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
        complex(cp), dimension(2*N), intent(inout) :: xx,xx2
        complex(cp),intent(in) :: j ! j = cmplx(0.0_cp,1.0_cp,cp)
        integer :: k
        xx = 0.0_cp; xx2 = xx
        e = 0.0_cp
        xx(1:N) = cmplx(x,0.0_cp,cp)
        do k=1,N
          e(k) = 0.5_cp*exp(-j*0.5_cp*PI*real(k-1,cp)/real(N,cp))
        enddo
        xx(1:N) = xx(1:N)*e
        xx2(1:N) = xx(1:N)
        do k=2,N
          xx2(N+k) = conjg(xx(N-k+2))
        enddo
        call fft1D(xx2)
        x = 2.0_cp/real(N,cp)*real(xx2(1:N),cp)
      end subroutine

      subroutine idct_3D(x,e,tempX,tempX2,dir,pad)
        implicit none
        type(grid_field),intent(inout) :: x
        type(grid_field_complex),intent(inout) :: e,tempX,tempX2
        integer,intent(in) :: dir,pad
        integer,dimension(3) :: s
        integer :: i,j,k

        select case (dir)
        case (1)
          !$OMP PARALLEL DO
          do k=1+pad,x%s(3)-pad; do j=1+pad,x%s(2)-pad
            call idct_1D(x%f(:,j,k),e%f(:,j,k),tempX%f(:,j,k),tempX2%f(:,j,k),x%s(1))
          enddo; enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO
          do k=1+pad,x%s(3)-pad; do i=1+pad,x%s(1)-pad
            call idct_1D(x%f(i,:,k),e%f(i,:,k),tempX%f(i,:,k),tempX2%f(i,:,k),x%s(2))
          enddo; enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO
          do j=1+pad,x%s(2)-pad; do i=1+pad,x%s(1)-pad
            call idct_1D(x%f(i,j,:),e%f(i,j,:),tempX%f(i,j,:),tempX2%f(i,j,:),x%s(3))
          enddo; enddo
          !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in idct_3D in idct_operator.f90.'
        end select
      end subroutine

      end module