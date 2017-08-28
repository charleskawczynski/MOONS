      module ops_fft_mod
      use current_precision_mod
      use grid_field_mod
      use grid_field_complex_mod
      use constants_mod
      implicit none

      private
      public :: fft
      interface fft;      module procedure applyFFT3D;    end interface

#ifdef _FFT_RADIX2_
      interface fft1D;    module procedure fft1D_Radix2;  end interface
#else
      interface fft1D;    module procedure fft1D_full;    end interface
#endif

      contains

#ifdef _FFT_RADIX2_
      recursive subroutine fft1D_Radix2(x,N) ! In place Cooley-Tukey FFT
        ! Computes
        !
        !                  N
        !    X(k) =       sum  x(n)*exp(-j*2*PI*(k-1)*(n-1)/N), 1 <= k <= N.
        !                 n=1
        !
        ! Notes: Only valid for when the number of cells is 2^N, where N>1
        !
        integer,intent(in) :: N
        complex(cp),dimension(N), intent(inout) :: x
        complex(cp),dimension(N/2) :: even
        complex(cp),dimension((N+1)/2) :: odd
        complex(cp) :: t
        integer :: i
        if(N .le. 1) return
        ! divide
        odd =x(1:N:2)
        even=x(2:N:2)
        ! conquer
        call fft1D(odd)
        call fft1D(even)
        ! combine
        do i=1,N/2
           t=exp(cmplx(0.0_cp,-2.0_cp*PI*real(i-1,cp)/real(N,cp),kind=cp))*even(i)
           x(i)     = odd(i) + t
           x(i+N/2) = odd(i) - t
        end do
      end subroutine
#else
      subroutine fft1D_full(x) ! Full Discrete Fourier Transform
        ! Computes
        !
        !                  N
        !    X(k) =       sum  x(n)*exp(-j*2*PI*(k-1)*(n-1)/N), 1 <= k <= N.
        !                 n=1
        !
        ! Notes: This routine computes the full FT (very slow), allowing for an
        !        arbitrary number of cells. If the number of cells
        !        is 2^N, where N>1, then use the Cooley-Tukey FFT below.
        !
        !
        complex(cp), dimension(:), intent(inout)  :: x
        complex(cp), dimension(:), allocatable    :: temp
        complex(cp)                               :: S,j ! sum, sqrt(-1)
        integer                                   :: N,i,k
        N=size(x)
        allocate(temp(N))
        j = cmplx(0.0_cp,1.0_cp,cp)
        S = cmplx(0.0_cp,0.0_cp,cp)
        temp = cmplx(0.0_cp,0.0_cp,cp)
        do i = 1,N
          do k = 1,N
            S = S + x(k)*exp(-2.0_cp*PI*j*real(k-1,cp)*real(i-1,cp)/real(N,cp))
          enddo
          temp(i) = S
          S = cmplx(0.0_cp,0.0_cp,cp)
        enddo
        x = temp
        deallocate(temp)
      end subroutine
#endif

      subroutine applyFFT3D(omega,f,dir,pad)
        implicit none
        complex(cp),dimension(:,:,:),intent(inout) :: omega
        real(cp),dimension(:,:,:),intent(in) :: f
        integer,intent(in) :: dir,pad
        integer,dimension(3) :: s
        integer :: i,j,k

        s = shape(f)

        !$OMP PARALLEL DO
        do k=1,s(3); do j=1,s(2); do i=1,s(1)
        omega(i,j,k) = f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO

        select case (dir)
        case (1)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad
            call fft1D(omega(:,j,k))
          enddo; enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
            call fft1D(omega(i,:,k))
          enddo; enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
            call fft1D(omega(i,j,:))
          enddo; enddo
          !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in applyFFT3D in fft.f90.'
        end select
      end subroutine


      end module