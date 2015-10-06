      module ops_fft_mod
      ! Returns the Fast Fourier Transform of the scalar field, f, wrt direction 
      ! dir (1,2,3) which corresponds to (x,y,z).
      ! 
      ! Flags: (fopenmp,_DEBUG_FFT_)
      ! 
      ! Implementation:
      ! call fft(omega,f,dir,pad)
      ! 
      ! INPUT:
      !     f    = f(x,y,z)
      !     dir  = direction along which to take the FT (1,2,3)
      !     pad  = (1,0) = (exclude,include) boundary calc along FT direction
      !            |0000000|     |-------|
      !            |-------|  ,  |-------| Look at fft for implementation details
      !            |0000000|     |-------|
      !
      ! INDEXING: The index range of the incoming scalar field is assumed to begin at one.
      !
      ! CharlieKawczynski@gmail.com
      ! 7/12/2015
      ! 
      ! Good references:
      ! 
      ! https://jakevdp.github.io/blog/2013/08/28/understanding-the-fft/

      use grid_mod
      implicit none

      private
      public :: fft,fft1D
      interface fft;      module procedure applyFFT3D;    end interface
#ifdef _FFT_RADIX2_
      interface fft1D;    module procedure fft1D_Radix2;      end interface
#endif
#ifdef _FFT_FULL_
      interface fft1D;    module procedure fft1D_full;    end interface
#endif


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif
       ! integer,parameter :: cip = selected_int_kind(64)
       ! real(cp),parameter :: PI = 3.1415926535897932384626433832795028841971693993751058_cp
       real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

      contains

#ifdef _FFT_FULL_
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

#ifdef _FFT_RADIX2_
      recursive subroutine fft1D_Radix2(x) ! In place Cooley-Tukey FFT
        ! Computes
        ! 
        !                  N
        !    X(k) =       sum  x(n)*exp(-j*2*PI*(k-1)*(n-1)/N), 1 <= k <= N.
        !                 n=1
        ! 
        ! Notes: Only valid for when the number of cells is 2^N, where N>1
        ! 
        complex(cp), dimension(:), intent(inout)  :: x
        complex(cp)                               :: t
        integer                                   :: N
        integer                                   :: i
        complex(cp), dimension(:), allocatable    :: even, odd
        N=size(x)
        if(N .le. 1) return
        allocate(odd((N+1)/2))
        allocate(even(N/2))
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
        deallocate(odd)
        deallocate(even)
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

#ifdef _DEBUG_FFT_
        call checkDimensions(s,shape(omega),dir)
#endif

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

      ! ******************* OPERATOR TYPES ****************************

#ifdef _DEBUG_FFT_
      subroutine checkDimensions(s1,s2,dir)
        ! This routine makes sure that the shapes s1 and s2 
        ! are correct
        implicit none
        integer,dimension(3),intent(in) :: s1,s2
        integer,intent(in) :: dir
        select case (dir)
        case (1); if (s1(2).ne.s2(2)) stop 'Error: Shape mismatch 1 in fft'
                  if (s1(3).ne.s2(3)) stop 'Error: Shape mismatch 2 in fft'
        case (2); if (s1(1).ne.s2(1)) stop 'Error: Shape mismatch 3 in fft'
                  if (s1(3).ne.s2(3)) stop 'Error: Shape mismatch 4 in fft'
        case (3); if (s1(1).ne.s2(1)) stop 'Error: Shape mismatch 5 in fft'
                  if (s1(2).ne.s2(2)) stop 'Error: Shape mismatch 6 in fft'
        case default
        stop 'Error: dir must = 1,2,3 in fft.f90'
        end select
        if (s1(dir).eq.s2(dir)) then         ! Ok (collocated)
        else; stop 'Error: shape mismatch 7 in fft.f90'
        endif
      end subroutine
#endif

      end module