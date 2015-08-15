      module dct_mod
      ! Returns the Discrete Cosine Transform of the scalar field, f, wrt direction 
      ! dir (1,2,3) which corresponds to (x,y,z).
      ! 
      ! Flags: (fopenmp,_DEBUG_DCT_)
      ! 
      ! Implementation:
      ! call apply(omega,f,g,dir,pad)
      ! 
      ! INPUT:
      !     f            = f(x,y,z)
      !     g            = grid (g%c(1,2,3)%dhn, g%c(1,2,3)%dhc)
      !     dir          = direction along which to take the fourier transform (1,2,3)
      !     pad          = (1,0) = (exclude,include) boundary calc along fourier transform direction
      !                    |0000000|     |-------|
      !                    |-------|  ,  |-------| Look at dct for implementation details
      !                    |0000000|     |-------|
      !
      ! INDEXING: The index range of the incoming scalar field is assumed to begin at one.
      !
      ! CharlieKawczynski@gmail.com
      ! 7/12/2015

      use grid_mod
      use fft_mod
      implicit none

      private
      public :: dct,dct1D
      interface dct;    module procedure applyDCT3D;    end interface


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif
       ! real(cp),parameter :: PI = 3.1415926535897932384626433832795028841971693993751058_cp
       real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

      contains

      subroutine dct1D(x)
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
        real(cp),    dimension(:), intent(inout)  :: x
        complex(cp), dimension(:), allocatable    :: xx,e
        integer                                   :: N,k
        complex(cp)                               :: j
        N = size(x)
        allocate(xx(2*N))
        allocate(e(N))
        xx = 0.0_cp
        e = 0.0_cp
        j = cmplx(0.0_cp,1.0_cp,cp)
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
        deallocate(xx)
        deallocate(e)
      end subroutine

      subroutine applyDCT3D(f,dir,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: f
        integer,intent(in) :: dir,pad
        integer,dimension(3) :: s
        integer :: i,j,k

        s = shape(f)

#ifdef _DEBUG_DCT_
        call checkDimensions(s,shape(f),dir)
#endif

        select case (dir)
        case (1)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad
            call dct1D(f(:,j,k))
          enddo; enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
            call dct1D(f(i,:,k))
          enddo; enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
            call dct1D(f(i,j,:))
          enddo; enddo
          !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in delGen in del.f90.'
        end select
      end subroutine

      ! ******************* OPERATOR TYPES ****************************

#ifdef _DEBUG_DCT_
      subroutine checkDimensions(s1,s2,dir)
        ! This routine makes sure that the shapes s1 and s2 
        ! are equal for orthogonal directions to dir, which
        ! must be the case for all derivatives in del.
        implicit none
        integer,dimension(3),intent(in) :: s1,s2
        integer,intent(in) :: dir
        select case (dir)
        case (1); if (s1(2).ne.s2(2)) stop 'Error: Shape mismatch 1 in del'
                  if (s1(3).ne.s2(3)) stop 'Error: Shape mismatch 2 in del'
        case (2); if (s1(1).ne.s2(1)) stop 'Error: Shape mismatch 3 in del'
                  if (s1(3).ne.s2(3)) stop 'Error: Shape mismatch 4 in del'
        case (3); if (s1(1).ne.s2(1)) stop 'Error: Shape mismatch 5 in del'
                  if (s1(2).ne.s2(2)) stop 'Error: Shape mismatch 6 in del'
        case default
        stop 'Error: dir must = 1,2,3 in del.f90'
        end select
        if (s1(dir).eq.s2(dir)) then         ! Ok (collocated)
        else; stop 'Error: shape mismatch 7 in del.f90'
        endif
      end subroutine
#endif

      end module