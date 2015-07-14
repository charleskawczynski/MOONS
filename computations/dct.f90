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
      !                    |-------|  ,  |-------| Look at fft for implementation details
      !                    |0000000|     |-------|
      !
      ! INDEXING: The index range of the incoming scalar field is assumed to begin at one.
      !
      ! CharlieKawczynski@gmail.com
      ! 7/12/2015

      use grid_mod
      implicit none

      private
      public :: apply

      interface apply;    module procedure applyDCT3D;    end interface


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif
       real(cp),parameter :: PI = real(3.14159265358979,cp)

      contains

      recursive subroutine dct1D(x)
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
        odd  = x(1:N:2)
        even = x(2:N:2)
        ! conquer
        call fft1D(odd)
        call fft1D(even)
        ! combine
        do i=1,N/2
           t=exp(cmplx(0.0_cp,-2.0_cp*pi*real(i-1,cp)/real(N,cp),cp))*even(i)
           x(i)     = odd(i) + t
           x(i+N/2) = odd(i) - t
        end do
        deallocate(odd)
        deallocate(even)
      end subroutine

      subroutine applyDCT3D(omega,f,g,dir,pad)
        implicit none
        complex(cp),dimension(:,:,:),intent(inout) :: omega
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
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
              call dct1D(omega(:,j,k),f(:,j,k))
          enddo; enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
              call dct1D(omega(i,:,k),f(i,:,k))
          enddo; enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
              call dct1D(omega(i,j,:),f(i,j,:))
          enddo; enddo
          !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in delGen in del.f90.'
        end select
      end subroutine

      ! ******************* OPERATOR TYPES ****************************

#ifdef _DEBUG_FFT_
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