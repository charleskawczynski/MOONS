      module fft_mod
      ! Returns the fft of the scalar field, f, wrt direction 
      ! dir (1,2,3) which corresponds to (x,y,z).
      ! 
      ! Flags: (fopenmp,_DEBUG_FFT_)
      ! 
      ! Implementation:
      ! type(fft) :: ft
      ! call init(ft,shape(f))
      ! call d%assign  (ft,omega,f,g,dir,pad) --> omega = fft(f), 0 if not defined.
      ! call d%add     (ft,omega,f,g,dir,pad) --> omega = omega + fft(f)
      ! call d%subtract(ft,omega,f,g,dir,pad) --> omega = omega - fft(f)
      ! call d%multiply(ft,omega,f,g,dir,pad) --> omega = omega * fft(f)
      ! call d%divide  (ft,omega,f,g,dir,pad) --> omega = omega / fft(f)
      ! 
      ! INPUT:
      !     f            = f(x,y,z)
      !     g            = grid (g%c(1,2,3)%dhn, g%c(1,2,3)%dhc)
      !     dir          = direction along which to take the derivative (1,2,3)
      !     pad          = (1,0) = (exclude,include) boundary calc along derivative direction
      !                    |0000000|     |-------|
      !                    |-------|  ,  |-------| Look at del for implementation details  
      !                    |0000000|     |-------|
      !
      ! INDEXING: The index range of the incoming scalar field is assumed to begin at one.
      !
      ! CharlieKawczynski@gmail.com
      ! 6/12/2015

      use grid_mod
      use stencils_mod
      implicit none

      private
      public :: fft

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type fft
        real(cp),dimension(:),allocatable :: odd,even
      end type

      contains

      subroutine initFFT(ft,s,dir)
        implicit none
        type(fft),intent(inout) :: ft
        integer,dimension(3),intent(in) :: s
        integer,intent(in) :: dir
        integer,dimension(3) :: sOdd,sEven
        select case (dir)
        case (1); sEvem = (/s(1)/2,s(2),s(3)/); sOdd = (/(s(1)+1)/2,s(2),s(3)/)
        case (2); sEvem = (/s(1),s(2)/2,s(3)/); sOdd = (/s(1),(s(2)+1)/2,s(3)/)
        case (3); sEvem = (/s(1),s(2),s(3)/2/); sOdd = (/s(1),s(2),(s(3)+1)/2/)
        case default
        stop 'Error: dir must = 1,2,3 in initFFT in fft.f90'
        end select
        call allocateField(odd,sOdd)
        call allocateField(even,sEven)
      end subroutine

      recursive subroutine fft1D(f,even,odd,s) ! In place Cooley-Tukey FFT
        implicit none
        complex(cp),dimension(:), intent(inout) :: f
        complex(cp),dimension(:),intent(inout) :: even,odd
        integer,intent(in) :: s
        complex(cp) :: t
        integer :: i
        if(s .le. 1) return
        ! divide
        odd  = f(1:s:2)
        even = f(2:s:2)
        ! conquer
        call fft1D(odd)
        call fft1D(even)
        ! combine
        do i=1,s/2
           t=exp(cmplx(0.0_dp,-2.0_dp*pi*real(i-1,dp)/real(s,dp),dp))*even(i)
           f(i)     = odd(i) + t
           f(i+s/2) = odd(i) - t
        end do
      end subroutine fft1D

      subroutine fft3D(ft,omega,f,g,n,dir,pad,genType)
        implicit none
        type(fft),intent(inout) :: ft
        real(cp),dimension(:,:,:),intent(inout) :: omega
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad,genType
        integer,dimension(3) :: s
        integer :: i,j,k,diffType

        s = shape(f)

#ifdef _DEBUG_DEL_
        call checkDimensions(shape(f),shape(omega),dir)
#endif

        diffType = getDiffType(s,shape(omega),g%c(dir)%sn,g%c(dir)%sc,dir)

        select case (dir)
        case (1)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad
              call fft1D(omega(:,j,k),f(:,j,k),g%c(dir)%dhc,g%c(dir)%dhn,n,diffType,s(1),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
              call diff(omega(i,:,k),f(i,:,k),g%c(dir)%dhc,g%c(dir)%dhn,n,diffType,s(2),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
              call diff(omega(i,j,:),f(i,j,:),g%c(dir)%dhc,g%c(dir)%dhn,n,diffType,s(3),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in delGen in del.f90.'
        end select

        if (genType.eq.1) then
          if (pad.gt.0) then
            select case (dir)
          case (1); omega(:,:,1) = real(0.0,cp); omega(:,:,s(3)) = real(0.0,cp)
                    omega(:,1,:) = real(0.0,cp); omega(:,s(2),:) = real(0.0,cp)
          case (2); omega(1,:,:) = real(0.0,cp); omega(s(1),:,:) = real(0.0,cp)
                    omega(:,:,1) = real(0.0,cp); omega(:,:,s(3)) = real(0.0,cp)
          case (3); omega(1,:,:) = real(0.0,cp); omega(s(1),:,:) = real(0.0,cp)
                    omega(:,1,:) = real(0.0,cp); omega(:,s(2),:) = real(0.0,cp)
          case default
            stop 'Error: dir must = 1,2,3 in delGen in del.f90.'
            end select
          endif
        endif

      end subroutine

      ! ******************* OPERATOR TYPES ****************************

#ifdef _DEBUG_DEL_
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
        elseif (s1(dir).eq.(s2(dir)+1)) then ! Ok (N and CC)
        elseif ((s1(dir)+1).eq.s2(dir)) then ! Ok (CC and N)
        else; stop 'Error: shape mismatch 7 in del.f90'
        endif
      end subroutine
#endif


      end module