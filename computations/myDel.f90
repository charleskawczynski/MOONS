      module myDel_mod
      ! call myDel(dfdh,f,g,n,dir,diffType,pad) returns an nth-derivative of the
      ! scalar field, f, along direction dir (1,2,3) which corresponds to (x,y,z).
      ! 
      ! INPUT:
      !     f            = f(x,y,z)
      !     g            = grid (g%c(1,2,3)%dhn, g%c(1,2,3)%dhc)
      !     n            = nth derivative (n=1,2 supported)
      !     dir          = direction along which to take the derivative (1,2,3)
      !     diffType     = (1,2,3,4) derivative type, refer to myDiff for more details
      !     pad          = (1,0) = (exclude,include) boundary calc along derivative direction
      !                    |0000000|     |-------|
      !                    |-------|  ,  |-------| Look at myDel for implementation details  
      !                    |0000000|     |-------|
      !
      ! INDEXING: The index range of the incoming scalar field is assumed to begin at one.
      !
      ! CharlieKawczynski@gmail.com
      ! 5/15/2014

      use grid_mod
      implicit none

      private
      public :: myDel ! call myDel(dfdh,f,g,n,dir,diffType,pad)

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      contains

      subroutine myDiff(dfdh,f,dhc,dhn,n,diffType,s)
        implicit none
        real(cp),dimension(:),intent(in) :: f
        real(cp),dimension(:),intent(in) :: dhc,dhn
        integer,intent(in) :: n,diffType,s
        real(cp),dimension(:),intent(inout) :: dfdh

        select case (diffType)
        case (1); call diff(dfdh,f,dhc,dhn,n,s)      ! Collocated CellCenter derivative
        case (2); call diff(dfdh,f,dhn,dhc,n,s)      ! Collocated Node derivative
        
        case (3); call upwind(dfdh,f,dhc,s)          ! Cell centered upwind derivative
        case (4); call upwind(dfdh,f,dhn,s)          ! Node centered upwind derivative
        end select
      end subroutine

      subroutine upwind(dfdh,f,dh,s)
        implicit none
        real(cp),dimension(:),intent(in) :: f
        real(cp),dimension(:),intent(inout) :: dfdh
        real(cp),dimension(:),intent(in) :: dh
        integer,intent(in) :: s
        integer :: i
        ! Interior
        dfdh(1:s-1) = dfdh(1:s-1) + (/((f(i+1)-f(i))/dh(i),i=1,s-1)/)
      end subroutine

      subroutine diff(dfdh,f,dh1,dh2,n,s)
        implicit none
        real(cp),intent(in),dimension(:) :: f
        real(cp),dimension(:),intent(inout) :: dfdh
        real(cp),intent(in),dimension(:) :: dh1,dh2
        integer,intent(in) :: n,s
        integer :: i,j,k
        real(cp) :: alpha,beta

        select case (n)
        case (1)
          ! Interior
          k = -1; j = 1
          do i=2,s-1
            alpha = -dh1(i-1); beta = dh1(i)
            dfdh(i) = dfdh(i) + (f( i )*(alpha/beta-beta/alpha) +&
                      f(i+k)*beta/alpha + &
                      f(i+j)*(-alpha/beta))/(beta-alpha)
          enddo

          ! Forward difference
          i = 1; k = 1; j = 2
          alpha = dh1(1); beta = dh1(1) + dh1(2)
          dfdh(i) = dfdh(i) + (f( i )*(alpha/beta-beta/alpha) +&
                    f(i+k)*beta/alpha + &
                    f(i+j)*(-alpha/beta))/(beta-alpha)

          ! Backward difference
          i = s; k = -1; j = -2
          alpha = -dh1(s-1); beta = -(dh1(s-1) + dh1(s-2))
          dfdh(i) = dfdh(i) + (f( i )*(alpha/beta-beta/alpha) +&
                    f(i+k)*beta/alpha + &
                    f(i+j)*(-alpha/beta))/(beta-alpha)
          
        case (2)
          ! Interior
          do i=2,s-1
            dfdh(i) = dfdh(i) + ((f(i+1)-f(i))/dh1(i) - (f(i)-f(i-1))/dh1(i-1))/dh2(i-1)
          enddo
          
        ! These formulas are identical to the derivative above.
        ! They involves taking the derivative AND interpolating it
        ! to the location where the values are stored (collocated)
        ! Order of accuracy = O(-beta*alpha - (beta + alpha))
        ! 
        ! NOTE: I have not added the incoming array here
        ! ! Central difference
        ! k = -1; j = 1
        ! do i=2,s-1
        !   alpha = -dh1(i-1); beta = dh1(i)
        !   dfdh(i) = 2.0*f(i)/(alpha*beta) + &
        !             2.0*f(i+k)/(alpha**2.0 - alpha*beta) + &
        !             2.0*f(i+j)/(beta**2.0 - alpha*beta)
        ! enddo
        ! ! Forward difference
        ! i = 1; k = 1; j = 2
        ! alpha = dh1(i); beta = dh1(i) + dh1(i+1)
        ! dfdh(i) = 2.0*f(i)/(alpha*beta) + &
        !           2.0*f(i+k)/(alpha**2.0 - alpha*beta) + &
        !           2.0*f(i+j)/(beta**2.0 - alpha*beta)
        ! ! Backward difference
        ! i = s; k = -1; j = -2
        ! alpha = -dh1(s-1); beta = -(dh1(s-1) + dh1(s-2))
        ! dfdh(i) = 2.0*f(i)/(alpha*beta) + &
        !           2.0*f(i+k)/(alpha**2.0 - alpha*beta) + &
        !           2.0*f(i+j)/(beta**2.0 - alpha*beta)
        end select
      end subroutine

      subroutine myDel(dfdh,f,g,nth,dir,diffType,pad,addTo)
        ! The order in the loops have been changed for better memory caching.
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        logical,intent(in),optional :: addTo
        integer,dimension(3) :: s
        integer :: i,j,k

        s = shape(f)

        if (.not.present(addTo)) then
          dfdh = real(0.0,cp) ! Zero result first
        endif
        
        select case (dir)
        case (1)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad
            do j=1+pad,s(2)-pad
              call myDiff(dfdh(:,j,k),f(:,j,k),g%c(dir)%dhc,g%c(dir)%dhn,nth,diffType,s(1))
            enddo
          enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad
            do i=1+pad,s(1)-pad
              call myDiff(dfdh(i,:,k),f(i,:,k),g%c(dir)%dhc,g%c(dir)%dhn,nth,diffType,s(2))
            enddo
          enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad
            do i=1+pad,s(1)-pad
              call myDiff(dfdh(i,j,:),f(i,j,:),g%c(dir)%dhc,g%c(dir)%dhn,nth,diffType,s(3))
            enddo
          enddo
          !$OMP END PARALLEL DO
        end select
      end subroutine

      end module