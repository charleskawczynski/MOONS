      module myTriOperator_mod
      ! Forms the RHS of a tridiagonal system.
      ! Implementation:
      ! 
      ! type(myTriOperator) :: T
      ! real(dpn),dimension(:,:,:) :: uOut,uIn
      ! integer :: dir,pad
      ! call apply(T,uOut,uIn,dir,pad) 
      ! 
      ! See applyTriOperator for more details

      use constants_mod
      implicit none
      
      private
      public :: myTriOperator
      public :: initialize, delete, apply

      type myTriOperator
        private
        real(dpn),dimension(:),allocatable :: loDiag,diag,upDiag
        integer :: s
      end type

      interface initialize;   module procedure initializeTriOperator;   end interface
      interface delete;       module procedure deleteTriOperator;       end interface
      interface apply;        module procedure applyTriOperator;        end interface
      interface triOperate;   module procedure triOperateAnderson;      end interface

      contains

      subroutine initializeTriOperator(T,loDiag,diag,upDiag)
        implicit none
        type(myTriOperator),intent(inout) :: T
        real(dpn),dimension(:),intent(in) :: loDiag,diag,upDiag
        T%s = size(diag)
        if (allocated(T%loDiag)) deallocate(T%loDiag)
        if (allocated(T%diag))   deallocate(T%diag)
        if (allocated(T%upDiag)) deallocate(T%upDiag)

        allocate(T%loDiag(T%s-1))
        allocate(T%diag(T%s))
        allocate(T%upDiag(T%s-1))
        T%loDiag = loDiag
        T%diag = diag
        T%upDiag = upDiag
      end subroutine

      subroutine deleteTriOperator(T)
        implicit none
        type(myTriOperator),intent(inout) :: T
        if (allocated(T%loDiag)) deallocate(T%loDiag)
        if (allocated(T%diag))   deallocate(T%diag)
        if (allocated(T%upDiag)) deallocate(T%upDiag)
        T%s = 0
      end subroutine

      subroutine applyTriOperator(T,uOut,uIn,dir,pad)
        ! Returns
        !        |  diag(1)  upDiag(1)                                  |
        !        |loDiag(1)    diag(2) upDiag(2)                        |
        !        |           loDiag(2)                                  |
        ! uOut = |                *       *       *                     | uIn
        !        |                   *         *        *               |
        !        |                loDiag(n-2)   diag(n-1)  upDiag(n-1)  |
        !        |                             loDiag(n-1)   diag(n)    |
        ! 
        ! Note that this matrix is defined in setUpSystem (in myADI.f90) as:
        ! 
        !        |    1         0                                       |
        !        |loDiag(1)    diag(2) upDiag(2)                        |
        !        |           loDiag(2)                                  |
        ! uOut = |                *       *       *                     | uIn
        !        |                   *         *        *               |
        !        |                loDiag(n-2)   diag(n-1)  upDiag(n-1)  |
        !        |                                  0           1       |
        implicit none
        type(myTriOperator),intent(in) :: T
        real(dpn),dimension(:,:,:),intent(inout) :: uOut ! fstar
        real(dpn),dimension(:,:,:),intent(in) :: uIn
        integer,intent(in) :: dir
        integer,intent(in) :: pad
        integer :: i,j,k
        integer,dimension(3) :: s
        s = shape(uIn)

        select case (dir)
        case (1)
          !$OMP PARALLEL DO
           do k=1+pad,s(3)-pad
             do j=1+pad,s(2)-pad
               call triOperate(uOut(:,j,k),uIn(:,j,k),T%loDiag,T%diag,T%upDiag,s(1))
             enddo
           enddo
          !$OMP END PARALLEL DO
        case (2)
         !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad
            do i=1+pad,s(1)-pad
              call triOperate(uOut(i,:,k),uIn(i,:,k),T%loDiag,T%diag,T%upDiag,s(2))
            enddo
          enddo
         !$OMP END PARALLEL DO
        case (3)
         !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad
            do i=1+pad,s(1)-pad
              call triOperate(uOut(i,j,:),uIn(i,j,:),T%loDiag,T%diag,T%upDiag,s(3))
            enddo
          enddo
         !$OMP END PARALLEL DO
        case default
        write(*,*) 'Error: dir must = 1,2,3 in solveTriOperator.';stop
        end select

      end subroutine

      !---------------------Low level Tridiagonal Operator---------------------

      subroutine triOperateAnderson(uOut,uIn,loDiag,diag,upDiag,n)
        implicit none
        real(dpn),dimension(:),intent(inout) :: uOut
        real(dpn),dimension(:),intent(in) :: uIn,diag
        real(dpn),dimension(:),intent(in) :: loDiag,upDiag
        integer,intent(in) :: n
        integer :: j
        if (n.eq.1) then
          uOut(1) = diag(1)*uIn(1); return
        endif
        uOut(1) = diag(1)*uIn(1) + upDiag(1)*uIn(2)
        do j = 2,n-1
          uOut(j) = loDiag(j-1)*uIn(j-1) + diag(j)*uIn(j) + upDiag(j)*uIn(j+1)
        enddo
        j = n
        uOut(j) = loDiag(j-1)*uIn(j-1) + diag(j)*uIn(j)
      end subroutine

      end module
