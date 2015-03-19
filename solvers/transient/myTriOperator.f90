      module myTriOperator_mod
      ! Forms the RHS of a tridiagonal system.
      ! Implementation:
      ! 
      ! type(myTriOperator) :: T
      ! real(cp),dimension(:,:,:) :: uOut,uIn
      ! integer :: dir,pad
      ! call apply(T,uOut,uIn,dir,pad) 
      ! 
      ! See applyTriOperator for more details

      implicit none
      
      private
      public :: myTriOperator
      public :: init, delete, apply

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type myTriOperator
        private
        real(cp),dimension(:),allocatable :: loDiag,diag,upDiag
        integer :: s
      end type

      interface init;         module procedure initTriOperator;           end interface
      interface delete;       module procedure deleteTriOperator;         end interface
      interface apply;        module procedure applyTriOperator;          end interface
      ! interface apply;        module procedure applyTriOperatorInterior;  end interface
      interface triOperate;   module procedure triOperateAnderson;        end interface

      contains

      subroutine initTriOperator(T,loDiag,diag,upDiag)
        implicit none
        type(myTriOperator),intent(inout) :: T
        real(cp),dimension(:),intent(in) :: loDiag,diag,upDiag
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
        real(cp),dimension(:,:,:),intent(inout) :: uOut ! fstar
        real(cp),dimension(:,:,:),intent(in) :: uIn
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

      subroutine applyTriOperatorInterior(T,uOut,uIn,dir,pad)
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
        real(cp),dimension(:,:,:),intent(inout) :: uOut ! fstar
        real(cp),dimension(:,:,:),intent(in) :: uIn
        integer,intent(in) :: dir
        integer,intent(in) :: pad
        integer :: i,j,k
        integer,dimension(3) :: s
        s = shape(uIn)
        uOut = real(0.0,cp)

        select case (dir)
        case (1)
          !$OMP PARALLEL DO
           do k=1+pad,s(3)-pad
             do j=1+pad,s(2)-pad
               call triOperate(uOut(2:s(1)-1,j,k),uIn(2:s(1)-1,j,k),&
                T%loDiag(2:s(1)-2),T%diag(2:s(1)-1),T%upDiag(2:s(1)-2),s(1)-2)
             enddo
           enddo
          !$OMP END PARALLEL DO
        case (2)
         !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad
            do i=1+pad,s(1)-pad
              call triOperate(uOut(i,2:s(2)-1,k),uIn(i,2:s(2)-1,k),&
                T%loDiag(2:s(2)-2),T%diag(2:s(2)-1),T%upDiag(2:s(2)-2),s(2)-2)
            enddo
          enddo
         !$OMP END PARALLEL DO
        case (3)
         !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad
            do i=1+pad,s(1)-pad
              call triOperate(uOut(i,j,2:s(3)-1),uIn(i,j,2:s(3)-1),&
                T%loDiag(2:s(3)-2),T%diag(2:s(3)-1),T%upDiag(2:s(3)-2),s(3)-2)
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
        real(cp),dimension(:),intent(inout) :: uOut
        real(cp),dimension(:),intent(in) :: uIn,diag
        real(cp),dimension(:),intent(in) :: loDiag,upDiag
        integer,intent(in) :: n
        integer :: j
        if (n.eq.1) then
          uOut(1) = diag(1)*uIn(1); return
        endif
        uOut(1) = diag(1)*uIn(1) + upDiag(1)*uIn(2)
        ! uOut(1) = 0.0
        do j = 2,n-1
          uOut(j) = loDiag(j-1)*uIn(j-1) + diag(j)*uIn(j) + upDiag(j)*uIn(j+1)
        enddo
        j = n
        uOut(j) = loDiag(j-1)*uIn(j-1) + diag(j)*uIn(j)
        ! uOut(j) = 0.0
      end subroutine

      end module
