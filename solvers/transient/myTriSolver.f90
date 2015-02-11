      module myTriSolver_mod
      ! Solves a tridiagonal system of equations.
      ! Implementation:
      ! 
      ! type(myTriSolver) :: T
      ! real(dpn),dimension(:,:,:) :: uOut,uIn
      ! integer :: dir,pad
      ! call apply(T,uOut,uIn,dir,pad) 
      ! 
      ! See applyTriSolver for more details

      use constants_mod
      implicit none
      
      private
      public :: myTrisolver
      public :: initialize, delete, apply

      type myTrisolver
        private
        real(dpn),dimension(:),allocatable :: loDiag,diag,upDiag
        integer :: s
      end type

      interface initialize; module procedure initializeTriSolver;   end interface
      interface delete;     module procedure deleteTriSolver;       end interface
      interface apply;      module procedure applyTriSolver;        end interface

      contains

      subroutine initializeTriSolver(T,loDiag,diag,upDiag)
        implicit none
        type(myTrisolver),intent(inout) :: T
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

      subroutine deleteTriSolver(T)
        implicit none
        type(myTrisolver),intent(inout) :: T
        if (allocated(T%loDiag)) deallocate(T%loDiag)
        if (allocated(T%diag))   deallocate(T%diag)
        if (allocated(T%upDiag)) deallocate(T%upDiag)
        T%s = 0
      end subroutine

      subroutine applyTriSolver(T,uOut,uIn,dir,pad)
        ! Returns
        !           _                                                    _ -1
        !          |  diag(1)  upDiag(1)                                  |
        !          |loDiag(1)    diag(2) upDiag(2)                        |
        !          |           loDiag(2)                                  |
        ! uOut =   |                *       *       *                     | uIn
        !          |                   *         *        *               |
        !          |                loDiag(n-2)   diag(n-1)  upDiag(n-1)  |
        !          |_                            loDiag(n-1)   diag(n)   _|
        ! 
        ! Note that this matrix is defined in setUpSystem (in myADI.f90) as:
        ! 
        !           _                                                    _ -1
        !          |    1           0                                     |
        !          |loDiag(1)    diag(2) upDiag(2)                        |
        !          |           loDiag(2)                                  |
        ! uOut =   |                *       *       *                     | uIn
        !          |                   *         *        *               |
        !          |                loDiag(n-2)   diag(n-1)  upDiag(n-1)  |
        !          |_                                 0           1      _|
        ! 
        implicit none
        type(myTrisolver),intent(in) :: T
        real(dpn),dimension(:,:,:),intent(inout) :: uOut
        real(dpn),dimension(:,:,:),intent(in) :: uIn
        integer,intent(in) :: pad,dir
        integer :: i,j,k
        integer,dimension(3) :: s
        s = shape(uIn)

        select case (dir)
        case (1)
          !$OMP PARALLEL DO
           do k=1+pad,s(3)-pad
             do j=1+pad,s(2)-pad
               call triSolve(uOut(:,j,k),uIn(:,j,k),T%loDiag,T%diag,T%upDiag,s(1))
             enddo
           enddo
          !$OMP END PARALLEL DO
        case (2)
         !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad
            do i=1+pad,s(1)-pad
              call triSolve(uOut(i,:,k),uIn(i,:,k),T%loDiag,T%diag,T%upDiag,s(2))
            enddo
          enddo
         !$OMP END PARALLEL DO
        case (3)
         !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad
            do i=1+pad,s(1)-pad
              call triSolve(uOut(i,j,:),uIn(i,j,:),T%loDiag,T%diag,T%upDiag,s(3))
            enddo
          enddo
         !$OMP END PARALLEL DO
        case default
        write(*,*) 'Error: dir must = 1,2,3 in solveTriSolver.';stop
        end select

      end subroutine

      !---------------------Low level Tridiagonal solver---------------------

      subroutine triSolveEldredge(u,f,a,b,c,M)
        implicit none
        integer,intent(in) :: M
        real(dpn),dimension(:),intent(inout) :: u,c,f
        real(dpn),dimension(:),intent(in) :: a,b
        real(dpn) :: temp,ctemp, ftemp, atemp
        integer j
        c(1) = c(1) / b(1)
        f(1) = f(1) / b(1)
        ctemp = c(1)
        ftemp = f(1)
        atemp = a(1)
        do j = 2, M-1
            temp = b(j) - atemp*ctemp
            ctemp = c(j)/temp
            atemp = a(j-1)
            ftemp = (f(j) - atemp*ftemp)/temp
            c(j) = ctemp
            f(j) = ftemp
        enddo
        f(M) = (f(M)-a(M-1)*f(M-1))/(b(M)-a(M-1)*c(M-1))
        temp = f(M)
        u(M) = temp
        do j = M-1, 2, -1
            temp = f(j) - c(j)*temp
            u(j) = temp
        enddo
        u(1) = f(1) - c(1)*temp
      end subroutine

      subroutine triSolve(u,f,loDiag,diag,upDiag,M)
        implicit none
        integer,intent(in) :: M
        real(dpn),dimension(:),intent(inout) :: u
        real(dpn),dimension(:),intent(in) :: f,diag
        real(dpn),dimension(:),intent(in) :: loDiag,upDiag
        real(dpn),dimension(:),allocatable :: loDiagTemp,upDiagTemp,fTemp
        allocate(loDiagTemp(M-1))
        allocate(upDiagTemp(M-1))
        allocate(fTemp(M))
        loDiagTemp = loDiag
        upDiagTemp = upDiag
        fTemp = f
        call triSolveEldredge(u,fTemp,loDiag,diag,upDiagTemp,M)
        deallocate(loDiagTemp,upDiagTemp,fTemp)
      end subroutine

      end module