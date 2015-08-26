      module triSolver_mod
      ! Solves a tridiagonal system of equations.
      ! Implementation:
      ! 
      ! type(triSolver) :: T
      ! real(cp),dimension(:,:,:) :: uOut,uIn
      ! integer :: dir,pad
      ! call apply(T,uOut,uIn,dir,pad) 
      ! 
      ! See applyTriSolver for more details

      implicit none
      
      private
      public :: trisolver
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

      type trisolver
        private
        real(cp),dimension(:),allocatable :: loDiag,diag,upDiag
        integer :: s
      end type

      interface init;    module procedure initTriSolver;            end interface
      interface delete;  module procedure deleteTriSolver;          end interface
      interface apply;   module procedure applyTriSolverInterior;   end interface

      contains

      subroutine initTriSolver(T,loDiag,diag,upDiag)
        implicit none
        type(trisolver),intent(inout) :: T
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

      subroutine deleteTriSolver(T)
        implicit none
        type(trisolver),intent(inout) :: T
        if (allocated(T%loDiag)) deallocate(T%loDiag)
        if (allocated(T%diag))   deallocate(T%diag)
        if (allocated(T%upDiag)) deallocate(T%upDiag)
        T%s = 0
      end subroutine

      subroutine applyTriSolverInterior(T,uOut,uIn,dir,pad)
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
        type(trisolver),intent(in) :: T
        real(cp),dimension(:,:,:),intent(inout) :: uOut
        real(cp),dimension(:,:,:),intent(in) :: uIn
        integer,intent(in) :: pad,dir
        integer :: i,j,k
        integer,dimension(3) :: s
        s = shape(uIn)

        ! write(*,*) 'minval T%diag = ',minval(T%diag(2:s(1)-1))
        ! write(*,*) 'minval T%loDiag = ',minval(T%loDiag(1:s(1)-1))
        ! write(*,*) 'minval T%upDiag = ',minval(T%upDiag(2:s(1)-1))
        ! write(*,*) 'maxval T%diag = ',maxval(T%diag(2:s(1)-1))
        ! write(*,*) 'maxval T%loDiag = ',maxval(T%loDiag(1:s(1)-1))
        ! write(*,*) 'maxval T%upDiag = ',maxval(T%upDiag(2:s(1)-1))
        ! write(*,*) 'T%diag = ',T%diag
        ! write(*,*) 'T%loDiag = ',T%loDiag
        ! write(*,*) 'T%upDiag = ',T%upDiag
        ! stop 'printed'
        select case (dir)
        case (1)
          !$OMP PARALLEL DO
           do k=1+pad,s(3)-pad
             do j=1+pad,s(2)-pad
               call triSolve(uOut(2:s(1)-1,j,k),uIn(2:s(1)-1,j,k),&
                T%loDiag(1:s(1)-2),T%diag(2:s(1)-1),T%upDiag(2:s(1)-1),s(1)-2)
             enddo
           enddo
          !$OMP END PARALLEL DO
        case (2)
         !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad
            do i=1+pad,s(1)-pad
              call triSolve(uOut(i,2:s(2)-1,k),uIn(i,2:s(2)-1,k),&
                T%loDiag(1:s(2)-2),T%diag(2:s(2)-1),T%upDiag(2:s(2)-1),s(2)-2)
            enddo
          enddo
         !$OMP END PARALLEL DO
        case (3)
         !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad
            do i=1+pad,s(1)-pad
              call triSolve(uOut(i,j,2:s(3)-1),uIn(i,j,2:s(3)-1),&
                T%loDiag(1:s(3)-2),T%diag(2:s(3)-1),T%upDiag(2:s(3)-1),s(3)-2)
            enddo
          enddo
         !$OMP END PARALLEL DO
        case default
        write(*,*) 'Error: dir must = 1,2,3 in applyTriSolverInterior.';stop
        end select
      end subroutine

      !---------------------Low level Tridiagonal solver---------------------

      subroutine triSolveEldredge(u,f,a,b,c,M)
        implicit none
        integer,intent(in) :: M
        real(cp),dimension(:),intent(inout) :: u,c,f
        real(cp),dimension(:),intent(in) :: a,b
        real(cp) :: temp,ctemp, ftemp
        integer j
        c(1) = c(1) / b(1)
        f(1) = f(1) / b(1)
        ctemp = c(1)
        ftemp = f(1)
        do j = 2, M-1
            temp = b(j) - a(j-1)*ctemp
            ctemp = c(j)/temp
            ftemp = (f(j) - a(j-1)*ftemp)/temp
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

      subroutine triSolve(u,f,loDiag,diag,upDiag,s)
        implicit none
        integer,intent(in) :: s
        real(cp),dimension(:),intent(inout) :: u
        real(cp),dimension(:),intent(in) :: f,diag
        real(cp),dimension(:),intent(in) :: loDiag,upDiag
        real(cp),dimension(:),allocatable :: upDiagTemp,fTemp
        allocate(upDiagTemp(s-1))
        allocate(fTemp(s))
        upDiagTemp = upDiag; fTemp = f
        call triSolveEldredge(u,fTemp,loDiag,diag,upDiagTemp,s)
        deallocate(upDiagTemp,fTemp)
      end subroutine

      end module
