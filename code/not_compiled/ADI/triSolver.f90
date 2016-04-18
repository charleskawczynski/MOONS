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
      use current_precision_mod
      use triDiag_mod
      use SF_mod
      use BCs_mod
      implicit none
      
      private
      public :: triSolver
      public :: init,delete,apply

      type triSolver
        type(triDiag) :: T
        type(triDiag) :: LU ! Stored values for LU factorization
      end type

      interface init;    module procedure init_TriSolver_LDU;          end interface
      interface init;    module procedure init_TriSolver_T;            end interface
      interface init;    module procedure init_TriSolver_copy;         end interface
      interface delete;  module procedure delete_TriSolver;            end interface
      interface apply;   module procedure apply_TriSolverInterior_SF;  end interface
      interface apply;   module procedure apply_TriSolverInterior_RF;  end interface

      contains

      ! ********************************************************************
      ! **************************** INIT/DELETE ***************************
      ! ********************************************************************

      subroutine init_TriSolver_LDU(TS,L,D,U)
        implicit none
        type(triSolver),intent(inout) :: TS
        real(cp),dimension(:),intent(in) :: L,D,U
        call init(TS%T,L,D,U)
      end subroutine

      subroutine init_TriSolver_T(TS,T)
        implicit none
        type(triSolver),intent(inout) :: TS
        type(triDiag),intent(in) :: T
        call init(TS%T,T)
      end subroutine

      subroutine init_TriSolver_copy(TS_out,TS_in)
        implicit none
        type(triSolver),intent(inout) :: TS_out
        type(triSolver),intent(in) :: TS_in
        call init(TS_out%T,TS_in%T%L,TS_in%T%D,TS_in%T%U)
      end subroutine

      subroutine delete_TriSolver(TS)
        implicit none
        type(triSolver),intent(inout) :: TS
        call delete(TS%T)
      end subroutine

      ! ********************************************************************
      ! ******************************* SOLVE ******************************
      ! ********************************************************************

      subroutine apply_TriSolverInterior_SF(TS,uOut,uIn,dir,pad)
        implicit none
        type(triSolver),intent(in) :: TS
        type(SF),intent(inout) :: uOut
        type(SF),intent(in) :: uIn
        integer,intent(in) :: pad,dir
        integer :: i
        logical :: CC
        CC = CC_along(uIn,dir)
        do i=1,uOut%s
          call apply_TriSolverInterior_RF(uOut%RF(i)%f,uIn%RF(i)%f,TS%T,uOut%RF(i)%b,dir,uOut%RF(i)%s,pad,CC)
        enddo
      end subroutine

      subroutine apply_TriSolverInterior_RF(uOut,uIn,T,b,dir,s,pad,CC)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: uOut
        real(cp),dimension(:,:,:),intent(in) :: uIn
        type(BCs),intent(in) :: b
        type(triDiag),intent(in) :: T
        integer,dimension(3),intent(in) :: s
        integer,intent(in) :: pad,dir
        logical,intent(in) :: CC
        integer :: i,j,k
        if (CC) then
          select case (dir)
          case (1)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad
          call triSolve_Dirichlet_CC(uOut(:,j,k),uIn(:,j,k),T,b%f(1)%vals(j,k),b%f(4)%vals(j,k),s(1))
          enddo; enddo
          !$OMP END PARALLEL DO
          case (2)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
          call triSolve_Dirichlet_CC(uOut(i,:,k),uIn(i,:,k),T,b%f(2)%vals(i,k),b%f(5)%vals(i,k),s(2))
          enddo; enddo
          !$OMP END PARALLEL DO
           case (3)
          !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          call triSolve_Dirichlet_CC(uOut(i,j,:),uIn(i,j,:),T,b%f(3)%vals(i,j),b%f(6)%vals(i,j),s(3))
          enddo; enddo
          !$OMP END PARALLEL DO
          case default
          write(*,*) 'Error: dir must = 1,2,3 in solveTriSolver.';stop
          end select
        else
          select case (dir)
          case (1)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad
          call triSolve_Dirichlet_node(uOut(:,j,k),uIn(:,j,k),T,b%f(1)%vals(j,k),b%f(4)%vals(j,k),s(1))
          enddo; enddo
          !$OMP END PARALLEL DO
          case (2)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
          call triSolve_Dirichlet_node(uOut(i,:,k),uIn(i,:,k),T,b%f(2)%vals(i,k),b%f(5)%vals(i,k),s(2))
          enddo; enddo
          !$OMP END PARALLEL DO
           case (3)
          !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          call triSolve_Dirichlet_node(uOut(i,j,:),uIn(i,j,:),T,b%f(3)%vals(i,j),b%f(6)%vals(i,j),s(3))
          enddo; enddo
          !$OMP END PARALLEL DO
          case default
          write(*,*) 'Error: dir must = 1,2,3 in solveTriSolver.';stop
          end select
        endif
      end subroutine

      ! ********************************************************************
      ! ***************************** DIRICHLET ****************************
      ! ********************************************************************

      subroutine triSolve_Dirichlet(x,d,T,b_front,b_back,n)
        implicit none
        real(cp),dimension(n),intent(inout) :: x
        real(cp),dimension(n),intent(in) :: d
        real(cp),intent(in) :: b_front,b_back
        type(triDiag),intent(in) :: T
        integer,intent(in) :: n
        call triSolve_Dirichlet_CC(x,d,T,b_front,b_back,n)
        ! call triSolve_Dirichlet_node(x,d,T,b_front,b_back,n)
      end subroutine

      subroutine triSolve_Dirichlet_CC(x,d,T,b_front,b_back,n)
        ! Checked, works well for non-uniform grids
        implicit none
        real(cp),dimension(n),intent(inout) :: x
        real(cp),dimension(n),intent(in) :: d
        real(cp),intent(in) :: b_front,b_back
        type(triDiag),intent(in) :: T
        integer,intent(in) :: n
        real(cp),dimension(n) :: dtemp
        dtemp = d
        dtemp(2)   = dtemp(2)   - T%L(1)   * b_front
        dtemp(n-1) = dtemp(n-1) - T%U(n-2) * b_back
        call solve_tridiag_CK(x(2:n-1),&
                              dtemp(2:n-1),&
                              T%L(2:n-2),&
                              T%D,&
                              T%U(1:n-3),&
                              n-2)
      end subroutine

      subroutine triSolve_Dirichlet_node(x,d,T,b_front,b_back,n)
        ! Checked, works well for non-uniform grids
        implicit none
        real(cp),dimension(n),intent(inout) :: x
        real(cp),dimension(n),intent(in) :: d
        real(cp),intent(in) :: b_front,b_back
        type(triDiag),intent(in) :: T
        integer,intent(in) :: n
        real(cp),dimension(n) :: dtemp
        dtemp = d
        dtemp(3)   = dtemp(3)   - T%L(2)   * b_front
        dtemp(n-2) = dtemp(n-2) - T%U(n-3) * b_back
        call solve_tridiag_CK(x(3:n-2),&
                              dtemp(3:n-2),&
                              T%L(3:n-3),&
                              T%D(2:n-3),&
                              T%U(2:n-4),&
                              n-4)
      end subroutine

      ! ********************************************************************
      ! ****************************** NEUMANN *****************************
      ! ********************************************************************

      subroutine triSolve_Neumann(x,d,T,b_front,b_back,n)
        implicit none
        real(cp),dimension(n),intent(inout) :: x
        real(cp),dimension(n),intent(in) :: d
        real(cp),intent(in) :: b_front,b_back
        type(triDiag),intent(in) :: T
        integer,intent(in) :: n
        call triSolve_Neumann_CC(x,d,T,b_front,b_back,n)
        ! call triSolve_Neumann_node(x,d,T,b_front,b_back,n)
      end subroutine

      subroutine triSolve_Neumann_CC(x,d,T,b_front,b_back,n)
        implicit none
        real(cp),dimension(n),intent(inout) :: x
        real(cp),dimension(n),intent(in) :: d
        real(cp),intent(in) :: b_front,b_back
        type(triDiag),intent(in) :: T
        integer,intent(in) :: n
        real(cp),dimension(n) :: dtemp
        dtemp = d
        dtemp(2)   = dtemp(2)   - T%L(1)   * b_front
        dtemp(n-1) = dtemp(n-1) - T%U(n-2) * b_back
        call solve_tridiag_CK(x(2:n-1),&
                              dtemp(2:n-1),&
                              T%L(2:n-2),&
                              T%D,&
                              T%U(1:n-3),&
                              n-2)
      end subroutine

      subroutine triSolve_Neumann_node(x,d,T,b_front,b_back,n)
        implicit none
        real(cp),dimension(n),intent(inout) :: x
        real(cp),dimension(n),intent(in) :: d
        real(cp),intent(in) :: b_front,b_back
        type(triDiag),intent(in) :: T
        integer,intent(in) :: n
        real(cp),dimension(n) :: dtemp
        dtemp = d
        dtemp(2)   = dtemp(2)   - T%L(1)   * b_front
        dtemp(n-1) = dtemp(n-1) - T%U(n-2) * b_back
        call solve_tridiag_CK(x(2:n-1),&
                              dtemp(2:n-1),&
                              T%L(2:n-2),&
                              T%D,&
                              T%U(1:n-3),&
                              n-2)
      end subroutine


      ! ********************************************************************
      ! **************************** LOW-LEVEL *****************************
      ! ********************************************************************

      subroutine solve_tridiag_CK(x,d,a,b,c,n)
        ! Returns
        !           _                                          _ -1
        !          |  b(1)  c(1)                                |
        !          |  a(1)  b(2)  c(2)                          |
        !          |        a(2)  b(3)  c(3)                    |
        ! x    =   |           *     *     *                    | d
        !          |                 *     *     *              |
        !          |                    a(n-2)  b(n-1)  c(n-1)  |
        !          |_                           a(n-1)  b(n)   _|
        ! 
        ! Where LU factorization has been used to solve the tridiagonal system:
        ! 
        !  x - result
        !  d - right hand side
        !  a - sub-diagonal
        !  b - main diagonal
        !  c - super-diagonal
        !  n - system size
        ! 
        ! Charlie Kawczynski 8/30/2015
        implicit none
        real(cp),dimension(n),intent(inout) :: x
        real(cp),dimension(n),intent(in) :: d,b
        real(cp),dimension(n-1),intent(in) :: a,c
        integer,intent(in) :: n
        real(cp),dimension(n-1) :: gamma
        real(cp),dimension(n) :: xstar,beta
        real(cp) :: m
        integer i
        ! Define coefficients:
        beta(1) = b(1)
        gamma(1) = c(1)/beta(1)
        do i = 2,n-1
        beta(i) = b(i)-a(i-1)*gamma(i-1)
        gamma(i) = c(i)/beta(i)
        enddo
        beta(n) = b(n)-a(n-1)*gamma(n-1)
        ! Forward substitution:
        xstar(1) = d(1)/beta(1)
        do i = 2,n
        m = d(i) - a(i-1)*xstar(i-1)
        xstar(i) = m/beta(i)
        enddo
        ! Backward substitution:
        x(n) = xstar(n)
        do i = n-1, 1,-1
        x(i) = xstar(i)-gamma(i)*x(i+1)
        end do
      end subroutine

      subroutine solve_tridiag_CK_stored(x,d,a,beta,gamma,n)
        ! Returns
        !           _                                          _ -1
        !          |  b(1)  c(1)                                |
        !          |  a(1)  b(2)  c(2)                          |
        !          |        a(2)  b(3)  c(3)                    |
        ! x    =   |           *     *     *                    | d
        !          |                 *     *     *              |
        !          |                    a(n-2)  b(n-1)  c(n-1)  |
        !          |_                           a(n-1)  b(n)   _|
        ! 
        ! Where LU factorization has been used to solve the tridiagonal system:
        !  _                                          _ 
        ! |  b(1)  c(1)                                |
        ! |  a(1)  b(2)  c(2)                          |
        ! |        a(2)  b(3)  c(3)                    |
        ! |           *     *     *                    |
        ! |                 *     *     *              |
        ! |                    a(n-2)  b(n-1)  c(n-1)  |
        ! |_                           a(n-1)  b(n)   _|
        ! 
        ! =
        !  _                                               _   _                                          _ -1
        ! |  beta(1)                                        | |  1  gamma(1)                               | 
        ! |  alpha(1)  beta(2)                              | |        1  gamma(2)                         | 
        ! |        alpha(2)  beta(3)                        | |              1  gamma(3)                   | 
        ! |           *     *     *                         | |                 *     *                    | 
        ! |                 *     *                         | |                       *     *              | 
        ! |                    alpha(n-2)  beta(n-1)        | |                             1   gamma(n-1) | 
        ! |_                           alpha(n-1)  beta(n) _| |_                                1         _| 
        implicit none
        real(cp),dimension(n),intent(inout) :: x
        real(cp),dimension(n),intent(in) :: d,beta
        real(cp),dimension(n-1),intent(in) :: a,gamma
        integer,intent(in) :: n
        real(cp),dimension(n) :: xstar
        real(cp) :: m
        integer i
        ! Forward substitution:
        xstar(1) = d(1)/beta(1)
        do i = 2,n
        m = d(i) - a(i-1)*xstar(i-1)
        xstar(i) = m/beta(i)
        enddo
        ! Backward substitution:
        x(n) = xstar(n)
        do i = n-1, 1,-1
        x(i) = xstar(i)-gamma(i)*x(i+1)
        end do
      end subroutine

      subroutine init_beta_gamma(beta,gamma,a,b,c,n)
        ! Returns LU in tridiagonal system for
        ! solve_tridiag_CK_stored
        implicit none
        real(cp),dimension(n),intent(inout) :: beta
        real(cp),dimension(n-1),intent(inout) :: gamma
        real(cp),dimension(n-1),intent(in) :: a,c
        real(cp),dimension(n),intent(in) :: b
        integer,intent(in) :: n
        integer i
        ! Define coefficients:
        beta(1) = b(1)
        gamma(1) = c(1)/beta(1)
        do i = 2,n-1
        beta(i) = b(i)-a(i-1)*gamma(i-1)
        gamma(i) = c(i)/beta(i)
        enddo
        beta(n) = b(n)-a(n-1)*gamma(n-1)
      end subroutine

      end module
