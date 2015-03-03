      module myTriSolver_mod
      ! Solves a tridiagonal system of equations.
      ! Implementation:
      ! 
      ! type(myTriSolver) :: T
      ! real(cp),dimension(:,:,:) :: uOut,uIn
      ! integer :: dir,pad
      ! call apply(T,uOut,uIn,dir,pad) 
      ! 
      ! See applyTriSolver for more details

      implicit none
      
      private
      public :: myTrisolver
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

      type myTrisolver
        private
        real(cp),dimension(:),allocatable :: loDiag,diag,upDiag
        integer :: s
      end type

      interface init;    module procedure initTriSolver;    end interface
      interface delete;  module procedure deleteTriSolver;  end interface
      interface apply;   module procedure applyTriSolver;   end interface

      contains

      subroutine initTriSolver(T,loDiag,diag,upDiag)
        implicit none
        type(myTrisolver),intent(inout) :: T
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
        real(cp),dimension(:,:,:),intent(inout) :: uOut
        real(cp),dimension(:,:,:),intent(in) :: uIn
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
        real(cp),dimension(:),intent(inout) :: u,c,f
        real(cp),dimension(:),intent(in) :: a,b
        real(cp) :: temp,ctemp, ftemp, atemp
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

      subroutine triSolveEldredgeShouldWork(u,f,a,b,c,M)
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

      subroutine triSolveWPivoting(u,f,loDiag,diag,upDiag,s)
        implicit none
        integer,intent(in) :: s
        real(cp),dimension(:),intent(inout) :: u
        real(cp),dimension(:),intent(in) :: f,diag
        real(cp),dimension(:),intent(in) :: loDiag,upDiag
        real(cp),dimension(:),allocatable :: loDiagTemp,diagTemp,upDiagTemp,fTemp
        integer :: info
        allocate(loDiagTemp(s-1))
        allocate(diagTemp(s))
        allocate(upDiagTemp(s-1))
        allocate(fTemp(s))
        loDiagTemp = loDiag; diagTemp = diag; upDiagTemp = upDiag; fTemp = f
        call dgtsv(s,1,loDiagTemp,diagTemp,upDiagTemp,fTemp,s,info)
        u = fTemp
        deallocate(loDiagTemp,diagTemp,upDiagTemp,fTemp)
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

      subroutine dgtsv( n, nrhs, dl, d, du, b, ldb, info )
        implicit none
        ! 
        !   -- lapack routine (version 3.3.1) --
        !   -- lapack is a software package provided by univ. of tennessee,    --
        !   -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
        !   -- april 2011                                                      --
        ! 
        !      .. scalar arguments ..
        integer :: info, ldb, n, nrhs
        !    ..
        !    .. array arguments ..
        ! real(cp) :: b( ldb, * ), d( * ), dl( * ), du( * )

        real(cp),dimension(ldb,*),intent(inout) :: b
        real(cp),dimension(:),intent(inout) :: d,dl,du
        !      ..
        ! 
        !   purpose
        !   =======
        ! 
        !   dgtsv  solves the equation
        ! 
        !      a*x = b,
        ! 
        !   where a is an n by n TRIDIAGONAL MATRIX, BY GAUSSIAN ELIMINATION WITH
        !   PARTIAL PIVOTING.
        ! 
        !   note that the equation  a**t*x = b  may be solved by interchanging the
        !   order of the arguments du and dl.
        ! 
        !   arguments
        !   =========
        ! 
        !   n       (input) integer
        !           the order of the matrix a.  n >= 0.
        ! 
        !   nrhs    (input) integer
        !           the number of right hand sides, i.e., the number of columns
        !           of the matrix b.  nrhs >= 0.
        ! 
        !   dl      (input/output) double precision array, dimension (n-1)
        !           on entry, dl must contain the (n-1) sub-diagonal elements of
        !           a.
        ! 
        !           on exit, dl is overwritten by the (n-2) elements of the
        !           second super-diagonal of the upper triangular matrix u from
        !           the lu factorization of a, in dl(1), ..., dl(n-2).
        ! 
        !   d       (input/output) double precision array, dimension (n)
        !           on entry, d must contain the diagonal elements of a.
        ! 
        !           on exit, d is overwritten by the n diagonal elements of u.
        ! 
        !   du      (input/output) double precision array, dimension (n-1)
        !           on entry, du must contain the (n-1) super-diagonal elements
        !           of a.
        ! 
        !           on exit, du is overwritten by the (n-1) elements of the first
        !           super-diagonal of u.
        ! 
        !   b       (input/output) double precision array, dimension (ldb,nrhs)
        !           on entry, the n by nrhs matrix of right hand side matrix b.
        !           on exit, if info = 0, the n by nrhs solution matrix x.
        ! 
        !   ldb     (input) integer
        !           the leading dimension of the array b.  ldb >= max(1,n).
        ! 
        !   info    (output) integer
        !           = 0: successful exit
        !           < 0: if info = -i, the i-th argument had an illegal value
        !           > 0: if info = i, u(i,i) is exactly zero, and the solution
        !                has not been computed.  the factorization has not been
        !                completed unless i = n.
        ! 
        !   =====================================================================
        ! 
        ! Revised by Charlie Kawczynski 3/2/2015
        ! University of California Los Angeles
        ! 
        real(cp),parameter :: zero = 0.0
        real(cp),parameter :: tol = 1.0d-3
        integer :: i, j
        real(cp) ::   fact, temp

        info = 0
        if( n.lt.0 ) then
           info = -1
        else if( nrhs.lt.0 ) then
           info = -2
        else if( ldb.lt.max( 1, n ) ) then
           info = -7
        end if
        if( info.ne.0 ) then
          write(*,*) 'Error: dgtsv undefined error.'
          write(*,*) 'info = ',info
          stop
        end if

        if( n.eq.0 ) return

        if( nrhs.eq.1 ) then
           do 10 i = 1, n - 2
              if( abs( d( i ) ).ge.abs( dl( i ) ) ) then
        ! no row interchange required
                 if( abs(d( i )).gt.tol ) then
                    fact = dl( i ) / d( i )
                    d( i+1 ) = d( i+1 ) - fact*du( i )
                    b( i+1, 1 ) = b( i+1, 1 ) - fact*b( i, 1 )
                 else
                    info = i
                    return
                 end if
                 dl( i ) = zero
              else

        !    interchange rows i and i+1

                 fact = d( i ) / dl( i )
                 d( i ) = dl( i )
                 temp = d( i+1 )
                 d( i+1 ) = du( i ) - fact*temp
                 dl( i ) = du( i+1 )
                 du( i+1 ) = -fact*dl( i )
                 du( i ) = temp
                 temp = b( i, 1 )
                 b( i, 1 ) = b( i+1, 1 )
                 b( i+1, 1 ) = temp - fact*b( i+1, 1 )
                 write(*,*) 'Rows interchanged!'
              end if
     10    continue
           if( n.gt.1 ) then
              i = n - 1
              if( abs( d( i ) ).ge.abs( dl( i ) ) ) then
                 if( abs(d( i )).gt.tol ) then
                    fact = dl( i ) / d( i )
                    d( i+1 ) = d( i+1 ) - fact*du( i )
                    b( i+1, 1 ) = b( i+1, 1 ) - fact*b( i, 1 )
                 else
                    info = i
                    return
                 end if
              else
                 fact = d( i ) / dl( i )
                 d( i ) = dl( i )
                 temp = d( i+1 )
                 d( i+1 ) = du( i ) - fact*temp
                 du( i ) = temp
                 temp = b( i, 1 )
                 b( i, 1 ) = b( i+1, 1 )
                 b( i+1, 1 ) = temp - fact*b( i+1, 1 )
              end if
           end if
           if( abs(d( n )).lt.tol ) then
              info = n
              return
           end if
        else
           do 40 i = 1, n - 2
              if( abs( d( i ) ).ge.abs( dl( i ) ) ) then

        !     no row interchange required

                 if( abs(d( i )).gt.tol ) then
                    fact = dl( i ) / d( i )
                    d( i+1 ) = d( i+1 ) - fact*du( i )
                    do 20 j = 1, nrhs
                       b( i+1, j ) = b( i+1, j ) - fact*b( i, j )
     20             continue
                 else
                    info = i
                    return
                 end if
                 dl( i ) = zero
              else

        !     interchange rows i and i+1

                 fact = d( i ) / dl( i )
                 d( i ) = dl( i )
                 temp = d( i+1 )
                 d( i+1 ) = du( i ) - fact*temp
                 dl( i ) = du( i+1 )
                 du( i+1 ) = -fact*dl( i )
                 du( i ) = temp
                 do 30 j = 1, nrhs
                    temp = b( i, j )
                    b( i, j ) = b( i+1, j )
                    b( i+1, j ) = temp - fact*b( i+1, j )
     30          continue
              end if
     40    continue
           if( n.gt.1 ) then
              i = n - 1
              if( abs( d( i ) ).ge.abs( dl( i ) ) ) then
                 if( abs(d( i )).gt.tol) then
                    fact = dl( i ) / d( i )
                    d( i+1 ) = d( i+1 ) - fact*du( i )
                    do 50 j = 1, nrhs
                       b( i+1, j ) = b( i+1, j ) - fact*b( i, j )
     50             continue
                 else
                    info = i
                    return
                 end if
              else
                 fact = d( i ) / dl( i )
                 d( i ) = dl( i )
                 temp = d( i+1 )
                 d( i+1 ) = du( i ) - fact*temp
                 du( i ) = temp
                 do 60 j = 1, nrhs
                    temp = b( i, j )
                    b( i, j ) = b( i+1, j )
                    b( i+1, j ) = temp - fact*b( i+1, j )
     60          continue
              end if
           end if
           if( abs(d( n )).lt.tol ) then
              info = n
              return
           end if
        end if

        !   back solve with the matrix u from the factorization.

        if( nrhs.le.2 ) then
           j = 1
     70    continue
           b( n, j ) = b( n, j ) / d( n )
           if( n.gt.1 ) b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) / d( n-1 )
           do 80 i = n - 2, 1, -1
              b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-dl( i )*b( i+2, j ) ) / d( i )
     80    continue
           if( j.lt.nrhs ) then
              j = j + 1
              go to 70
           end if
        else
           do 100 j = 1, nrhs
              b( n, j ) = b( n, j ) / d( n )
              if( n.gt.1 ) b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) / d( n-1 )
              do 90 i = n - 2, 1, -1
                 b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-dl( i )* b( i+2, j ) ) / d( i )
     90       continue
    100    continue
        end if
      end subroutine

      end module
