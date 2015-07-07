       module myfdgen_mod
       implicit none

       private

       public :: myfdgen

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

       subroutine myfdgen(kl,kr,ml,mr,n,alpha,beta,p,ecoeff)
         implicit none
         !myfdgen Generate finite difference schemes
         !   This routine develops a finite difference scheme based on grid data at
         !   the points i+kl -> i+kr and derivatives at i+ml -> m+mr to approximate
         !   a derivative of order n at point i. If ml and mr are zero, only
         !   an explicit scheme is sought otherwise a Pade scheme is generated.
         !
         !   The coefficients of the expansion are returned as alpha and beta and
         !   the order of accuracy as p. Note that we must
         !   have n <= (kr-kl)+(mr-ml) to generate a valid scheme with non-zero
         !   order of accuracy. The coefficient of the leading error term is
         !   returned as ecoeff.
         !
         !   Specifically, we seek coefficients beta, alpha and p in the formula
         !          mr                    
         !     h**n sum beta(m) u**(n)|_(i+m)
         !         m=ml                
         !                             kr
         !                          = sum alpha(k) u_(i+k) + O(h**(p+n))
         !                            k=kl
         !   MAE 259A
         !   J. D. Eldredge
         !   1/7/2014
         !
         !   Converted to Fortran90
         !   Charlie Kawczynski
         !   5/8/2014

         ! Input
         integer :: kl,kr,ml,mr,n
         ! Output
         real(cp),dimension(kr-kl+1) :: alpha
         real(cp),dimension(mr-ml+1) :: beta
         real(cp) :: ecoeff,sum
         integer :: p

         ! Dummy Variables
         integer :: mg,ng,j,k,m,cnt,bcnt
         real(cp),dimension(mr-ml+kr-kl+1,mr-ml+kr-kl+1) :: A
         real(cp),dimension(mr-ml+kr-kl+1) :: b,soln

         ! Preliminaries
         mg = mr-ml   ! Number of derivative grid points (excluding point i)
         ng = kr-kl+1 ! Number of function grid points
         if (ng .lt. (n+1)) then
            ! This message was suppressed for the adaptive diff function.
            ! write(*,*) 'ERROR: Insufficient number of &
            ! grid points to approximate derivative.'
             beta = 0.0
             alpha = 0.0
             p = 0
             ecoeff = 0
             return
         endif

         ! Construct rows of matrix.
         ! Column mg+1 corresponds to kl and column mg+ng to kr
         A(:,:) = 0.0
         b(:) = 0.0
         do k = kl,kr
             A(1,k-kl+mg+1) = 1.0  ! u_i row
         enddo
         ! Insert the function coefficients
         do j = 2,ng+mg
           ! u**(j-1)_i row
           do k = kl,kr
             A(j,k-kl+mg+1) = real(k,cp)**real(j-1.0,cp)
           enddo
         enddo
         ! Insert the derivative coefficients
         ! If mg > 0, column 1 corresponds to ml and column mg to mr. No column for m = 0.
         do j = n+1,ng+mg
           ! u**(j-1)_i row
           cnt = 0
           do m = ml,mr
            if (m.eq.0) then
              cycle
            endif
            cnt = cnt + 1
            A(j,cnt) = -real(m,cp)**real(j-n-1,cp)*real(factorial(j-1),cp)/real(factorial(j-n-1),cp)
           enddo
         enddo
         b(n+1) = factorial(n) ! Row n+1 corresponds to desired derivative

         ! Solve
         soln(:) = 0.0
         call gaussianElim(A,b,soln)
         cnt = 0 
         bcnt = 0
         beta = (/(0.0,j=1,mg+1)/)
         do j=1,mg+1
          beta(j) = 0.0
         enddo
         do m = ml,mr
           bcnt = bcnt + 1
           if (m .eq. 0) then
             beta(bcnt) = 1
           else
             cnt = cnt + 1
             beta(bcnt) = soln(cnt)
           endif
         enddo
         alpha = soln(mg+1:mr-ml+kr-kl+1)

         !! Determine order of accuracy
         ! Check factors of higher Taylor series terms to find lowest non-vanishing one.
         do j = mg+ng+1,100
           sum = 0.0
           do k = kl,kr
             sum = sum + real(k,cp)**real(j-1,cp)*real(alpha(k-kl+1),cp)
           enddo
           do m = ml,mr
             sum = sum - real(m,cp)**real(j-n-1,cp)*real(factorial(j-1),cp)/ &
             real(factorial(j-n-1),cp)*real(beta(m-ml+1),cp)
           enddo
           if (abs(sum) > 1e-14) then
            p = j-n-1
            ecoeff = -real(sum,cp)/real(factorial(p+n),cp)
            return
           endif
         enddo
       end subroutine
       
       RECURSIVE FUNCTION factorial(n)  RESULT(Fact)

       IMPLICIT NONE
       INTEGER :: Fact
       INTEGER, INTENT(IN) :: n

       IF (n == 0) THEN
       Fact = 1
       ELSE
       Fact = n * Factorial(n-1)
       END IF

       END FUNCTION Factorial

       subroutine gaussianElim(aa,bb,x)
         implicit none
         ! This subroutine was taken from this slide: 
         ! http://tbp.usu.edu/files/Fortran_Review.pdf
         !--------------------------------------------------
         ! Solutions to a system of linear equations A*x=b
         ! Method: Basic Gauss Elimination
         !--------------------------------------------------
         REAL(dpn),DIMENSION(:,:),INTENT(IN)::aa
         REAL(dpn),DIMENSION(SIZE(aa,1)),INTENT(IN)::bb
         REAL(dpn),DIMENSION(SIZE(bb)),INTENT(OUT)::x
         REAL(dpn),DIMENSION(SIZE(aa,1),SIZE(aa,2))::A
         REAL(dpn),DIMENSION(SIZE(bb))::B
         REAL(dpn),DIMENSION(SIZE(aa,2))::row
         REAL(dpn)::tmp, large, small, r
         INTEGER::j,k

         A = aa
         B(:) = bb(:)
         large = 0D0
         DO k=LBOUND(A,1),UBOUND(A,1)
         large = MAX(large,ABS(A(k,k)))
         END DO
         small = EPSILON(1D0)*large*1D1

         DO k=LBOUND(A,1),UBOUND(A,1)-1,1
         IF(ABS(A(k,k))<=small) THEN
         j = k+1
         DO WHILE(ABS(A(j,k))<=small)
         j = j+1
         END DO
         row(:) = A(k,:)
         A(k,:) = A(j,:)
         A(j,:) = row(:)
         tmp = B(k)
         B(k) = B(j)
         B(j) = tmp
         END IF
         B(k) = B(k)/A(k,k)
         A(k,:) = A(k,:)/A(k,k)
         DO j=k+1,UBOUND(A,1),1
         r = A(j,k)/A(k,k)
         A(j,:) = A(j,:)-r*A(k,:)
         B(j) = B(j)-r*B(k)
         END DO
         END DO

         k = UBOUND(A,1)
         B(k) = B(k)/A(k,k)
         A(k,k) = 1D0
         DO k=UBOUND(A,1),LBOUND(A,1)+1,-1
         DO j=k-1,LBOUND(A,1),-1
         B(j) = B(j)-A(j,k)*B(k)
         A(j,k) = 0D0
         END DO
         END DO
         large = MAXVAL(ABS(B))
         small = EPSILON(1D0)*large*1D1
         WHERE(ABS(B)<=small)
         B = 0D0
         END WHERE
         x = B
       end subroutine

       end module


