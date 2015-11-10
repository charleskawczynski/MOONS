       module grid_stretchParamMatch_mod
       implicit none

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       real(cp),parameter :: one = 1.0_cp
       real(cp),parameter :: two = 2.0_cp
       real(cp),parameter :: zero = 0.0_cp
       
       public :: betaRobertsLeft
       public :: betaRobertsRight
       public :: betaRobertsBoth
       ! public :: betaCluster ! Not yet implemented

       contains

       ! ***************************************************************
       ! ********************** TRANSFORMATION 1 ***********************
       ! ***************************************************************

       ! ***************************************************************
       ! ********************** TRANSFORMATION 2 ***********************
       ! ***************************************************************

       function T2_root(beta,hmin,hmax,alpha,N,dh) result(f)
         ! Returns the root of
         ! 
         !       h(sn) - h(sn-1) = dh
         ! 
         ! to match grid stretching for Robert's transformation 2
         real(cp),intent(in) :: beta,hmin,hmax,alpha,dh
         integer,intent(in) :: N
         real(cp) :: f,K1,K2,g1,g2
         g1 = ((beta+one)/(beta-one))**(((one   )-alpha)/(one-alpha))
         g2 = ((beta+one)/(beta-one))**(((one-one/real(N,cp))-alpha)/(one-alpha))
         K1 = ((beta+two*alpha)*g1-beta+two*alpha)/((two*alpha+one)*(one+g1))
         K2 = ((beta+two*alpha)*g2-beta+two*alpha)/((two*alpha+one)*(one+g2))
         f = K1 - K2 - dh/(hmax-hmin)
       end function

       function T2_prime(beta,alpha,N) result(f)
         ! Returns the root of
         ! 
         !    d/d(beta) [h(sn) - h(sn-1) = dh]
         ! 
         ! to match grid stretching for Robert's transformation 2.
         ! This expression was evaluated in wolfram alpha.
         real(cp),intent(in) :: beta,alpha
         integer,intent(in) :: N
         real(cp) :: f
         real(cp) :: a,b,g,x,y,z,r,t
         a = alpha; b = beta; g = (b+one)/(b-one)
         x = two*a + b; y = one - a - one/real(N,cp); z = one - a
         t = b-one; r = g**(y/z)

         f = ((-one + g + x/t - ((one + b)*x)/t**two)/(one + g) - &
         ((t**(-one) - g**two)*(two*a - b + ((one + b)*x)/t))/(one + g)**two - &
         (-one + r + (g**(-one + y/z)*x*(t**(-one) - g**two)*y)/z)/(one + r) + &
         (g**(-one + y/z)*(t**(-one) - g**two)*(two*a - b + r*x)*y)/(z*(one + r)**two)) / &
         (two*a+one)
       end function

       subroutine newtonT2(T2_root,T2_prime,beta,hmin,hmax,alpha,N,dh)
         ! Estimate the zero of T2_root(beta) using Newton's method. 
         ! Input:
         !   T2_root:  the function to find a root of
         !   T2_prime: function returning the derivative T2_root'
         !   debug: logical, prints iterations if debug=.true.
         ! Returns:
         !   the estimate beta satisfying T2_root(beta)=0 (assumes Newton converged!) 
         !   the number of iterations iters
         implicit none
         real(cp),intent(inout) :: beta
         real(cp),external :: T2_root, T2_prime
         real(cp),intent(in) :: hmin,hmax,alpha,dh
         integer,intent(in) :: N

         real(cp) :: dbeta, fbeta, fbetaprime ! local variables
         integer,parameter :: maxiter = 1000000
         real(cp),parameter :: tol = 10.0_cp**(-14.0_cp) ! Good tolerance
         real(cp),parameter :: err = 10.0_cp**(-10.0_cp) ! We may have a problem
         integer :: k
         logical :: debug
         debug = .false.

         if (debug) write(*,*) 'Initial guess: beta = ',beta

         do k=1,maxiter ! Newton iteration to find a zero of T2_root(beta)
           fbeta = T2_root(beta,hmin,hmax,alpha,N,dh) ! evaluate function and its derivative:
           if (debug) write(*,*) 'fbeta = ',fbeta
           fbetaprime = T2_prime(beta,alpha,N)
           if (debug) write(*,*) 'fbetaprime = ',fbetaprime
           if (abs(fbeta) < tol) exit  ! jump out of do loop
           dbeta = fbeta/fbetaprime ! compute Newton increment beta:
           beta = beta - dbeta ! update beta:
           if (debug) write(*,*) '(Iterations,beta) = ',k,beta
         enddo

         if (k > maxiter) then ! might not have converged
           fbeta = T2_root(beta,hmin,hmax,alpha,N,dh)
           if (abs(fbeta) > err) then
            write(*,*) '*** Warning: has not yet converged'
            write(*,*) '(Iterations,beta) = ',k,beta
            write(*,*) 'tol = ',tol
            write(*,*) 'fbeta = ',fbeta
            ! stop 'Error: matchGridStretching has failed in matchGridStretching.f90'
           endif
         endif
         if (debug) write(*,*) 'max(iter) = ',k-1 ! number of iterations taken
       end subroutine

       ! ***************************************************************
       ! ********************** TRANSFORMATION 3 ***********************
       ! ***************************************************************

       ! ***************************************************************
       ! ******************** SOLUTIONS OF ROOTS ***********************
       ! ***************************************************************

       function betaRobertsRight(hmin,hmax,N,dh) result(beta)
         implicit none
         real(cp),intent(in) :: hmin,hmax,dh
         integer,intent(in) :: N
         real(cp) :: beta
         beta = 1.5_cp ! Initial guess
         call newtonT2(T2_root,T2_prime,beta,hmin,hmax,0.0_cp,N,dh)
       end function

       function betaRobertsLeft(hmin,hmax,N,dh) result(beta)
         ! I believe transformations 1 and 2 have symmetries that
         ! can be exploited to compute the stretching factors by
         ! only using one of the transformations. This was the approach
         ! used here.
         implicit none
         real(cp),intent(in) :: hmin,hmax,dh
         integer,intent(in) :: N
         real(cp) :: beta
         beta = 1.5_cp ! Initial guess
         call newtonT2(T2_root,T2_prime,beta,hmin,hmax,0.0_cp,N,dh)
       end function

       function betaRobertsBoth(hmin,hmax,N,dh) result(beta)
         implicit none
         real(cp),intent(in) :: hmin,hmax,dh
         integer,intent(in) :: N
         real(cp) :: beta
         beta = 1.5_cp
         call newtonT2(T2_root,T2_prime,beta,hmin,hmax,0.5_cp,N,dh)
       end function

       end module