       module coordinate_stretch_param_match_mod
       use is_nan_mod
       use current_precision_mod
       implicit none

       private

       real(cp),parameter :: one = 1.0_cp
       real(cp),parameter :: two = 2.0_cp
       real(cp),parameter :: four = 4.0_cp
       real(cp),parameter :: zero = 0.0_cp
       
       abstract interface
         function func(beta,hmin,hmax,alpha,N,dh) result(f)
           import cp
           real(cp),intent(in) :: beta,hmin,hmax,alpha,dh
           integer,intent(in) :: N
           real(cp) :: f
         end function
       end interface

       abstract interface
         function func_prime(beta,alpha,N) result(f)
           import cp
           real(cp),intent(in) :: beta,alpha
           integer,intent(in) :: N
           real(cp) :: f
         end function
       end interface

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

       ! NOTE: For more info, see documentation for equation.
       !       Also see python stretch_parameter_matching

       function T2_h_i(beta,alpha,i,N) result(f)
         real(cp),intent(in) :: beta,alpha
         integer,intent(in) :: i,N
         real(cp) :: f,t,a,b,g
         a = alpha; b = beta
         g = (b+one)/(b-one)
         t = (real(i,cp)/real(N,cp) - a)/(one-a) ! theta
         f = ((b+two*a)*g**t - b + two*a)/((two*a+one)*(one+g**t))
       end function

       function T2_prime_h_i(beta,alpha,i,N) result(f)
         real(cp),intent(in) :: beta,alpha
         integer,intent(in) :: i,N
         real(cp) :: f,t,a,b,g
         a = alpha; b = beta
         g = (b+one)/(b-one)
         t = (real(i,cp)/real(N,cp) - a)/(one-a) ! theta
         f = -(-b**two*g**(two*t) + b**two + four*b*g**t*t + &
         g**(two*t) - one)/((two*a + one)*(b - one)*(b + one)*(g**t + one)**two)
       end function

       ! ---------------------------------------------------------------

       function T2_root_dh_near_hmax(beta,hmin,hmax,alpha,N,dh) result(f)
         real(cp),intent(in) :: beta,hmin,hmax,alpha,dh
         integer,intent(in) :: N
         real(cp) :: f
         f = T2_h_i(beta,alpha,N,N) - T2_h_i(beta,alpha,N-1,N) - dh/(hmax-hmin)
       end function

       function T2_prime_dh_near_hmax(beta,alpha,N) result(f)
         real(cp),intent(in) :: beta,alpha
         integer,intent(in) :: N
         real(cp) :: f
         f = T2_prime_h_i(beta,alpha,N,N) - T2_prime_h_i(beta,alpha,N-1,N)
       end function

       function T2_root_dh_near_hmin(beta,hmin,hmax,alpha,N,dh) result(f)
         real(cp),intent(in) :: beta,hmin,hmax,alpha,dh
         integer,intent(in) :: N
         real(cp) :: f
         f = T2_h_i(beta,alpha,1,N) - T2_h_i(beta,alpha,0,N) - dh/(hmax-hmin)
       end function

       function T2_prime_dh_near_hmin(beta,alpha,N) result(f)
         real(cp),intent(in) :: beta,alpha
         integer,intent(in) :: N
         real(cp) :: f
         f = T2_prime_h_i(beta,alpha,1,N) - T2_prime_h_i(beta,alpha,0,N)
       end function

       ! ---------------------------------------------------------------

       subroutine newtonT2(T_root,T_prime,beta,hmin,hmax,alpha,N,dh)
         ! Estimate the zero of T_root(beta) using Newton's method. 
         ! Input:
         !   T_root:  the function to find a root of
         !   T_prime: function returning the derivative T_root'
         !   debug: logical, prints iterations if debug=.true.
         ! Returns:
         !   the estimate beta satisfying T_root(beta)=0 (assumes Newton converged!) 
         !   the number of iterations iters
         implicit none
         real(cp),intent(inout) :: beta
         procedure(func) :: T_root
         procedure(func_prime) :: T_prime
         real(cp),intent(in) :: hmin,hmax,alpha,dh
         integer,intent(in) :: N

         real(cp) :: dbeta, fbeta, fbetaprime ! local variables
         integer,parameter :: maxiter = 1000000
         real(cp),parameter :: tol = 10.0_cp**(-14.0_cp) ! Good tolerance
         real(cp),parameter :: err = 10.0_cp**(-10.0_cp) ! We may have a problem
         integer :: k
         logical :: debug
         debug = .false.

         if (debug) write(*,*) '****************************************'
         if (debug) write(*,*) '****************************************'
         if (debug) write(*,*) '****************************************'
         if (debug) write(*,*) 'Initial guess: beta = ',beta
         if (debug) write(*,*) 'alpha,N = ',alpha,N
         if (debug) write(*,*) 'dh = ',dh
         if (debug) write(*,*) 'hmin,hmax = ',hmin,hmax
         if (debug) write(*,*) '****************************************'
         if (debug) write(*,*) '****************************************'

         do k=1,maxiter ! Newton iteration to find a zero of T_root(beta)
           if (debug) write(*,*) '****************************************'
           if (debug) write(*,*) 'iteration = ',k
           fbeta = T_root(beta,hmin,hmax,alpha,N,dh) ! evaluate function and its derivative:
           if (debug) write(*,*) 'f = ',fbeta
           fbetaprime = T_prime(beta,alpha,N)
           if (debug) write(*,*) 'f_prime = ',fbetaprime
           if (abs(fbeta) .lt. tol) exit  ! jump out of do loop
           dbeta = fbeta/fbetaprime ! compute Newton increment beta:
           if (debug) write(*,*) 'dbeta = ',dbeta
           beta = beta - dbeta ! update beta:
           if (debug) write(*,*) 'updated beta = ',beta
           if (is_nan(beta)) stop 'Done in newtonT2 due to Nans'
         enddo

         if (k.gt.maxiter) then ! might not have converged
           fbeta = T_root(beta,hmin,hmax,alpha,N,dh)
           if (abs(fbeta).gt.err) then
            write(*,*) '*** Warning: has not yet converged'
            write(*,*) '(Iterations,beta) = ',k,beta
            write(*,*) 'tol = ',tol
            write(*,*) 'fbeta = ',fbeta
            ! stop 'Error: matchGridStretching has failed in matchGridStretching.f90'
           endif
         endif
         if (debug) write(*,*) 'max(iter) = ',k-1 ! number of iterations taken
         if (debug) then; stop 'Done in newtonT2'; endif
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
         real(cp) :: beta,tol
         tol = 10.0_cp**(-10.0_cp)
         beta = one + tol ! Initial guess
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
         real(cp) :: beta,tol
         tol = 10.0_cp**(-10.0_cp)
         beta = one + tol ! Initial guess
         call newtonT2(T2_root,T2_prime,beta,hmin,hmax,0.0_cp,N,dh)
       end function

       function betaRobertsBoth(hmin,hmax,N,dh) result(beta)
         implicit none
         real(cp),intent(in) :: hmin,hmax,dh
         integer,intent(in) :: N
         real(cp) :: beta,tol
         tol = 10.0_cp**(-10.0_cp)
         beta = one + tol ! Initial guess
         call newtonT2(T2_root,T2_prime,beta,hmin,hmax,0.5_cp,N,dh)
       end function

       end module