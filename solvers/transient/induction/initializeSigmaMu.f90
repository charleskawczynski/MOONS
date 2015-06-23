       module initializeSigmaMu_mod
       ! use simParams_mod
       use grid_mod
       use ops_embedExtract_mod
       use scalarField_mod
       implicit none

       private
       public :: initSigmaMu


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif


       ! This gets overridden by benchmarkCase
       integer,parameter :: preDefined_Sigma = 1 ! sigma* = sigma_wall/sigma_l
       !                                       0 : User-defined case (no override)
       !                                       1 : sigma* = sigmaStar
       real(cp) :: sigmaStarWall = real(1000.0,cp) ! sigma* = sigma_wall/sigma_l

       contains

       subroutine initSigmaMu(sigma,mu,SD,g)
         implicit none
         type(grid),intent(in) :: g
         type(subdomain),intent(in) :: SD
         type(scalarField),intent(inout) :: sigma,mu
         call initSigma(sigma,SD,g)
         call initMu(mu)
       end subroutine

       ! *************************************************************
       ! *************************************************************
       ! *************************** SIGMA ***************************
       ! *************************************************************
       ! *************************************************************

       subroutine initSigma(sigma,SD,g)
         implicit none
         type(grid),intent(in) :: g
         type(subdomain),intent(in) :: SD
         type(scalarField),intent(inout) :: sigma
         if (preDefined_Sigma.ne.0) then
           call initPredefinedSigma(sigma,SD,g)
         else
           call initUserSigma(sigma,SD,g)
         endif
       end subroutine

       subroutine initPredefinedSigma(sigma,SD,g)
         implicit none
         type(scalarField),intent(inout) :: sigma
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         type(scalarField) :: sigma_l
         call allocateField(sigma_l,SD%s)
         call assign(sigma_l,real(1.0,cp))
         call assign(sigma,sigmaStarWall)
         call embedCC(sigma,sigma_l,SD,g)
         call delete(sigma_l)
       end subroutine

       subroutine initUserSigma(sigma,SD,g)
         implicit none
         type(scalarField),intent(inout) :: sigma
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         type(scalarField) :: sigma_l
         call allocateField(sigma_l,SD%s)
         call assign(sigma_l,real(1.0,cp))
         call assign(sigma,sigmaStarWall)
         call embedCC(sigma,sigma_l,SD,g)
         call delete(sigma_l)
       end subroutine


       ! *************************************************************
       ! *************************************************************
       ! ***************************** MU ****************************
       ! *************************************************************
       ! *************************************************************

       subroutine initMu(mu)
         implicit none
         type(scalarField),intent(inout) :: mu
         call assign(mu,real(1.0,cp))
       end subroutine


       end module
