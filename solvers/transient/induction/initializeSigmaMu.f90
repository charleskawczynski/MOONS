       module initializeSigmaMu_mod
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
       !                                       1 : Box in a box
       !                                       2 : Cylinder (2D)

       real(cp) :: sigmaStarWall = real(1.0,cp) ! sigma* = sigma_wall/sigma_l

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
         select case (preDefined_Sigma)
         case (1); call initBoxInBox(sigma,SD,g)
         case (2); call initCylinder2D(sigma,SD,g,3)
         case default
         stop 'Error: preDefined_Sigma not found in initPredefinedSigma in initializeSigmaMu.f90'
         end select
       end subroutine

       subroutine initBoxInBox(sigma,SD,g)
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

       subroutine initCylinder2D(sigma,SD,g,dir)
         implicit none
         type(scalarField),intent(inout) :: sigma
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         type(scalarField) :: sigma_l
         real(cp),dimension(3) :: hc
         integer,dimension(3) :: s
         integer :: i,j,k
         real(cp) :: r0,r,two
         two = real(2.0,cp)
         r0 = real(1.0,cp)

         call allocateField(sigma_l,SD%s)
         call assign(sigma_l,real(1.0,cp))
         call assign(sigma,sigmaStarWall)

         s = sigma%s

         hc = (/((g%c(i)%hmax+g%c(i)%hmin)/real(2.0,cp),i=1,3)/)
         select case (dir)
         case (1)
           do k=1,s(3);do j=1,s(2);do i=1,s(1)
                r = sqrt((g%c(2)%hc(j)-hc(2))**two + (g%c(3)%hc(k)-hc(3))**two)
                if (r.lt.r0) sigma%phi(i,j,k) = real(1.0,cp)
           enddo;enddo;enddo
         case (2)
           do k=1,s(3);do j=1,s(2);do i=1,s(1)
                r = sqrt((g%c(1)%hc(i)-hc(1))**two + (g%c(3)%hc(k)-hc(3))**two)
                if (r.lt.r0) sigma%phi(i,j,k) = real(1.0,cp)
           enddo;enddo;enddo
         case (3)
           do k=1,s(3);do j=1,s(2);do i=1,s(1)
                r = sqrt((g%c(1)%hc(i)-hc(1))**two + (g%c(2)%hc(j)-hc(2))**two)
                if (r.lt.r0) sigma%phi(i,j,k) = real(1.0,cp)
           enddo;enddo;enddo
         case default
         stop 'Error: dir must = 1,2,3 in initCylinder2D in initializeSigmaMu.f90'
         end select
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
