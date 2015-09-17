       module init_Sigma_mod
       use grid_mod
       use ops_embedExtract_mod
       use SF_mod
       implicit none

       private
       public :: initSigma


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
       integer,parameter :: preDefined_Sigma = 2 ! sigma* = sigma_wall/sigma_l
       !                                       0 : User-defined case (no override)
       !                                       1 : Subdomain dependent
       !                                       2 : Index based
       !                                       3 : Cylinder (2D)

       real(cp) :: sigmaStarWall = 0.001_cp ! sigma* = sigma_wall/sigma_l

       contains

       subroutine initSigma(sigma,SD,g)
         implicit none
         type(grid),intent(in) :: g
         type(subdomain),intent(in) :: SD
         type(SF),intent(inout) :: sigma
         if (preDefined_Sigma.ne.0) then
           call initPredefinedSigma(sigma,SD,g)
         else
           call initUserSigma(sigma,SD,g)
         endif
       end subroutine

       subroutine initPredefinedSigma(sigma,SD,g)
         implicit none
         type(SF),intent(inout) :: sigma
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         select case (preDefined_Sigma)
         case (1); call initSubdomain(sigma,SD,g)
         case (2); call initIndexBased(sigma,SD)
         case (3); call initCylinder2D(sigma,SD,g,3)
         case default
         stop 'Error: preDefined_Sigma not found in initPredefinedSigma in initializeSigma.f90'
         end select
       end subroutine

       subroutine initSubdomain(sigma,SD,g)
         implicit none
         type(SF),intent(inout) :: sigma
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         type(SF) :: sigma_l
         call init_CC(sigma_l,SD%g)
         call assign(sigma_l,1.0_cp)
         call assign(sigma,sigmaStarWall)
         call embedCC(sigma,sigma_l,SD,g)
         call delete(sigma_l)
       end subroutine

       subroutine initIndexBased(sigma,SD)
         implicit none
         type(SF),intent(inout) :: sigma
         type(subdomain),intent(in) :: SD
         integer :: pad
         pad = 6+1
         call assign(sigma,sigmaStarWall)
         sigma%RF(1)%f(SD%Nice1(1)-pad:SD%Nice2(1)+pad,&
                       SD%Nice1(2)-pad:SD%Nice2(2)-1,&
                       SD%Nice1(3)-pad:SD%Nice2(3)+pad) = 1.0_cp
       end subroutine

       subroutine initCylinder2D(sigma,SD,g,dir)
         implicit none
         type(SF),intent(inout) :: sigma
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         type(SF) :: sigma_l
         real(cp),dimension(3) :: hc
         integer,dimension(3) :: s
         integer :: i,j,k
         real(cp) :: r0,r,two
         two = 2.0_cp
         r0 = 1.0_cp
         call init_CC(sigma_l,SD%g)
         call assign(sigma_l,1.0_cp)
         call assign(sigma,sigmaStarWall)
         s = sigma%RF(1)%s
         hc = (/((g%c(i)%hmax+g%c(i)%hmin)/2.0_cp,i=1,3)/)
         select case (dir)
         case (1)
           do k=1,s(3);do j=1,s(2);do i=1,s(1)
                r = sqrt((g%c(2)%hc(j)-hc(2))**two + (g%c(3)%hc(k)-hc(3))**two)
                if (r.lt.r0) sigma%RF(1)%f(i,j,k) = 1.0_cp
           enddo;enddo;enddo
         case (2)
           do k=1,s(3);do j=1,s(2);do i=1,s(1)
                r = sqrt((g%c(1)%hc(i)-hc(1))**two + (g%c(3)%hc(k)-hc(3))**two)
                if (r.lt.r0) sigma%RF(1)%f(i,j,k) = 1.0_cp
           enddo;enddo;enddo
         case (3)
           do k=1,s(3);do j=1,s(2);do i=1,s(1)
                r = sqrt((g%c(1)%hc(i)-hc(1))**two + (g%c(2)%hc(j)-hc(2))**two)
                if (r.lt.r0) sigma%RF(1)%f(i,j,k) = 1.0_cp
           enddo;enddo;enddo
         case default
         stop 'Error: dir must = 1,2,3 in initCylinder2D in initializeSigma.f90'
         end select
         call delete(sigma_l)
       end subroutine

       subroutine initUserSigma(sigma,SD,g)
         implicit none
         type(SF),intent(inout) :: sigma
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         type(SF) :: sigma_l
         call init_CC(sigma_l,SD%g)
         call assign(sigma_l,1.0_cp)
         call assign(sigma,sigmaStarWall)
         call embedCC(sigma,sigma_l,SD,g)
         call delete(sigma_l)
       end subroutine

       end module
