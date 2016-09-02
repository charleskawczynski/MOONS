       module init_Sigma_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use domain_mod
       use ops_embedExtract_mod
       use SF_mod
       implicit none

       private
       public :: initSigma

       integer :: preDefined_Sigma = 1 ! sigma* = sigma_wall/sigma_l
       !                                          0 : Uniform
       !                                          1 : Subdomain dependent
       !                                          2 : Cylinder (2D)

       contains

       subroutine initSigma(sigma,D,m,sig_local_over_sig_f)
         implicit none
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D
         type(SF),intent(inout) :: sigma
         real(cp),intent(in) :: sig_local_over_sig_f
         if (preDefined_Sigma.ne.0) then
           call initPredefinedSigma(sigma,D,m,sig_local_over_sig_f)
         else
           call initUserSigma(sigma,D,sig_local_over_sig_f)
         endif
       end subroutine

       subroutine initPredefinedSigma(sigma,D,m,sig_local_over_sig_f)
         implicit none
         type(SF),intent(inout) :: sigma
         type(domain),intent(in) :: D
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: sig_local_over_sig_f
         call assign(sigma,1.0_cp)
         select case (preDefined_Sigma)
         case (0);
         case (1); call initSubdomain(sigma,D,sig_local_over_sig_f)
         case (2); call initCylinder2D(sigma,D,m%g(1),3,sig_local_over_sig_f) ! Only for single domain
         case default
         stop 'Error: preDefined_Sigma not found in initPredefinedSigma in initializeSigma.f90'
         end select
       end subroutine

       subroutine initSubdomain(sigma,D,sig_local_over_sig_f)
         implicit none
         type(SF),intent(inout) :: sigma
         type(domain),intent(in) :: D
         real(cp),intent(in) :: sig_local_over_sig_f
         type(SF) :: sigma_l
         call init_CC(sigma_l,D%m_in)
         call assign(sigma_l,1.0_cp)
         call assign(sigma,sig_local_over_sig_f)
         call embedCC(sigma,sigma_l,D)
         call delete(sigma_l)
       end subroutine

       subroutine initCylinder2D(sigma,D,g,dir,sig_local_over_sig_f)
         implicit none
         type(SF),intent(inout) :: sigma
         type(domain),intent(in) :: D
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         real(cp),intent(in) :: sig_local_over_sig_f
         type(SF) :: sigma_l
         real(cp),dimension(3) :: hc
         integer,dimension(3) :: s
         integer :: i,j,k
         real(cp) :: r0,r,two
         two = 2.0_cp
         r0 = 1.0_cp
         call init_CC(sigma_l,D%m_in)
         call assign(sigma_l,1.0_cp)
         call assign(sigma,sig_local_over_sig_f)
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

       subroutine initUserSigma(sigma,D,sig_local_over_sig_f)
         implicit none
         type(SF),intent(inout) :: sigma
         type(domain),intent(in) :: D
         real(cp),intent(in) :: sig_local_over_sig_f
         type(SF) :: sigma_l
         call init_CC(sigma_l,D%m_in)
         call assign(sigma_l,1.0_cp)
         call assign(sigma,sig_local_over_sig_f)
         call embedCC(sigma,sigma_l,D)
         call delete(sigma_l)
       end subroutine

       end module
