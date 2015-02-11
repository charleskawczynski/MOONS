       module initializeSigmaMu_mod
       use constants_mod
       use griddata_mod
       use simParams_mod
       implicit none

       private
       public :: initializeSigmaMu

       contains

       subroutine initializeSigmaMu(sigma,mu,gd)
         implicit none
         type(griddata),intent(in) :: gd
         real(dpn),dimension(:,:,:),intent(inout) :: sigma,mu
         if (benchmarkCase.ne.0) then
           call initializeBenchmarkSigmaMu(sigma,mu)
         elseif (preDefined_SigmaMu.ne.0) then
           call initializePredefinedSigmaMu(sigma,mu,gd)
         else
           call initializeUserSigmaMu(sigma,mu,gd)
         endif
       end subroutine

       subroutine initializeBenchmarkSigmaMu(sigma,mu)
         implicit none
         ! Auxiliary data types
         real(dpn),dimension(:,:,:),intent(inout) :: sigma,mu
         real(dpn) :: sigma_w,sigma_l
        
         sigma = one; mu = one
         sigma_l = one; sigma_w = one
         select case (benchmarkCase)
         ! Hydrodynamic cases
         case (100); sigma_l = 1.0d0; sigma_w = 1.0d0
         case (101); sigma_l = 1.0d0; sigma_w = 1.0d0

         case (102); sigma_l = 1.0d0; sigma_w = 1.0d0
         case (103); sigma_l = 1.0d0; sigma_w = 1.0d0
         case (104); sigma_l = 1.0d0; sigma_w = 1.0d0

         ! Multi-material tests
         case (105); sigma_l = 1.0d0; sigma_w = 1.0d0
         case (106); sigma_l = 1.0d2; sigma_w = 1.0d0
         case (107); sigma_l = 1.0d3; sigma_w = 1.0d0
         case (108); sigma_l = 1.0d6; sigma_w = 1.0d0

         case (109); sigma_l = 1.0d0; sigma_w = 1.0d0

         case (200); sigma_l = 1.0d0; sigma_w = 1.0d0
         case (201); sigma_l = 1.0d0; sigma_w = 1.0d0
         case (202); sigma_l = 1.0d0; sigma_w = 1.0d0
         case default
           write(*,*) 'Incorrect benchmarkCase in initializeBenchmarkSigmaMu'
           stop
         end select

         ! Total domain
         sigma = sigma_w/sigma_l

         ! Interior domain
         select case (BLoc)
         case (dom_cc_tot)
           sigma(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = one
         case (dom_n_tot)
           sigma(Nin1(1):Nin2(1),Nin1(2):Nin2(2),Nin1(3):Nin2(3)) = one
         end select
       end subroutine

       subroutine initializeUserSigmaMu(sigma,mu,gd)
         implicit none
         ! Auxiliary data types
         type(griddata),intent(in) :: gd
         real(dpn),dimension(:,:,:),intent(inout) :: sigma,mu
         integer,dimension(3) :: N,Ni
         real(dpn) :: sigma_w,sigma_l

         call getN(gd,N)
         call getNi(gd,Ni)
        
         sigma = one
         mu = one

         select case (BLoc)
         case (dom_cc_tot)
         sigma(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = 0.9d0
         case (dom_n_tot)
         sigma(Nin1(1):Nin2(1),Nin1(2):Nin2(2),Nin1(3):Nin2(3)) = 0.9d0
         case default
         write(*,*) 'No sigma defined..';stop
         end select

         sigma = one

         if (multiMaterials) then
           sigma_l = 1.0
           sigma_w = 1.0             ! Conducting
           ! sigma_w = 10.0**(-6.0)   ! Insulating
           ! sigma_w = 10.0**(6.0)    ! Perfectly conducting
           ! Outside
           sigma = sigma_w/sigma_l
           ! Interface
           ! sigma(:,Nyw-1:Ny-(Nyw-1)+1,Nzw-1:Nz-(Nzw-1)+1) = 2.0*sigma_w/(sigma_w + sigma_l) ! Harmonic mean
           ! Interior
           ! sigma(:,Nyw:Ny-Nyw+1,Nzw:Nz-Nzw+1) = sigma_l
         endif
       end subroutine

       end module
