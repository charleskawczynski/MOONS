       module initializeSigmaMu_mod
       use grid_mod
       use griddata_mod
       use simParams_mod
       implicit none

       private
       public :: initSigmaMu

       public :: Nin1
       public :: Nin2
       public :: Nice1
       public :: Nice2
       public :: Nici1
       public :: Nici2

       ! This gets overridden by benchmarkCase
       integer,parameter :: preDefined_Sigma = 0 ! sigma* = sigma_wall/sigma_l
       !                                       0 : User-defined case (no override)
       !                                       1 : sigma* = 1 (uniform, conducting)
       !                                       2 : sigma* = 10^-2 (insulating, need small dt for B)
       !                                       3 : sigma* = 10^-3 (insulating, need small dt for B)
       !                                       4 : sigma* = 10^-6 (insulating, need small dt for B)
       !                                       5 : sigma* = 10^2 (conducting)
       !                                       6 : sigma* = 10^3 (conducting)
       !                                       7 : sigma* = 10^6 (conducting)

       integer,parameter :: preDefined_SigmaMu = 0
       !                                         0 : User-defined case (no override)
       !                                         1 : sigma = mu = 1

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

       subroutine initSigmaMu(sigma,mu,g)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: sigma,mu
         if (benchmarkCase.ne.0) then
           call initBenchmarkSigmaMu(sigma,mu)
         elseif (preDefined_SigmaMu.ne.0) then
           call initPredefinedSigmaMu(sigma,mu,g)
         else
           call initUserSigmaMu(sigma,mu)
         endif
       end subroutine

       subroutine initBenchmarkSigmaMu(sigma,mu)
         implicit none
         ! Auxiliary data types
         real(cp),dimension(:,:,:),intent(inout) :: sigma,mu
         real(cp) :: sigma_w,sigma_l,cw,tw,sigma_star
         
         sigma = real(1.0,cp); mu = real(1.0,cp)
         sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         select case (benchmarkCase)
         case (1); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (2); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (3); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (4); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)

         ! Hydrodynamic cases
         case (100); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (101); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)

         case (102); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (103); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (104); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)

         ! Multi-material tests
         case (105); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (106); sigma_l = real(1.0d2,cp); sigma_w = real(1.0,cp)
         case (107); sigma_l = real(1.0d3,cp); sigma_w = real(1.0,cp)
         case (108); sigma_l = real(1.0d6,cp); sigma_w = real(1.0,cp)

         case (109); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)

         case (200); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (201); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (202); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (250); 
         ! cw = sigma_star*tw/L_parallel
         ! sigma_star = cw/tw for L = 1
         cw = real(0.07,cp)
         tw = real(0.142394,cp)
         sigma_star = cw/tw
         sigma_l = real(1.0,cp); sigma_w = sigma_star

         case (1001); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (1002); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (1003); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (1004); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case (1005); sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         case default
           write(*,*) 'Incorrect benchmarkCase in initBenchmarkSigmaMu'
           stop
         end select

         ! Total domain
         sigma = sigma_w/sigma_l

         ! Interior domain

         ! sigma(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = real(1.0,cp)
         
         ! For duct flow:
         sigma(Nici1(1):Nici2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = real(1.0,cp)

         ! Make lid have fluid conductivity (what HIMAG does)
         ! sigma(Nice1(1):Nice2(1),Nice1(2):Nici2(2),Nice1(3):Nice2(3)) = real(1.0,cp)

       end subroutine

       subroutine initUserSigmaMu(sigma,mu)
         implicit none
         ! Auxiliary data types
         real(cp),dimension(:,:,:),intent(inout) :: sigma,mu
         real(cp) :: sigma_w,sigma_l

         sigma = real(1.0,cp)
         mu = real(1.0,cp)
         sigma_l = real(1.0,cp); sigma_w = real(1.0,cp)
         sigma = sigma_w/sigma_l

         sigma(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = real(1.0,cp)

         sigma = real(1.0,cp)

       end subroutine

       end module
