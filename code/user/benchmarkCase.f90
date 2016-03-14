       module benchmarkCase_mod
       use simParams_mod
       implicit none
       private
       public :: benchmarkCase

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif
       real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

       contains

       subroutine benchmarkCase(Re,Ha,Gr,Fr,Pr,Ec,Rem,finite_Rem,&
         dt_eng,dt_mom,dt_ind,NmaxMHD,N_nrg,tol_nrg,N_mom,tol_mom,&
         N_PPE,tol_PPE,N_ind,tol_ind,N_cleanB,tol_cleanB)
         implicit none
         real(cp),intent(inout) :: Re,Ha,Gr,Fr,Pr,Ec,Rem
         logical,intent(inout) :: finite_Rem
         real(cp),intent(inout) :: dt_eng,dt_mom,dt_ind
         real(cp),intent(inout) :: tol_nrg,tol_PPE,tol_mom,tol_ind,tol_cleanB
         integer,intent(inout) :: NmaxMHD,N_nrg,N_mom,N_PPE,N_ind,N_cleanB
         real(cp) :: t
         Re         = 10.0d0
         Ha         = 500.0d0
         Rem        = 100.0d0
         Gr         = 0.0_cp
         Fr         = 0.0d0
         Pr         = 0.71d0
         Ec         = 0.0d0

         finite_Rem = .false.
         dt_eng     = 5.0d-3
         dt_mom     = 3.0d-7
         dt_ind     = 4.0d-8
         ! t          = 80.0
         t          = 1.0_cp
         ! NmaxMHD       = ceiling(t/dt_eng)
         NmaxMHD       = ceiling(t/dt_mom)
         ! NmaxMHD       = ceiling(t/dt_ind)
         ! NmaxMHD       = maxval((/ceiling(t/dt_eng),ceiling(t/dt_mom),ceiling(t/dt_ind)/))
         ! NmaxMHD       = 1500

         N_nrg         = 1000     ! Number of iterations to solve energy    equation (if iterative solver is used)
         N_mom         = 100      ! Number of iterations to solve momentum  equation (if iterative solver is used)
         N_ind         = 10       ! Number of iterations to solve induction equation (if iterative solver is used)
         N_PPE         = 5        ! Number of iterations to solve PPE steps
         N_cleanB      = 5        ! Number of iterations to solve Poisson equation to clean B

         ! Stopping criteria for iterative solvers:
         !         ||Ax - b||
         !         ----------- < tol
         !         ||Ax⁰ - b||
         ! NOTE: tol must be > 10^(-10)
         ! For the PPE, div(u) -> 0 as t-> infinity.
         ! Which means b and r⁰ -> 0 as t-> infinity.
         tol_nrg       = 10.0_cp**(-10.0_cp)
         tol_mom       = 10.0_cp**(-10.0_cp)
         tol_ind       = 10.0_cp**(-6.0_cp)
         tol_PPE       = 10.0_cp**(-10.0_cp)
         tol_cleanB    = 10.0_cp**(-10.0_cp)
         
         call benchmarkCase_vetted(Re,Ha,Gr,Fr,Pr,Ec,Rem,finite_Rem,&
         dt_eng,dt_mom,dt_ind,NmaxMHD,N_nrg,tol_nrg,N_mom,tol_mom,&
         N_PPE,tol_PPE,N_ind,tol_ind,N_cleanB,tol_cleanB)
       end subroutine

       subroutine benchmarkCase_vetted(BMC,Re,Ha,Gr,Fr,Pr,Ec,Rem,finite_Rem,&
         dt_eng,dt_mom,dt_ind,NmaxMHD,N_nrg,tol_nrg,N_mom,tol_mom,&
         N_PPE,tol_PPE,N_ind,tol_ind,N_cleanB,tol_cleanB)
         implicit none
         integer,intent(in) :: BMC
         real(cp),intent(inout) :: Re,Ha,Gr,Fr,Pr,Ec,Rem
         logical,intent(inout) :: finite_Rem
         real(cp),intent(inout) :: dt_eng,dt_mom,dt_ind
         real(cp),intent(inout) :: tol_nrg,tol_PPE,tol_mom,tol_ind,tol_cleanB
         integer,intent(inout) :: NmaxMHD,N_nrg,N_mom,N_PPE,N_ind,N_cleanB
         ! ********** PREPARE BENCHMARK CASE IF DEFINED *****************
         ! FORMAT CONVENTION:
         !     TITLE: AUTHORS, FLOW CONFIGURATION, SOME KEY PARAMETERS, ADDITIONAL INFO
         select case (BMC)
         case (1) ! TITLE: Guj and Stella, LDC, Re=400
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case (2) ! TITLE: Guj and Stella, LDC,Re=1000
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case (3) ! TITLE: Pattison, LDC (low Rem), Re=100, Ha=45
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case (4) ! TITLE: Shercliff, 2D duct flow (low Rem), Re=10, Ha=500
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re         = 10.0d0
           Ha         = 500.0d0
           finite_Rem = .false.
           dt_mom     = 3.0d-7
           dt_ind     = 4.0d-8
           t          = 1.0_cp
           NmaxMHD       = ceiling(t/dt_mom)
           N_ind         = 10       ! Number of iterations to solve induction equation (if iterative solver is used)
           N_PPE         = 5        ! Number of iterations to solve PPE steps
           tol_ind       = 10.0_cp**(-6.0_cp)
           tol_PPE       = 10.0_cp**(-10.0_cp)

         case (5) ! TITLE: Hunt, 2D duct flow (low Rem), Re=10, Ha=500
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case (6) ! TITLE: Shercliff, 3D duct flow (low Rem), Re=10, Ha=500, fringing field
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case (7) ! TITLE: Hunt, 3D duct flow (low Rem), Re=10, Ha=500, fringing field
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case (8) ! TITLE: Bandaru, 2D channel flow (finite Rem), Re=10, Ha=500
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case default; stop 'Error: incorrect BMC in benchmarkCase in benchmarkCase.f90'
           
         end select
       end subroutine

       subroutine benchmarkCase_unvetted(BMC,Re,Ha,Gr,Fr,Pr,Ec,Al,Rem,&
         dt_eng,dt_mom,dt_ind,NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB)
         implicit none
         integer,intent(in) :: BMC
         real(cp),intent(inout) :: Re,Ha,Gr,Fr,Pr,Ec,Al,Rem
         real(cp),intent(inout) :: dt_eng,dt_mom,dt_ind
         integer,intent(inout) :: NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB
         ! ********** PREPARE BENCHMARK CASE IF DEFINED *****************
         ! FORMAT CONVENTION:
         !     TITLE: AUTHORS, FLOW CONFIGURATION, SOME KEY PARAMETERS, ADDITIONAL INFO
         select case (BMC)
         case (1) ! TITLE: Weiss, eddy flow (finite Rem), Re=10, Ha=500, magnetic reconnection
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case (2) ! TITLE: Weiss, band of eddies (finite Rem), Re=10, Ha=500, magnetic reconnection
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case (3) ! TITLE: Parker, 2D rotating cylinder (finite Rem), Re=10, Ha=500, magnetic reconnection
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case (4) ! TITLE: Iwatsu, LDC, Re=1000, Gr=100, Pr=0.71, mixed convection
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 1000.0_cp
           Gr = 100.0_cp
           Pr = 0.71_cp
           dt_eng = 1.0d-4
         case (4) ! TITLE: Iwatsu, LDC, Re=100, Gr=100, Pr=0.71, mixed convection
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case (5) ! TITLE: Iwatsu, LDC, Re=1000, Gr=10^6, Pr=0.71, mixed convection
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 1000.0_cp
           Gr = 10.0_cp**(6.0_cp)
           Pr = 0.71_cp
           dt_eng = 1.0d-4

         case default; stop 'Error: incorrect BMC in benchmarkCase in benchmarkCase.f90'
           
         end select
       end subroutine


       subroutine MOONS_setParams(Re,Ha,Gr,Fr,Pr,Ec,Al,Rem,&
!          dTime,ds,NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB)
!          implicit none
!          real(cp),intent(inout) :: Re,Ha,Gr,Fr,Pr,Ec,Al,Rem
!          real(cp),intent(inout) :: dTime,ds
!          integer,intent(inout) :: NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB
!          ! ***************** USER DEFINED MHD VARIABLES *****************
!          Re = 1000.0d0
!          Ha = 100.0d0
!          Gr = 10.0_cp**6.0_cp
!          Fr = 0.0d0
!          Pr = 0.71d0
!          Ec = 0.0d0
!          Al = 0.0d0
!          Rem = 1.0d0
!          dTime = 1.0d-4
!          ds = 1.0d-4

!          ! NmaxMHD = 100    ! One hundred steps
!          ! NmaxMHD = 5000    ! Five thousand steps
!          ! NmaxMHD = 10000   ! Ten thousand steps
!          ! NmaxMHD = 50000   ! Fifty thousand steps
!          ! NmaxMHD = 100000  ! One hundred thousand steps
!          ! NmaxMHD = 500000  ! Five hundred thousand steps
!          NmaxMHD = 1000000 ! One million steps
!          NmaxPPE    = 5 ! Number of PPE steps
!          NmaxB      = 5 ! Number of Steps for Low Rem approx to solve B
!          NmaxCleanB = 5 ! Number of Steps to clean B

!          ! ********** PREPARE BENCHMARK CASE IF DEFINED *****************
!          select case (benchmarkCase)
!          case (1);   Re = 100d0;   Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
!          case (2);   Re = 100d0;   Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
!          case (3);   Re = 100d0;   Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
!          case (4);   Re = 100d0;   Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = ds

!          case (50);  Re = 1970d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3
!          case (51);  Re = 3200d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3

!          ! case (100); Re = 400d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-2
!          ! case (100); Re = 400d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 2.0d-3 ! For mesh refinement
!          ! case (100); Re = 400d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.67d-2
!          ! case (100); Re = 10000d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 8.0d-4
!          ! case (100); Re = 1000d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 5.0d-3

!          ! case (100); Re = 1d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-6
!          case (100); Re = 400d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-2
!          ! case (100); Re = 4.0d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-3 ! Low Rem for momentum ADI
!          ! case (101); Re = 1000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 2.5d-4
!          ! case (101); Re = 1000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 2.5d-4
!          ! case (101); Re = 400d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 2.5d-4
!          case (101); Re = 400d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 2.5d-4
!          case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 0.01d0 ; ds = 1.0d-4; dTime = 1.0d-2
!          ! case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 0.01d0 ; ds = 1.0d-6; dTime = 1.0d-2 ! Low but finite Rem
!          ! case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 10.0d0 ; ds = 1.0d-6; dTime = 1.0d-2 ! finite Rem
!          ! case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 0.01d0 ; ds = 2.0d-6; dTime = 1.0d-2
!          ! case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 5.0d-3
!          ! case (103); Re = 1000d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 3.0d-4
!          case (103); Re = 1000d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 4.0d-7; dTime = 3.0d-4
!          case (104); Re = 1000d0;   Ha = 1000.0d0 ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.9d-6

!          case (105); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
!          case (106); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-6; dTime = 1.0d-2
!          case (107); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-7; dTime = 3.0d-2
!          case (108); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-7; dTime = 1.0d-2 ! Has not worked yet

!          case (109); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2

!          case (200); Re = 200d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 5.0d-3
!          case (201); Re = 1000d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-4
!          case (202); Re = 1000d0;   Ha = 500.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-5

!          case (250); Re = 15574.07d0;   Ha = 2900.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 5.0d-6
!          ! case (250); Re = 1000.07d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-5

!          case (300); Re = 1000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3
!          case (301); Re = 2000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3

!          ! case (1001); Re = 100d0;   Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 6.0d-6; dTime = 3.0d-4 ! Ha = 10
!          ! case (1001); Re = 100d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 5.0d-7; dTime = 4.0d-5 ! Ha = 100
!          case (1001); Re = 100d0;   Ha = 1000.0d0  ; Rem = 1.0d0 ; ds = 1.0d-8; dTime = 9.0d-7 ! Ha = 1000

!          ! case (1002); Re = 10d0;    Ha = 500.0d0 ; Rem = 1.0d0 ; ds = 1.0d-8; dTime = 5.0d-7
!          case (1002); Re = 10d0;    Ha = 500.0d0 ; Rem = 1.0d0 ; ds = 8.0d-9; dTime = 2.0d-7
!          ! case (1002); Re = 100d0;    Ha = 500.0d0 ; Rem = 1.0d0 ; ds = 1.0d-6; dTime = 1.0d-5
!          case (1003); 
!          ds = 1.0d-5; dTime = ds

!          ! Ha = 10

!          ! Re = 100d0;    Ha = 10.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 500d0;    Ha = 10.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 1000d0;   Ha = 10.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 10000d0;  Ha = 10.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds

!          ! Re = 100d0;    Ha = 10.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 500d0;    Ha = 10.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 1000d0;   Ha = 10.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 10000d0;  Ha = 10.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds

!          ! Re = 100d0;    Ha = 10.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 500d0;    Ha = 10.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 1000d0;   Ha = 10.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 10000d0;  Ha = 10.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds

!          ! Ha = 100

!          ! Re = 100d0;    Ha = 100.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds ! streamlines sideways?
!          ! Re = 500d0;    Ha = 100.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 1000d0;   Ha = 100.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 10000d0;  Ha = 100.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds

!          ! Re = 100d0;    Ha = 100.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 500d0;    Ha = 100.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 1000d0;   Ha = 100.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 10000d0;  Ha = 100.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds

!          ! Re = 100d0;    Ha = 100.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds ! Next
!          Re = 10000.0_cp; Ha = 10000.0_cp; Rem = 100.0_cp; ds = 1.0d-5; dTime = ds ! Next
!          ! Re = 500d0;    Ha = 100.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 1000d0;   Ha = 100.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 10000d0;  Ha = 100.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds

!          ! Ha = 1000

!          ! Re = 100d0;    Ha = 1000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 500d0;    Ha = 1000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 1000d0;   Ha = 1000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 10000d0;  Ha = 1000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds

!          ! Re = 100d0;    Ha = 1000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 500d0;    Ha = 1000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 1000d0;   Ha = 1000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 10000d0;  Ha = 1000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds

!          ! Re = 100d0;    Ha = 1000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 500d0;    Ha = 1000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 1000d0;   Ha = 1000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 10000d0;  Ha = 1000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds

!          ! Ha = 10000

!          ! Re = 100d0;    Ha = 10000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 500d0;    Ha = 10000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 1000d0;   Ha = 10000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 10000d0;  Ha = 10000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds

!          ! Re = 100d0;    Ha = 10000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 500d0;    Ha = 10000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 1000d0;   Ha = 10000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 10000d0;  Ha = 10000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds

!          ! Re = 100d0;    Ha = 10000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 500d0;    Ha = 10000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 1000d0;   Ha = 10000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
!          ! Re = 10000d0;  Ha = 10000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds

!          ! case (1004); Re = 400d0;    Ha = 0.0d0 ; Rem = 1.0d0  ; ds = 1.0d-4; dTime = ds
!          case (1004); Re = 400d0;    Ha = 0.0d0 ; Rem = 1.0d0  ; ds = 1.0d-3; dTime = ds
!          ! Rem = 0.1d0; ds = 1.0d-5
!          Rem = 100.0d0; ds = 1.0d-4
!          ! Rem = 400.1d00; ds = 1.0d-3
!          ! Rem = 1000.0d0; ds = 1.0d-3
!          case (1005); Re = 400d0;    Ha = 10.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
!          ! case (1006); Rem = 50.0d0*real(4.0,cp)*PI ; ds = 1.0d-4; dTime = ds
!          case (1006); Rem = real(1000.0,cp) ; ds = 5.0d-5; dTime = ds

!          case (1007); Rem = real(100.0,cp) ; ds = 3.0d-5; dTime = ds ! Parker

!          case (1008); 
!          Re = real(200.0,cp)
!          Ha = real(25.819888974716115,cp) ! Q  = 0.3 : Q = 1/N = Re/Ha^2 => Ha^2 = Re/Q
!          ! Ha = real(20.0,cp)             ! Q  = 0.5 : Q = 1/N = Re/Ha^2 => Ha^2 = Re/Q
!          ! Ha = real(100.0,cp)             ! Q  = 0.5 : Q = 1/N = Re/Ha^2 => Ha^2 = Re/Q
!          Rem = real(1.0,cp)
!          ! Rem = real(0.001,cp)
!          ds = 2.0d-5; dTime = ds ! Q = 0.3, good dt
!          ! ds = 3.0d-4; dTime = ds

!          case (1009)
!          Re = real(400.0,cp); Ha = real(20.0,cp)

!          ! Rem = real(0.0,cp);    ds = 1.0d-4;   dTime = 1.0d-2 ! (Rem = 0)
!          Rem = real(1.0,cp);    ds = 5.0d-5;   dTime = ds     ! (Rem = 1)
!          ! Rem = real(10.0,cp);   ds = 5.0d-4;   dTime = ds     ! (Rem = 10)
!          ! Rem = real(100.0,cp);  ds = 5.0d-3;   dTime = ds     ! (Rem = 100)

!          case (1010)
!          Re = 400.0_cp; Ha = 20.0_cp

!          ! Rem = real(0.0,cp);    ds = 1.0d-4;   dTime = 1.0d-2 ! (Rem = 0)
!          ! Rem = real(1.0,cp);    ds = 5.0d-5;   dTime = ds     ! (Rem = 1)
!          ! Rem = real(10.0,cp);   ds = 5.0d-4;   dTime = ds     ! (Rem = 10)
!          ! Rem = real(100.0,cp);  ds = 5.0d-5;   dTime = ds     ! (Rem = 100) , sigma* = 0.01

!          ! Rem = 100.0_cp;  ds = 1.0d-7;   dTime = ds     ! (Rem = 100) , sigma* = 0.001, fine grid
!          Rem = 100.0_cp;  ds = 2.0d-6;   dTime = ds     ! (Rem = 100) , sigma* = 0.001, fine grid
!          ! Rem = real(100.0,cp);  ds = 2.0d-3;   dTime = ds     ! (Rem = 100) , sigma* = 0.001, fine grid

!          case (1011)
!          Re = real(10.0,cp)
!          Ha = real(500.0,cp)
!          Rem = real(100.0,cp)
!          ds = 5.0d-8
!          dTime = 1.0d-7

!          case (1012)
!          Re = real(100.0,cp); Ha = real(45.0,cp)
!          ds = 5.0d-5; dTime = 1.0d-3

!          case (1013)
!          Re = 400.0_cp
!          dTime = 10.0_cp**(-3.0_cp)

!          case default
!            stop 'Incorrect benchmarkCase in MOONS'
!          end select

!          select case (benchmarkCase)
!          case (1);   NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4000
!          case (2);   NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4000
!          case (3);   NmaxPPE = 5; NmaxB = 5; NmaxMHD = 8000
!          ! case (4);   NmaxPPE = 5; NmaxB = 5; NmaxMHD = 8000
!          case (4);   NmaxPPE = 5; NmaxB = 5; NmaxMHD = 8000

!          case (50);  NmaxPPE = 5; NmaxB = 0; NmaxMHD = 1000000
!          case (51);  NmaxPPE = 5; NmaxB = 0; NmaxMHD = 1000000
         
!          case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 4000
!          ! case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 80000
!          ! case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 10000
!          ! case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 10**5
!          ! case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 70000 ! For convergence rate test

!          ! case (101); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 3*10**5
!          case (101); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 2*10**5 ! u-bend flow
!          case (102); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4000
!          ! case (102); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 20000
!          case (103); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 500000
!          case (104); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3000000

!          case (105); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 6000
!          case (106); NmaxPPE = 5; NmaxB = 50; NmaxMHD = 6000
!          case (107); NmaxPPE = 5; NmaxB = 50; NmaxMHD = 60000
!          case (108); NmaxPPE = 5; NmaxB = 50; NmaxMHD = 20000

!          case (109); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 60000

!          case (200); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 4*10**5 ! Insul
!          ! case (200); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 7500
!          case (201); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 15000
!          case (202); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 1000000

!          ! case (250); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10
!          case (250); NmaxPPE = 15; NmaxB = 5; NmaxMHD = 10**7 ! Case B2

!          case (300); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 100000
!          case (301); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 100000

!          ! case (1001); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 5*10**5 ! A
!          ! case (1001); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**6 ! B
!          case (1001); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! Shercliff flow
!          case (1002); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! Hunt flow
!          ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**5 ! Mimicking PD
!          ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**1 ! Mimicking PD
!          ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! Mimicking PD
!          ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**6 ! Mimicking PD
!          ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 400000 ! Mimicking PD
!          ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! Mimicking PD
!          case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 1 ! Mimicking PD

!          ! case (1004); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**5 ! Salah
!          ! case (1004); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 2*10**4
!          case (1004); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**6
!          case (1005); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 8000
!          case (1006); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4*5*10**4
!          case (1007); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 12*10**6
!          ! case (1008); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7
!          case (1008); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**6 ! Q = 0.3

!          case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! for testing

!          ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 2*10**3 ! (Rem = 0) B0x
!          ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 2*10**3 ! (Rem = 0) B0y - done
!          ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 2*10**3 ! (Rem = 0) B0z

!          ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3*10**4 ! (Rem = 1) B0x
!          ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3*10**4 ! (Rem = 1) B0y - pending
!          ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3*10**4 ! (Rem = 1) B0z

!          ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3*10**4 ! (Rem = 10) B0x
!          ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3*10**4 ! (Rem = 10) B0y - done
!          ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3*10**4 ! (Rem = 10) B0z

!          ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4*10**4 ! (Rem = 100) B0x
!          ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4*10**4 ! (Rem = 100) B0y - done
!          ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4*10**4 ! (Rem = 100) B0z
!          ! case (1010); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! for testing
!          ! case (1010); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 6*10**6 ! for testing
!          ! case (1010); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 20*10**6 ! for testing
!          case (1010); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 5*10**7 ! for testing
!          ! case (1010); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**4 ! for testing

!          case (1011); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! for testing
!          case (1012); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! for testing

!          case (1013); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! for testing

!          case default
!            stop 'Incorrect benchmarkCase in MOONS'
!          end select
!        end subroutine

!        ! ***************************************************************
!        ! ***************************************************************
!        ! ******************** For Single Simulation ********************
!        ! ***************************************************************
!        ! ***************************************************************

!        subroutine MOONS_Single_Grid(Ni,Nwtop,Nwbot)
!          implicit none
!          integer,dimension(3),intent(inout) :: Ni,Nwtop,Nwbot
!          ! ***************************************************************
!          ! ***************************************************************
!          ! *************************** For BMCs **************************
!          ! ***************************************************************
!          ! ***************************************************************
!          select case (benchmarkCase)
!          case (1);    Ni = 48;            Nwtop = 0;           Nwbot = 0         ! (LDC: Purely Hydrodynamic / Insulating)
!          case (2);    Ni = 40;            Nwtop = 8;           Nwbot = 8         ! (LDC: Conducting)
!          case (3);    Ni = (/64,32,32/);  Nwtop = 0;           Nwbot = 0         ! (Duct: Purely Hydrodynamic / Insulating)
!          case (4);    Ni = (/1,100,100/); Nwtop = (/0,5,5/);   Nwbot = (/0,5,5/) ! (Duct: Conducting)
!          case (50);   Ni = 105;           Nwtop = 0;           Nwbot = 0
!          case (51);   Ni = 105;           Nwtop = 0;           Nwbot = 0
!          case (100);  Ni = (/67,67,27/);  Nwtop = 0;           Nwbot = 0
!          ! case (100);  Ni = (/128,128,1/);  Nwtop = 0;           Nwbot = 0
!          ! case (100);  Ni = (/256,256,1/);  Nwtop = 0;           Nwbot = 0
!          case (101);  Ni = 52;            Nwtop = 0;           Nwbot = 0
!          case (102);  Ni = 45;            Nwtop = 11;          Nwbot = 11
!          case (103);  Ni = 45;            Nwtop = 11;          Nwbot = 11
!          case (104);  Ni = 51;            Nwtop = 5;           Nwbot = 5
!          case (105);  Ni = 45;            Nwtop = (/11,0,11/); Nwbot = 11
!          case (106);  Ni = 45;            Nwtop = (/11,0,11/); Nwbot = 11
!          case (107);  Ni = 45;            Nwtop = (/11,0,11/); Nwbot = 11
!          case (108);  Ni = 45;            Nwtop = (/11,0,11/); Nwbot = 11
!          case (109);  Ni = 45;            Nwtop = (/11,2,11/); Nwbot = (/11,2,11/)
!          case (200);  Ni = (/129,33,33/); Nwtop = 0;           Nwbot = 0
!          case (201);  Ni = (/101,32,32/); Nwtop = (/0,5,5/);   Nwbot = (/0,5,5/)
!          case (202);  Ni = (/181,47,47/); Nwtop = (/0,5,5/);   Nwbot = (/0,5,5/)
!          case (250);  Ni = (/200,51,56/); Nwtop = (/0,5,5/);   Nwbot = (/0,5,5/)
!          case (300);  Ni = 51;            Nwtop = 0;           Nwbot = 0
!          case (301);  Ni = 101;           Nwtop = 0;           Nwbot = 0
!          case (1001); Ni = 52;            Nwtop = (/8,0,8/);   Nwbot = 8 ! Ha = 10,100,1000
!          case (1002); Ni = (/65,45,45/);  Nwtop = 0;           Nwbot = 0     ! Insulating
!          ! case (1003); Ni = (/75,45,45/);  Nwtop = 11;          Nwbot = 11
!          ! case (1003); Ni = (/1,64,64/); Nwtop = (/0,11,11/); Nwbot = Nwtop
!          ! case (1003); Ni = (/1,100,100/); Nwtop = (/0,5,5/); Nwbot = Nwtop
!          ! case (1003); Ni = (/1,160,160/); Nwtop = (/0,8,8/); Nwbot = Nwtop
!          ! case (1003); Ni = (/1,260,260/); Nwtop = (/0,13,13/); Nwbot = Nwtop
!          ! case (1003); Ni = (/64,64,1/); Nwtop = 0; Nwbot = Nwtop
!          case (1003); Ni = (/64,64,1/); Nwtop = 0; Nwbot = Nwtop ! SK flow

!          case (1004); Ni = 35;            Nwtop = 0;           Nwbot = 0
!          case (1005); Ni = (/64,32,32/);  Nwtop = 0;           Nwbot = 0  ! (Jack's Experiment)
!          ! case (1006); Ni = (/64,64,1/);   Nwtop = 32;          Nwbot = 32 ! (Weiss, Isolated Eddy)
!          ! Nwtop(3) = 0;        Nwbot(3) = 0                                ! (Weiss, Isolated Eddy)

!          ! case (1006); Ni = (/200,100,1/);   Nwtop = 0;           Nwbot = 0  ! (Weiss, Single Eddy)
!          case (1006); Ni = (/400,100,1/);   Nwtop = 0;           Nwbot = 0  ! (Weiss, Single Eddy)
!          ! case (1006); Ni = (/100,100,1/);   Nwtop = 0;           Nwbot = 0  ! (Weiss, Single Eddy)

!          case (1007); Ni = (/100,100,1/);   Nwtop = 0;           Nwbot = 0  ! (Parker, Cylinder)

!          ! case (1008); Ni = (/100,1,100/);   Nwtop = 0;           Nwbot = 0  ! (Bandaru)
!          case (1008); Ni = (/64,1,64/);   Nwtop = 0;           Nwbot = 0  ! (Bandaru)
!          case (1009); Ni = 50;            Nwtop = 0;           Nwbot = 0  ! (Kawczynski - demo)
!          ! case (1009); Ni = 100;            Nwtop = 0;           Nwbot = 0  ! (Kawczynski - demo)
!          ! case (1010); Ni = 50;            Nwtop = 25;          Nwbot = 25  ! (Kawczynski - demo) for sigma* = 0.01
!          ! case (1010); Ni = 30;            Nwtop = (/8,0,8/);   Nwbot = 8  ! (Kawczynski - demo) for sigma* = 0.001
!          case (1010); Ni = 30;            Nwtop = (/5,0,5/);   Nwbot = 5  ! (Kawczynski - demo) for sigma* = 0.001, t_wall=0.05
!          case (1011); Ni = (/1,45,45/);   Nwtop = 0;           Nwbot = 0  ! (Kawczynski - demo) for shercliff / hunt flow
!          case (1012); Ni = 64;            Nwtop = 0;           Nwbot = 0  ! Pattison
!          case (1013); Ni = (/128,128,1/);   Nwtop = 0;           Nwbot = 0  ! Pattison

!          case default
!            Ni = (/64,32,32/)
!            Nwtop = 0
!            Nwbot = 0
!          end select
!          write(*,*) 'Ni = ',Ni
!          write(*,*) 'Nwtop = ',Nwtop
!          write(*,*) 'Nwbot = ',Nwbot
!        end subroutine


       end module
