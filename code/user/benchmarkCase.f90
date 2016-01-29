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
<<<<<<< HEAD
         case (1) ! TITLE: Weiss, single eddy flow (finite Rem), Re=10, Ha=500, magnetic reconnection
=======
         case (1) ! TITLE: Weiss, eddy flow (finite Rem), Re=10, Ha=500, magnetic reconnection
>>>>>>> 97b0bbf1f0665107a0c28c22821097efeb12e8e8
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

<<<<<<< HEAD
         case (4) ! TITLE: Iwatsu, LDC, Re=1000, Gr=100, Pr=0.71, mixed convection
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 1000.0_cp
           Gr = 100.0_cp
           Pr = 0.71_cp
           dt_eng = 1.0d-4
=======
         case (4) ! TITLE: Iwatsu, LDC, Re=100, Gr=100, Pr=0.71, mixed convection
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
           Re = 100.0_cp
>>>>>>> 97b0bbf1f0665107a0c28c22821097efeb12e8e8
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case (5) ! TITLE: Iwatsu, LDC, Re=1000, Gr=10^6, Pr=0.71, mixed convection
           ! PROBLEM DESCRIPTION: 
           ! REFERENCE: 
<<<<<<< HEAD
           Re = 1000.0_cp
           Gr = 10.0_cp**(6.0_cp)
           Pr = 0.71_cp
           dt_eng = 1.0d-4
=======
           Re = 100.0_cp
>>>>>>> 97b0bbf1f0665107a0c28c22821097efeb12e8e8
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case default; stop 'Error: incorrect BMC in benchmarkCase in benchmarkCase.f90'
           
         end select
       end subroutine

       end module
