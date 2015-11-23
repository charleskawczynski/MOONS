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

       subroutine benchmarkCase(BMC,Re,Ha,Gr,Fr,Pr,Ec,Al,Rem,&
         dt_eng,dt_mom,dt_ind,NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB)
         implicit none
         integer,intent(in) :: BMC
         real(cp),intent(inout) :: Re,Ha,Gr,Fr,Pr,Ec,Al,Rem
         real(cp),intent(inout) :: dt_eng,dt_mom,dt_ind
         integer,intent(inout) :: NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB
         ! ********************** DEFAULT VALUES **********************
         Re = 100.0_cp
         Ha = 100.0_cp
         Gr = 100.0_cp
         Fr = 100.0_cp
         Pr = 1.0_cp
         Ec = 1.0_cp
         Al = 1.0_cp
         Rem = 1.0_cp
         dt_mom = 1.0d-4
         dt_ind = 1.0d-4
         dt_eng = 1.0d-4
         NmaxMHD = 1000000 ! (One million steps)
         NmaxPPE    = 5 ! Number of PPE steps
         NmaxB      = 5 ! Number of Steps for Low Rem approx to solve B
         NmaxCleanB = 5 ! Number of Steps to clean B
         call benchmarkCase_vetted(BMC,Re,Ha,Gr,Fr,Pr,Ec,Al,Rem,&
         dt_eng,dt_mom,dt_ind,NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB)
         ! call benchmarkCase_unvetted(BMC,Re,Ha,Gr,Fr,Pr,Ec,Al,Rem,&
         ! dt_eng,dt_mom,dt_ind,NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB)
       end subroutine

       subroutine benchmarkCase_vetted(BMC,Re,Ha,Gr,Fr,Pr,Ec,Al,Rem,&
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
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

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
           Re = 100.0_cp
           dt_mom = 1.0d-4
           NmaxMHD = 4000
           NmaxPPE = 5

         case default; stop 'Error: incorrect BMC in benchmarkCase in benchmarkCase.f90'
           
         end select
       end subroutine

       end module
