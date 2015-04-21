       module MOONS_mod
       use simParams_mod
       use constants_mod
       use myDebug_mod
       use IO_tools_mod
       use IO_scalarFields_mod
       use IO_vectorFields_mod
       use IO_auxiliary_mod
       use version_mod
       use myTime_mod
       use grid_mod
       use griddata_mod
       use rundata_mod
       use myError_mod
       use delOps_mod
       use scalarField_mod
       use vectorField_mod

       use solverSettings_mod
       use BCs_mod
       use applyBCs_mod

       use initializeUBCs_mod
       use initializeUfield_mod

       use initializeBBCs_mod
       use initializeBfield_mod
       use initializeSigmaMu_mod

       use momentumSolver_mod
       use inductionSolver_mod

       use mySOR_mod
       use myADI_mod
       use myPoisson_mod

       use MHDSolver_mod
       use omp_lib

       implicit none
       contains

       subroutine MOONS(dir)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         character(len=*),intent(in) :: dir ! Output directory

         ! ***************** USER DEFINED MHD VARIABLES *****************
         real(dpn) :: Re = 1000.0d0
         real(dpn) :: Ha = 100.0d0
         real(dpn) :: Rem = 1.0d0
         ! real(dpn) :: dTime = 0.025
         ! real(dpn) :: dTime = 0.01d0   ! Case 3: LDC Re100Ha10
         real(dpn) :: dTime = 0.00035d0  ! Case 4: LDC Re1000Ha100
         real(dpn) :: ds = 1.0d-4     ! Case 4: LDC Re1000Ha100

         ! integer :: NmaxMHD = 100    ! One hundred steps
         ! integer :: NmaxMHD = 5000    ! Five thousand steps
         ! integer :: NmaxMHD = 10000   ! Ten thousand steps
         ! integer :: NmaxMHD = 50000   ! Fifty thousand steps
         ! integer :: NmaxMHD = 100000  ! One hundred thousand steps
         ! integer :: NmaxMHD = 500000  ! Five hundred thousand steps
         integer :: NmaxMHD = 1000000 ! One million steps

         integer :: NmaxPPE    = 5 ! Number of PPE steps
         integer :: NmaxB      = 5 ! Number of Steps for Low Rem approx to solve B
         integer :: NmaxCleanB = 5 ! Number of Steps to clean B

         ! *********************** LOCAL VARIABLES **********************
         type(griddata) :: gd
         type(rundata) :: rd
         integer :: n_mhd ! Number of Steps reached so far
         ! **************************************************************
         type(momentum) :: mom
         type(induction) :: ind
         ! type(energy) :: nrg
         type(grid) :: grid_mom,grid_ind
         type(solverSettings) :: ss_MHD
         type(myTime) :: time

         ! **************************************************************
         call computationInProgress(time)

         ! ********** PREPARE BENCHMARK CASE IF DEFINED *****************
         select case (benchmarkCase)
         case (1);   Re = 1000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
         case (50);  Re = 1970d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3
         case (51);  Re = 3200d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3

         case (100); Re = 400d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-2
         ! case (100); Re = 1d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-6
         ! case (100); Re = 400d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-2
         ! case (100); Re = 4.0d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-3 ! Low Rem for momentum ADI
         case (101); Re = 1000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 2.5d-4
         case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
         ! case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 5.0d-3
         ! case (103); Re = 1000d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 3.0d-4
         case (103); Re = 1000d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 4.0d-7; dTime = 3.0d-4
         case (104); Re = 1000d0;   Ha = 1000.0d0 ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.9d-6

         case (105); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
         case (106); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-6; dTime = 1.0d-2
         case (107); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-7; dTime = 3.0d-2
         case (108); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-7; dTime = 1.0d-2 ! Has not worked yet

         case (109); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2

         case (200); Re = 200d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 5.0d-3
         case (201); Re = 1000d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-4
         case (202); Re = 1000d0;   Ha = 500.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-5

         case (250); Re = 15574.07d0;   Ha = 2900.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 5.0d-6
         ! case (250); Re = 1000.07d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-5

         case (300); Re = 1000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3
         case (301); Re = 2000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3

         ! case (1001); Re = 100d0;   Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 6.0d-6; dTime = 3.0d-4 ! Ha = 10
         ! case (1001); Re = 100d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 5.0d-7; dTime = 4.0d-5 ! Ha = 100
         case (1001); Re = 100d0;   Ha = 1000.0d0  ; Rem = 1.0d0 ; ds = 1.0d-8; dTime = 9.0d-7 ! Ha = 1000

         ! case (1002); Re = 10d0;    Ha = 500.0d0 ; Rem = 1.0d0 ; ds = 1.0d-8; dTime = 5.0d-7
         case (1002); Re = 10d0;    Ha = 500.0d0 ; Rem = 1.0d0 ; ds = 8.0d-9; dTime = 2.0d-7
         ! case (1002); Re = 100d0;    Ha = 500.0d0 ; Rem = 1.0d0 ; ds = 1.0d-6; dTime = 1.0d-5
         case (1003); Re = 100d0;    Ha = 10.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds

         ! case (1004); Re = 400d0;    Ha = 0.0d0 ; Rem = 1.0d0  ; ds = 1.0d-4; dTime = ds
         case (1004); Re = 400d0;    Ha = 0.0d0 ; Rem = 1.0d0  ; ds = 1.0d-3; dTime = ds
         ! Rem = 0.1d0; ds = 1.0d-5
         Rem = 100.0d0; ds = 1.0d-4
         ! Rem = 400.1d00; ds = 1.0d-3
         ! Rem = 1000.0d0; ds = 1.0d-3

         case default
           stop 'Incorrect benchmarkCase in MOONS'
         end select

         select case (benchmarkCase)
         case (1);   NmaxPPE = 5; NmaxB = 0; NmaxMHD = 60000
         case (50);  NmaxPPE = 5; NmaxB = 0; NmaxMHD = 1000000
         case (51);  NmaxPPE = 5; NmaxB = 0; NmaxMHD = 1000000
         
         ! case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 4000
         case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 4000
         case (101); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 3*10**5
         case (102); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4000
         ! case (102); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 20000
         case (103); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 500000
         case (104); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3000000

         case (105); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 6000
         case (106); NmaxPPE = 5; NmaxB = 50; NmaxMHD = 20000
         case (107); NmaxPPE = 5; NmaxB = 50; NmaxMHD = 60000
         case (108); NmaxPPE = 5; NmaxB = 50; NmaxMHD = 20000

         case (109); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 60000

         case (200); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 4*10**5 ! Insul
         ! case (200); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 7500
         case (201); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 15000
         case (202); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 1000000

         ! case (250); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10
         case (250); NmaxPPE = 15; NmaxB = 5; NmaxMHD = 10**7 ! Case B2

         case (300); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 100000
         case (301); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 100000

         ! case (1001); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 5*10**5 ! A
         ! case (1001); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**6 ! B
         case (1001); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! Shercliff flow
         case (1002); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! Hunt flow
         case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**5 ! Mimicking PD
         ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**1 ! Mimicking PD

         ! case (1004); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**5 ! Salah
         ! case (1004); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 2*10**4
         case (1004); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**6

         case default
           stop 'Incorrect benchmarkCase in MOONS'
         end select

         write(*,*) 'MOONS output directory = ',dir

         ! ****************** CLEAN DIRECTORY ***************************
         call rmDir(dir) ! Does not work yet...
         call myPause()
         ! **************** BUILD NEW DIRECTORY *************************
         call makeDir(dir)
         call makeDir(dir,'Tfield')
         call makeDir(dir,'Ufield')
         call makeDir(dir,'Bfield')
         call makeDir(dir,'Jfield')
         call makeDir(dir,'material')
         call makeDir(dir,'parameters')

         call printVersion()
         call exportVersion(dir)

         ! **************************************************************
         ! Initialize all grids
         call init(gd,grid_mom,grid_ind,Re,Ha)

         ! Initialize Momentum grid/fields/parameters
         call setDTime(mom,dTime)
         call setRe(mom,Re)
         call setNMaxPPE(mom,NmaxPPE)
         if (exportGrids) then
          call export(grid_mom,dir//'Ufield/','grid_mom')
         endif
         call init(mom,grid_mom,dir)
         if (exportRawICs) then
           call exportRaw(mom,mom%g,dir)
         endif

         ! Initialize Induction grid/fields/parameters
         call setDTime(ind,ds)
         call setNmaxB(ind,NmaxB)
         call setRem(ind,Rem)
         call setNmaxCleanB(ind,NmaxCleanB)
         if (exportGrids) then
          call export(grid_ind,dir//'Bfield/','grid_ind')
         endif
         call init(ind,grid_ind,dir)
         if (exportRawICs) then
           call exportRaw(ind,ind%g,dir)
         endif

         ! ****************** INITIALIZE RUNDATA ************************
         ! These all need to be re-evaluated because the Fo and Co now depend
         ! on the smallest spatial step (dhMin)

         call setRunData(rd,dTime,ds,Re,Ha,Rem,&
          mom%U%x,mom%U%y,mom%U%z,grid_mom,grid_ind,solveCoupled,solveBMethod)

         ! ****************** INITIALIZE TIME ***************************
         call init(time)

         ! *************** CHECK IF CONDITIONS ARE OK *******************
         call printGriddata(gd)
         call printRundata(rd)
         call exportGriddata(gd,dir)
         call exportRundata(rd,dir)
         call printExportBCs(ind,dir)
         call printExportBCs(mom,dir)
         call computeDivergence(mom,mom%g)
         call computeDivergence(ind,ind%g)

         if (exportRawICs) then
           call exportRaw(mom,mom%g,dir)
           call exportRaw(ind,ind%g,dir)
         endif
         if (exportICs) then
           call export(mom,mom%g,dir)
           call export(ind,ind%g,dir)
         endif

         call checkGrid(gd)

         write(*,*) ''
         write(*,*) 'Press enter if these parameters are okay.'
         write(*,*) ''

         ! ********************** PREP LOOP ******************************
         ! This is done in both MOONS and MHDSolver, need to fix this..
         ! if (restartU.and.(.not.solveMomentum)) n_mhd = mom%nstep + ind%nstep
         ! if (restartB.and.(.not.solveInduction)) n_mhd = mom%nstep + ind%nstep

         ! n_mhd = maxval(/mom%nstep,ind%nstep/) ! What if U = fixed, and B is being solved?

         n_mhd = 0 ! Only counts MHD loop, no data is plotted vs MHD step, only n_mom,n_ind,n_nrg

         ! n_mhd = maxval(/mom%nstep,ind%nstep,nrg%nstep/)
         ! if (restartU.or.restartB) then
         !   call readLastStepFromFile(n_mhd,dir//'parameters/','n_mhd')
         !   n_mhd = n_mhd + 1
         ! else; n_mhd = 0
         ! endif
         ! n_mhd = 0

         call writeKillSwitchToFile(.true.,dir//'parameters/','killSwitch')

         ! ******************* SET MHD SOLVER SETTINGS *******************
         call init(ss_MHD)

         ! call setMaxIterations(ss_MHD,n_mhd+NmaxMHD)
         ! call setIteration(ss_MHD,n_mhd)

         call setMaxIterations(ss_MHD,NmaxMHD)
         call setIteration(ss_MHD,0)

         ! if ((solveInduction).and.(.not.solveCoupled).and.(solveBMethod.eq.1).and.restartU) then
         !   call setMaxIterations(ss_MHD,1)
         ! endif

         ! ********************* SET B SOLVER SETTINGS *******************

         call MHDSolver(mom,ind,gd,rd,ss_MHD,time,dir)

         ! ******************* DELETE ALLOCATED DERIVED TYPES ***********

         call delete(ind)
         call delete(mom)
         call delete(gd)

         call computationComplete(time)
       end subroutine

       end module
