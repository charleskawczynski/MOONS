       module MOONS_mod
       use simParams_mod
       use IO_tools_mod
       use IO_scalarFields_mod
       use IO_vectorFields_mod
       use IO_auxiliary_mod
       use version_mod
       use myTime_mod
       use grid_mod
       use griddata_mod
       use generateGrids_mod
       use ops_embedExtract_mod
       use ops_interp_mod
       use rundata_mod
       use norms_mod
       use VF_mod

       use solverSettings_mod
       use BCs_mod
       use applyBCs_mod

       use energySolver_mod
       use momentumSolver_mod
       use inductionSolver_mod

       use MHDSolver_mod
       use omp_lib

       implicit none

       private

       public :: MOONS_Parametric,MOONS

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif
       real(cp),parameter :: PI = 3.14159265358979_cp

       contains

       subroutine MOONS_solve(U,B,grid_mom,grid_ind,Ni,Nwtop,Nwbot,dir)
         implicit none
         integer,dimension(3),intent(in) :: Ni,Nwtop,Nwbot
         type(VF),intent(inout) :: U,B
         type(grid),intent(inout) :: grid_mom,grid_ind
         character(len=*),intent(in) :: dir ! Output directory

         ! ********************** BIG VARIABLES *************************
         type(momentum) :: mom
         type(induction) :: ind
         type(energy) :: nrg
         ! ********************** MEDIUM VARIABLES **********************
         type(griddata) :: gd
         type(rundata) :: rd
         type(solverSettings) :: ss_MHD
         type(myTime) :: time
         type(subdomain) :: SD
         ! ********************** SMALL VARIABLES ***********************
         real(cp) :: Re,Ha,Gr,Fr,Pr,Ec,Al,Rem
         real(cp) :: dTime,ds
         integer :: NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB

         integer :: n_mhd
         ! **************************************************************
         call computationInProgress(time)

         call MOONS_setParams(Re,Ha,Gr,Fr,Pr,Ec,Al,Rem,&
         dTime,ds,NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB)

         write(*,*) 'MOONS output directory = ',dir

         ! ****************** CLEAN DIRECTORY ***************************
         call rmDir(dir) ! Does not work yet...
         ! call myPause()
         ! **************** BUILD NEW DIRECTORY *************************
         call makeDir(dir)
         call makeDir(dir,'Tfield')
         call makeDir(dir,'Ufield')
         call makeDir(dir,'Ufield','\transient')
         call makeDir(dir,'Ufield','\energy')
         call makeDir(dir,'Bfield')
         call makeDir(dir,'Bfield','\transient')
         call makeDir(dir,'Bfield','\energy')
         call makeDir(dir,'Jfield')
         call makeDir(dir,'Jfield','\transient')
         call makeDir(dir,'Jfield','\energy')
         call makeDir(dir,'material')
         call makeDir(dir,'parameters')

         call printVersion()
         call exportVersion(dir)

         if (solveInduction) then
           select case(solveBMethod)
           case(1:4)
             if (NmaxB.lt.1) stop 'Error: NmaxB must be larger than 1 for low Rem cases'
           case default
           end select
         endif

         ! **************************************************************
         ! Initialize all grids
         ! call init(gd,grid_mom,grid_ind,Ni,Nwtop,Nwbot,Re,Ha)
         call makeGrids(grid_mom,grid_ind,Ni,Nwtop,Nwbot)
         call init(SD,Ni,Nwtop,Nwbot+10,grid_mom)

         ! Initialize Energy grid/fields/parameters
         call setDTime(nrg,dTime)
         call setPiGroups(nrg,Re,Pr,Ec,Al,Rem)
         call init(nrg,grid_ind,SD,dir)

         ! Initialize Momentum grid/fields/parameters
         call setDTime(mom,dTime)
         call setNMaxPPE(mom,NmaxPPE)
         call setPiGroups(mom,Re,Ha,Gr,Fr)
         call init(mom,grid_mom,dir)

         ! Initialize Induction grid/fields/parameters
         call setDTime(ind,ds)
         call setNmaxB(ind,NmaxB)
         call setNmaxCleanB(ind,NmaxCleanB)
         call setPiGroups(ind,Ha,Rem)
         if (solveInduction) call init(ind,grid_ind,SD,dir)

         ! ******************** EXPORT GRIDS ****************************
         if (exportGrids) call writeToFile(mom%g,dir//'Ufield/','grid_mom')
         ! if (exportGrids) call writeToFile(ind%g,dir//'Tfield/','grid_nrg')
         if (exportGrids) call writeToFile(ind%g,dir//'Bfield/','grid_ind')

         ! ********************* EXPORT RAW ICs *************************
         ! if (exportRawICs) call exportRaw(nrg,nrg%g,dir)
         ! if (exportRawICs) call exportRaw(mom,mom%g,dir)
         ! if (exportRawICs) call exportRaw(ind,ind%g,dir)

         ! ********************* EXPORT ICs *****************************
         ! if (exportICs) call export(mom,mom%g,dir)
         ! if (exportICs) call export(nrg,nrg%g,dir)
         ! if (exportICs) call embedVelocity(ind,mom%U,mom%g)
         if (exportICs) call exportMaterial(ind,dir)
         ! if (exportICs) call export(ind,ind%g,dir)

         ! ****************** INITIALIZE RUNDATA ************************
         ! These all need to be re-evaluated because the Fo and Co now depend
         ! on the smallest spatial step (dhMin)

         call setRunData(rd,dTime,ds,Re,Ha,Rem,&
          mom%U%x,mom%U%y,mom%U%z,grid_mom,grid_ind,addJCrossB,solveBMethod)

         ! ****************** INITIALIZE TIME ***************************
         call init(time)

         ! *************** CHECK IF CONDITIONS ARE OK *******************
         call printGriddata(gd)
         call printRundata(rd)
         call exportGriddata(gd,dir)
         call exportRundata(rd,dir)
         call printExportBCs(ind,dir)
         call printExportBCs(mom,dir)


         if (solveMomentum)  call computeDivergence(mom,mom%g)
         if (solveInduction) call computeDivergence(ind,ind%g)

         ! if (exportRawICs) then
         !   if (solveMomentum)  call exportRaw(mom,mom%g,dir)
         !   if (solveEnergy)    call exportRaw(nrg,nrg%g,dir)
         !   if (solveInduction) call embedVelocity(ind,mom%U,mom%g)
         !   if (solveInduction) call exportRaw(ind,ind%g,dir)
         ! endif
         ! if (exportICs) then
         !   if (solveMomentum)  call export(mom,mom%g,dir)
         !   if (solveEnergy)    call export(nrg,nrg%g,dir)
         !   if (solveInduction) call embedVelocity(ind,mom%U,mom%g)
         !   if (solveInduction) call export(ind,ind%g,dir)
         ! endif

         if (stopAfterExportICs) then
           stop 'Finished exporting ICs. Turn off stopAfterExportICs in simParams.f90 to run sim'
         endif

         ! call checkGrid(gd)

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

         call writeSwitchToFile(.true.,dir//'parameters/','killSwitch')
         call writeSwitchToFile(.false.,dir//'parameters/','exportNow')

         ! ******************* SET MHD SOLVER SETTINGS *******************
         call init(ss_MHD)

         ! call setMaxIterations(ss_MHD,n_mhd+NmaxMHD)
         ! call setIteration(ss_MHD,n_mhd)

         call setMaxIterations(ss_MHD,NmaxMHD)
         call setIteration(ss_MHD,0)

         ! ********************* SET B SOLVER SETTINGS *******************

         call MHDSolver(nrg,mom,ind,ss_MHD,time,dir)
         ! call export(mom,mom%g,dir)
         ! call export(ind,ind%g,dir)

         call init(U,mom%g%c(1)%sn,mom%g%c(2)%sn,mom%g%c(3)%sn)
         call face2Node(U,mom%U,mom%g)
         if (solveInduction) call init(B,ind%g%c(1)%sn,ind%g%c(2)%sn,ind%g%c(3)%sn)
         if (solveInduction) call cellCenter2Node(B,ind%B,ind%g)

         ! ******************* DELETE ALLOCATED DERIVED TYPES ***********

         call delete(ind)
         call delete(mom)
         call delete(gd)

         call computationComplete(time)
       end subroutine

       subroutine MOONS_setParams(Re,Ha,Gr,Fr,Pr,Ec,Al,Rem,&
         dTime,ds,NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB)
         implicit none
         real(cp),intent(inout) :: Re,Ha,Gr,Fr,Pr,Ec,Al,Rem
         real(cp),intent(inout) :: dTime,ds
         integer,intent(inout) :: NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB
         ! ***************** USER DEFINED MHD VARIABLES *****************
         Re = 1000.0d0
         Ha = 100.0d0
         Gr = real(10**6,cp)
         Fr = 0.0d0
         Pr = 0.71d0
         Ec = 0.0d0
         Al = 0.0d0
         Rem = 1.0d0
         dTime = 1.0d-4
         ds = 1.0d-4

         ! NmaxMHD = 100    ! One hundred steps
         ! NmaxMHD = 5000    ! Five thousand steps
         ! NmaxMHD = 10000   ! Ten thousand steps
         ! NmaxMHD = 50000   ! Fifty thousand steps
         ! NmaxMHD = 100000  ! One hundred thousand steps
         ! NmaxMHD = 500000  ! Five hundred thousand steps
         NmaxMHD = 1000000 ! One million steps
         NmaxPPE    = 5 ! Number of PPE steps
         NmaxB      = 5 ! Number of Steps for Low Rem approx to solve B
         NmaxCleanB = 5 ! Number of Steps to clean B

         ! ********** PREPARE BENCHMARK CASE IF DEFINED *****************
         select case (benchmarkCase)
         case (1);   Re = 100d0;   Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
         case (2);   Re = 100d0;   Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
         case (3);   Re = 100d0;   Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
         case (4);   Re = 100d0;   Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = ds

         case (50);  Re = 1970d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3
         case (51);  Re = 3200d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3

         ! case (100); Re = 400d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-2
         ! case (100); Re = 400d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 2.0d-3 ! For mesh refinement
         ! case (100); Re = 400d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.67d-2
         ! case (100); Re = 10000d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 8.0d-4
         case (100); Re = 1000d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 5.0d-3

         ! case (100); Re = 1d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-6
         ! case (100); Re = 400d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-2
         ! case (100); Re = 4.0d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-3 ! Low Rem for momentum ADI
         case (101); Re = 1000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 2.5d-4
         case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 0.01d0 ; ds = 1.0d-4; dTime = 1.0d-2
         ! case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 0.01d0 ; ds = 1.0d-6; dTime = 1.0d-2 ! Low but finite Rem
         ! case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 10.0d0 ; ds = 1.0d-6; dTime = 1.0d-2 ! finite Rem
         ! case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 0.01d0 ; ds = 2.0d-6; dTime = 1.0d-2
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
         case (1003); 
         ds = 1.0d-5; dTime = ds

         ! Ha = 10

         ! Re = 100d0;    Ha = 10.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 500d0;    Ha = 10.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 1000d0;   Ha = 10.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 10000d0;  Ha = 10.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds

         ! Re = 100d0;    Ha = 10.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 500d0;    Ha = 10.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 1000d0;   Ha = 10.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 10000d0;  Ha = 10.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds

         ! Re = 100d0;    Ha = 10.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 500d0;    Ha = 10.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 1000d0;   Ha = 10.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 10000d0;  Ha = 10.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds

         ! Ha = 100

         ! Re = 100d0;    Ha = 100.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds ! streamlines sideways?
         ! Re = 500d0;    Ha = 100.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 1000d0;   Ha = 100.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 10000d0;  Ha = 100.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds

         ! Re = 100d0;    Ha = 100.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 500d0;    Ha = 100.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 1000d0;   Ha = 100.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 10000d0;  Ha = 100.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds

         ! Re = 100d0;    Ha = 100.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds ! Next
         Re = 10000.0_cp; Ha = 10000.0_cp; Rem = 100.0_cp; ds = 1.0d-5; dTime = ds ! Next
         ! Re = 500d0;    Ha = 100.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 1000d0;   Ha = 100.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 10000d0;  Ha = 100.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds

         ! Ha = 1000

         ! Re = 100d0;    Ha = 1000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 500d0;    Ha = 1000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 1000d0;   Ha = 1000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 10000d0;  Ha = 1000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds

         ! Re = 100d0;    Ha = 1000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 500d0;    Ha = 1000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 1000d0;   Ha = 1000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 10000d0;  Ha = 1000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds

         ! Re = 100d0;    Ha = 1000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 500d0;    Ha = 1000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 1000d0;   Ha = 1000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 10000d0;  Ha = 1000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds

         ! Ha = 10000

         ! Re = 100d0;    Ha = 10000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 500d0;    Ha = 10000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 1000d0;   Ha = 10000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 10000d0;  Ha = 10000.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds

         ! Re = 100d0;    Ha = 10000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 500d0;    Ha = 10000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 1000d0;   Ha = 10000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 10000d0;  Ha = 10000.0d0 ; Rem = 10.0d0  ; ds = 1.0d-5; dTime = ds

         ! Re = 100d0;    Ha = 10000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 500d0;    Ha = 10000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 1000d0;   Ha = 10000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds
         ! Re = 10000d0;  Ha = 10000.0d0 ; Rem = 100.0d0  ; ds = 1.0d-5; dTime = ds

         ! case (1004); Re = 400d0;    Ha = 0.0d0 ; Rem = 1.0d0  ; ds = 1.0d-4; dTime = ds
         case (1004); Re = 400d0;    Ha = 0.0d0 ; Rem = 1.0d0  ; ds = 1.0d-3; dTime = ds
         ! Rem = 0.1d0; ds = 1.0d-5
         Rem = 100.0d0; ds = 1.0d-4
         ! Rem = 400.1d00; ds = 1.0d-3
         ! Rem = 1000.0d0; ds = 1.0d-3
         case (1005); Re = 400d0;    Ha = 10.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds
         ! case (1006); Rem = 50.0d0*real(4.0,cp)*PI ; ds = 1.0d-4; dTime = ds
         case (1006); Rem = real(1000.0,cp) ; ds = 5.0d-5; dTime = ds

         case (1007); Rem = real(100.0,cp) ; ds = 3.0d-5; dTime = ds ! Parker

         case (1008); 
         Re = real(200.0,cp)
         Ha = real(25.819888974716115,cp) ! Q  = 0.3 : Q = 1/N = Re/Ha^2 => Ha^2 = Re/Q
         ! Ha = real(20.0,cp)             ! Q  = 0.5 : Q = 1/N = Re/Ha^2 => Ha^2 = Re/Q
         ! Ha = real(100.0,cp)             ! Q  = 0.5 : Q = 1/N = Re/Ha^2 => Ha^2 = Re/Q
         Rem = real(1.0,cp)
         ! Rem = real(0.001,cp)
         ds = 2.0d-5; dTime = ds ! Q = 0.3, good dt
         ! ds = 3.0d-4; dTime = ds

         case (1009)
         Re = real(400.0,cp); Ha = real(20.0,cp)

         ! Rem = real(0.0,cp);    ds = 1.0d-4;   dTime = 1.0d-2 ! (Rem = 0)
         Rem = real(1.0,cp);    ds = 5.0d-5;   dTime = ds     ! (Rem = 1)
         ! Rem = real(10.0,cp);   ds = 5.0d-4;   dTime = ds     ! (Rem = 10)
         ! Rem = real(100.0,cp);  ds = 5.0d-3;   dTime = ds     ! (Rem = 100)

         case (1010)
         Re = real(400.0,cp); Ha = real(20.0,cp)

         ! Rem = real(0.0,cp);    ds = 1.0d-4;   dTime = 1.0d-2 ! (Rem = 0)
         ! Rem = real(1.0,cp);    ds = 5.0d-5;   dTime = ds     ! (Rem = 1)
         ! Rem = real(10.0,cp);   ds = 5.0d-4;   dTime = ds     ! (Rem = 10)
         ! Rem = real(100.0,cp);  ds = 5.0d-5;   dTime = ds     ! (Rem = 100) , sigma* = 0.01
         Rem = real(100.0,cp);  ds = 2.0d-6;   dTime = ds     ! (Rem = 100) , sigma* = 0.001, fine grid

         case (1011)
         Re = real(10.0,cp)
         Ha = real(500.0,cp)
         Rem = real(100.0,cp)
         ds = 5.0d-8
         dTime = 1.0d-7

         case (1012)
         Re = real(100.0,cp); Ha = real(45.0,cp)
         ds = 5.0d-5; dTime = 1.0d-3

         case (1013)
         Re = 400.0_cp
         dTime = 10.0_cp**(-3.0_cp)

         case default
           stop 'Incorrect benchmarkCase in MOONS'
         end select

         select case (benchmarkCase)
         case (1);   NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4000
         case (2);   NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4000
         case (3);   NmaxPPE = 5; NmaxB = 5; NmaxMHD = 8000
         ! case (4);   NmaxPPE = 5; NmaxB = 5; NmaxMHD = 8000
         case (4);   NmaxPPE = 5; NmaxB = 5; NmaxMHD = 8000

         case (50);  NmaxPPE = 5; NmaxB = 0; NmaxMHD = 1000000
         case (51);  NmaxPPE = 5; NmaxB = 0; NmaxMHD = 1000000
         
         ! case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 4000
         ! case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 80000
         ! case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 10000
         case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 10**5
         ! case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 70000 ! For convergence rate test

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
         ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**5 ! Mimicking PD
         ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**1 ! Mimicking PD
         ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! Mimicking PD
         ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**6 ! Mimicking PD
         ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 400000 ! Mimicking PD
         ! case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! Mimicking PD
         case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 1 ! Mimicking PD

         ! case (1004); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**5 ! Salah
         ! case (1004); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 2*10**4
         case (1004); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**6
         case (1005); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 8000
         case (1006); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4*5*10**4
         case (1007); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 12*10**6
         ! case (1008); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7
         case (1008); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**6 ! Q = 0.3

         case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! for testing

         ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 2*10**3 ! (Rem = 0) B0x
         ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 2*10**3 ! (Rem = 0) B0y - done
         ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 2*10**3 ! (Rem = 0) B0z

         ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3*10**4 ! (Rem = 1) B0x
         ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3*10**4 ! (Rem = 1) B0y - pending
         ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3*10**4 ! (Rem = 1) B0z

         ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3*10**4 ! (Rem = 10) B0x
         ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3*10**4 ! (Rem = 10) B0y - done
         ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3*10**4 ! (Rem = 10) B0z

         ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4*10**4 ! (Rem = 100) B0x
         ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4*10**4 ! (Rem = 100) B0y - done
         ! case (1009); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4*10**4 ! (Rem = 100) B0z
         ! case (1010); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! for testing
         ! case (1010); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 6*10**6 ! for testing
         case (1010); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 20*10**6 ! for testing

         case (1011); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! for testing
         case (1012); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! for testing

         case (1013); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! for testing

         case default
           stop 'Incorrect benchmarkCase in MOONS'
         end select
       end subroutine

       ! ***************************************************************
       ! ***************************************************************
       ! ******************** For Single Simulation ********************
       ! ***************************************************************
       ! ***************************************************************

       subroutine MOONS_Single_Grid(Ni,Nwtop,Nwbot)
         implicit none
         integer,dimension(3),intent(inout) :: Ni,Nwtop,Nwbot
         ! ***************************************************************
         ! ***************************************************************
         ! *************************** For BMCs **************************
         ! ***************************************************************
         ! ***************************************************************
         select case (benchmarkCase)
         case (1);    Ni = 48;            Nwtop = 0;           Nwbot = 0         ! (LDC: Purely Hydrodynamic / Insulating)
         case (2);    Ni = 40;            Nwtop = 8;           Nwbot = 8         ! (LDC: Conducting)
         case (3);    Ni = (/64,32,32/);  Nwtop = 0;           Nwbot = 0         ! (Duct: Purely Hydrodynamic / Insulating)
         case (4);    Ni = (/1,100,100/); Nwtop = (/0,5,5/);   Nwbot = (/0,5,5/) ! (Duct: Conducting)
         case (50);   Ni = 105;           Nwtop = 0;           Nwbot = 0
         case (51);   Ni = 105;           Nwtop = 0;           Nwbot = 0
         ! case (100);  Ni = (/67,67,27/);  Nwtop = 0;           Nwbot = 0
         case (100);  Ni = (/128,128,1/);  Nwtop = 0;           Nwbot = 0
         ! case (100);  Ni = (/256,256,1/);  Nwtop = 0;           Nwbot = 0
         case (101);  Ni = 52;            Nwtop = 0;           Nwbot = 0
         case (102);  Ni = 45;            Nwtop = 11;          Nwbot = 11
         case (103);  Ni = 45;            Nwtop = 11;          Nwbot = 11
         case (104);  Ni = 51;            Nwtop = 5;           Nwbot = 5
         case (105);  Ni = 45;            Nwtop = (/11,0,11/); Nwbot = 11
         case (106);  Ni = 45;            Nwtop = (/11,0,11/); Nwbot = 11
         case (107);  Ni = 45;            Nwtop = (/11,0,11/); Nwbot = 11
         case (108);  Ni = 45;            Nwtop = (/11,0,11/); Nwbot = 11
         case (109);  Ni = 45;            Nwtop = (/11,2,11/); Nwbot = (/11,2,11/)
         case (200);  Ni = (/129,33,33/); Nwtop = 0;           Nwbot = 0
         case (201);  Ni = (/101,32,32/); Nwtop = (/0,5,5/);   Nwbot = (/0,5,5/)
         case (202);  Ni = (/181,47,47/); Nwtop = (/0,5,5/);   Nwbot = (/0,5,5/)
         case (250);  Ni = (/200,51,56/); Nwtop = (/0,5,5/);   Nwbot = (/0,5,5/)
         case (300);  Ni = 51;            Nwtop = 0;           Nwbot = 0
         case (301);  Ni = 101;           Nwtop = 0;           Nwbot = 0
         case (1001); Ni = 52;            Nwtop = (/8,0,8/);   Nwbot = 8 ! Ha = 10,100,1000
         case (1002); Ni = (/65,45,45/);  Nwtop = 0;           Nwbot = 0     ! Insulating
         ! case (1003); Ni = (/75,45,45/);  Nwtop = 11;          Nwbot = 11
         ! case (1003); Ni = (/1,64,64/); Nwtop = (/0,11,11/); Nwbot = Nwtop
         ! case (1003); Ni = (/1,100,100/); Nwtop = (/0,5,5/); Nwbot = Nwtop
         ! case (1003); Ni = (/1,160,160/); Nwtop = (/0,8,8/); Nwbot = Nwtop
         ! case (1003); Ni = (/1,260,260/); Nwtop = (/0,13,13/); Nwbot = Nwtop
         ! case (1003); Ni = (/64,64,1/); Nwtop = 0; Nwbot = Nwtop
         case (1003); Ni = (/64,64,1/); Nwtop = 0; Nwbot = Nwtop ! SK flow

         case (1004); Ni = 35;            Nwtop = 0;           Nwbot = 0
         case (1005); Ni = (/64,32,32/);  Nwtop = 0;           Nwbot = 0  ! (Jack's Experiment)
         ! case (1006); Ni = (/64,64,1/);   Nwtop = 32;          Nwbot = 32 ! (Weiss, Isolated Eddy)
         ! Nwtop(3) = 0;        Nwbot(3) = 0                                ! (Weiss, Isolated Eddy)

         ! case (1006); Ni = (/200,100,1/);   Nwtop = 0;           Nwbot = 0  ! (Weiss, Single Eddy)
         case (1006); Ni = (/400,100,1/);   Nwtop = 0;           Nwbot = 0  ! (Weiss, Single Eddy)
         ! case (1006); Ni = (/100,100,1/);   Nwtop = 0;           Nwbot = 0  ! (Weiss, Single Eddy)

         case (1007); Ni = (/100,100,1/);   Nwtop = 0;           Nwbot = 0  ! (Parker, Cylinder)

         ! case (1008); Ni = (/100,1,100/);   Nwtop = 0;           Nwbot = 0  ! (Bandaru)
         case (1008); Ni = (/64,1,64/);   Nwtop = 0;           Nwbot = 0  ! (Bandaru)
         case (1009); Ni = 50;            Nwtop = 0;           Nwbot = 0  ! (Kawczynski - demo)
         ! case (1009); Ni = 100;            Nwtop = 0;           Nwbot = 0  ! (Kawczynski - demo)
         ! case (1010); Ni = 50;            Nwtop = 25;          Nwbot = 25  ! (Kawczynski - demo) for sigma* = 0.01
         case (1010); Ni = 30;            Nwtop = (/8,0,8/);   Nwbot = 8  ! (Kawczynski - demo) for sigma* = 0.001
         case (1011); Ni = (/1,45,45/);   Nwtop = 0;           Nwbot = 0  ! (Kawczynski - demo) for shercliff / hunt flow
         case (1012); Ni = 64;            Nwtop = 0;           Nwbot = 0  ! Pattison
         case (1013); Ni = (/128,128,1/);   Nwtop = 0;           Nwbot = 0  ! Pattison

         case default
           Ni = (/64,32,32/)
           Nwtop = 0
           Nwbot = 0
         end select
         write(*,*) 'Ni = ',Ni
         write(*,*) 'Nwtop = ',Nwtop
         write(*,*) 'Nwbot = ',Nwbot
       end subroutine

       subroutine MOONS(dir)
         implicit none
         character(len=*),intent(in) :: dir ! Output directory
         type(VF) :: U,B
         integer,dimension(3) :: Ni,Nwtop,Nwbot
         type(grid) :: grid_mom,grid_ind
         call MOONS_Single_Grid(Ni,Nwtop,Nwbot)
         call MOONS_solve(U,B,grid_mom,grid_ind,Ni,Nwtop,Nwbot,dir)
         call delete(U)
         call delete(B)
         call delete(grid_mom)
         call delete(grid_ind)
       end subroutine

       ! ***************************************************************
       ! ***************************************************************
       ! ********************* For Grid Refinement *********************
       ! ***************************************************************
       ! ***************************************************************

       subroutine MOONS_Parametric_Grid(Ni,Nwtop,Nwbot,Nall)
         implicit none
         integer,dimension(3),intent(inout) :: Nwtop,Nwbot,Ni
         integer,dimension(3),intent(in) :: Nall
         ! 
         ! N = 2**{5,6,7}
         ! 
         ! For non-zero walls, N must > k for Nwtop = N/2**k
         ! 
         !                                                                 ! Thinner walls
         ! Ni = (N/2); Nwtop = N/2**2                                      !    |
         ! Ni = (N/2 + N/2**2); Nwtop = N/2**3                             !    |
         ! Ni = (N/2 + N/2**2 + N/2**3); Nwtop = N/2**4                    !    |
         ! Ni = (N/2 + N/2**2 + N/2**3 + N/2**4); Nwtop = N/2**5           !    V
         ! Ni = (N/2 + N/2**2 + N/2**3 + N/2**4 + N/2**5); Nwtop = N/2**6  !

         Ni = Nall; Nwtop = 0; Nwbot = Nwtop

         ! Ni = (N/2); Nwtop = N/2**2                                      ! Good for N = 2**{3,4,5}
         ! Ni = (N/2 + N/2**2); Nwtop = N/2**3                             ! Good for N = 2**{4,5,6}
         ! Ni = (N/2 + N/2**2 + N/2**3); Nwtop = N/2**4                    ! Good for N = 2**{5,6,7}
         ! Ni = (N/2 + N/2**2 + N/2**3 + N/2**4); Nwtop = N/2**5           ! Good for N = 2**{6,7,8}
         ! Ni = (N/2 + N/2**2 + N/2**3 + N/2**4 + N/2**5); Nwtop = N/2**6  ! Good for N = 2**{7,8,9}

         ! Ni = Nall
         ! Nwtop = Ni*5/100
         ! Nwbot = Nwtop
         ! Ni(1) = 1
         ! Nwbot(1) = 0
         ! Nwtop(1) = 0
       end subroutine

       subroutine MOONS_Parametric(U,B,grid_mom,grid_ind,Nall,dir)
         implicit none
         character(len=*),intent(in) :: dir ! Output directory
         type(VF),intent(inout) :: U,B
         type(grid),intent(inout) :: grid_mom,grid_ind
         integer,intent(in) :: Nall
         integer,dimension(3) :: Nwtop,Nwbot,Ni
         call MOONS_Parametric_Grid(Ni,Nwtop,Nwbot,(/Nall,Nall,Nall/))
         call MOONS_solve(U,B,grid_mom,grid_ind,Ni,Nwtop,Nwbot,dir)
       end subroutine

       end module
