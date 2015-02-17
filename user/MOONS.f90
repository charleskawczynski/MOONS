       module MOONS_mod
       use simParams_mod
       use constants_mod
       use myDebug_mod
       use myIO_mod
       use version_mod
       use myTime_mod
       use myAllocate_mod
       use griddata_mod
       use rundata_mod
       use myError_mod
       use vectorOps_mod
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

         ! *********************** LOCAL VARIABLES **********************
         type(griddata) :: gd
         type(rundata) :: rd
         integer :: n_mhd ! Number of Steps reached so far
         ! **************************************************************
         type(momentum) :: mom
         type(induction) :: ind
         ! type(vectorOps) :: vecOps
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
         case (101); Re = 1000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 2.5d-4
         case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
         case (103); Re = 1000d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 3.0d-4
         case (104); Re = 1000d0;   Ha = 1000.0d0 ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.9d-6

         case (105); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
         case (106); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-6; dTime = 1.0d-2
         case (107); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-7; dTime = 3.0d-2
         case (108); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-7; dTime = 1.0d-2 ! Has not worked yet

         case (109); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2

         case (200); Re = 100d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
         case (201); Re = 1000d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-4
         case (202); Re = 1000d0;   Ha = 500.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-5

         case (300); Re = 1000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3
         case (301); Re = 2000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3
         case default
           write(*,*) 'Incorrect benchmarkCase in MOONS';stop
         end select

         select case (benchmarkCase)
         case (1);   NmaxPPE = 5; NmaxB = 0; NmaxMHD = 60000
         case (50);  NmaxPPE = 5; NmaxB = 0; NmaxMHD = 1000000
         case (51);  NmaxPPE = 5; NmaxB = 0; NmaxMHD = 1000000
         
         case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 4000
         case (101); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 250000
         case (102); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4000
         case (103); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 500000
         case (104); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3000000

         case (105); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 6000
         case (106); NmaxPPE = 5; NmaxB = 50; NmaxMHD = 20000 ! ds = 1.0d-6, NmaxB = 50
         case (107); NmaxPPE = 5; NmaxB = 50; NmaxMHD = 60000 ! ds = 1.0d-7, NmaxB = 50
         case (108); NmaxPPE = 5; NmaxB = 50; NmaxMHD = 20000 ! ds = 1.0d-6, NmaxB = 50

         case (109); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 60000

         case (200); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 1000
         case (201); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 15000
         case (202); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 1000000

         case (300); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 100000
         case (301); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 100000
         case default
           write(*,*) 'Incorrect benchmarkCase in MOONS';stop
         end select

         ! **************************************************************
         call setGriddata(gd,Re,Ha)
         call initialize(mom,gd,dir)
         call initialize(ind,gd,dir)
         ! call initialize(vecOps,gd)

         write(*,*) 'MOONS output directory = ',dir

         ! ****************** CLEAN DIRECTORY ***************************
         call rmDir(dir) ! Does not work yet...
         call myPause()
         ! **************** BUILD NEW DIRECTORY *************************
         call makeDir(dir)
         call makeDir(dir,'Ufield')
         call makeDir(dir,'Bfield')
         call makeDir(dir,'Jfield')
         call makeDir(dir,'material')
         call makeDir(dir,'parameters')

         call printVersion()
         call exportVersion(dir)

         ! ****************** INITIALIZE RUNDATA ************************
         ! These all need to be re-evaluated because the Fo and Co now depend
         ! on the smallest spatial step (dhMin)

         call setRunData(rd,dTime,ds,Re,Ha,Rem,&
          mom%U%x,mom%U%y,mom%U%z,gd,solveCoupled,solveBMethod)

         ! ****************** INITIALIZE TIME ***************************
         call initialize(time)

         ! *************** CHECK IF CONDITIONS ARE OK *******************
         call printGriddata(gd)
         call printRundata(rd)
         call exportGriddata(gd,dir)
         call exportRundata(rd,dir)
         call printExportBCs(ind,dir)
         call printExportBCs(mom,dir)
         call computeDivergence(mom,gd)
         call computeDivergence(ind,gd)

         ! call computeCurrent(jx,jy,jz,Bx,By,Bz,Bx0,By0,Bz0,mu,Re,Ha,gd)

         if ((.not.(restartU.and.restartB)).and.(.not.quickStart)) then
           call exportRaw(mom,gd,dir)
           call exportRaw(ind,gd,dir)
         endif

         call checkGrid(gd)

         write(*,*) ''
         write(*,*) 'Press enter if these parameters are okay.'
         write(*,*) ''
         if (checkICs) call myPause()

         ! ********************** PREP LOOP ******************************
         ! This is done in both MOONS and MHDSolver, need to fix this..
         if (restartU.or.restartB) then
           call readLastStepFromFile(n_mhd,dir//'parameters/','n_mhd')
         else; n_mhd = 0
         endif

         call writeKillSwitchToFile(.true.,dir//'parameters/','killSwitch')

         ! ******************* SET MHD SOLVER SETTINGS *******************
         call initializeSolverSettings(ss_MHD)

         call setMaxIterations(ss_MHD,n_mhd+NmaxMHD)
         call setIteration(ss_MHD,n_mhd)

         if ((solveInduction).and.(.not.solveCoupled).and.(solveBMethod.eq.1).and.restartU) then
           call setMaxIterations(ss_MHD,1)
         endif

         ! ********************* SET B SOLVER SETTINGS *******************

         ! call unitTestRelax(ind%B%x,gd,dir)
         call MHDSolver(mom,ind,gd,rd,ss_MHD,time,dir)

         ! if (calculateOmegaPsi) call calcOmegaPsi(u,v,w,gd,dir)

         ! ******************* DELETE ALLOCATED DERIVED TYPES ***********

         call delete(ind,dir)
         call delete(mom,dir)
         ! call delete(vecOps)
         call delete(gd)

         call computationComplete(time)
       end subroutine

       subroutine unitTestRelax(B,gd,dir)
        implicit none
        type(griddata),intent(in) :: gd
        character(len=*),intent(in) :: dir
        real(dpn),dimension(:,:,:),intent(in) :: B
        real(dpn),dimension(:,:,:),allocatable :: u,u_exact,f
        real(dpn),dimension(:,:),allocatable :: bvals
        type(myADI) :: ADI
        type(BCs) :: u_bcs
        type(myError) :: e
        real(dpn) :: alpha
        integer :: i
        integer,dimension(3) :: s
        s = shape(B)
        s = s-1
        alpha = 2.0d0
        ADI%alpha = alpha
        call setAllZero(u_bcs,s(1),s(2),s(3),1)
        allocate(bvals(s(2),s(3)))
        bvals = -1.0d0/(4.0d0*PI**2.0d0*alpha)
        ! write(*,*) 'shape(bvals) = ',shape(bvals)
        call setXminVals(u_bcs,bvals)
        call setXmaxVals(u_bcs,bvals)
        deallocate(bvals)
        call setGrid(u_bcs,gd)
        call checkBCs(u_bcs)

        allocate(u(s(1),s(2),s(3)))
        allocate(u_exact(s(1),s(2),s(3)))
        allocate(f(s(1),s(2),s(3)))
        do i=1,s(1)
          f(i,:,:) = cos(2.0d0*PI*gd%xni(i))
        enddo
        ! do i=1,s(1)
        !   write(*,*) 'xni(',i,') = ',gd%xni(i)
        ! enddo
        call applyBCFace(u_bcs,u,gd,1)
        call applyBCFace(u_bcs,u,gd,4)

        call writeToFile(gd%xni,u(:,3,3),dir//'Test/','u0',.false.)
        ADI%dt = 0.01d0
        call solveADI1D(ADI,u,f,u_bcs,gd)

        do i=1,s(1)
          u_exact(i,:,:) = -cos(2.0d0*PI*gd%xni(i))/(4.0d0*PI**2.0d0*alpha)
        enddo
        call computeError(e,u_exact,u)
        call printMyError(e,'u')
        call writeToFile(gd%xni,gd%yni,gd%zni,u,dir//'Test/','u')
        call writeToFile(gd%xni,gd%yni,gd%zni,u_exact,dir//'Test/','u_exact')
        call writeToFile(gd%xni,gd%yni,gd%zni,f,dir//'Test/','f')

        call writeToFile(gd%xni,u(:,3,3),dir//'Test/','u1D',.false.)
        call writeToFile(gd%xni,u_exact(:,3,3),dir//'Test/','u_exact1D',.false.)
        call writeToFile(gd%xni,f(:,3,3),dir//'Test/','f1D',.false.)

        deallocate(u,u_exact,f)
      end subroutine

       end module
