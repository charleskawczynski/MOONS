       module MOONS_mod
       use simParams_mod
       use constants_mod
       use myDebug_mod
       use myIO_mod
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
         type(grid) :: grid_mom,grid_ind
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
         ! case (100); Re = 1d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-6
         ! case (100); Re = 400d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-2
         ! case (100); Re = 4.0d0;    Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.679d-3 ! Low Rem for momentum ADI
         case (101); Re = 1000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 2.5d-4
         case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
         ! case (102); Re = 100d0;    Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 5.0d-3
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

         case (250); Re = 15574.07d0;   Ha = 2900.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 5.0d-6
         ! case (250); Re = 1000.07d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-5

         case (300); Re = 1000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3
         case (301); Re = 2000d0;   Ha = 0.0d0    ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-3

         ! case (1001); Re = 100d0;   Ha = 10.0d0   ; Rem = 1.0d0 ; ds = 6.0d-6; dTime = 3.0d-4 ! Ha = 10
         ! case (1001); Re = 100d0;   Ha = 100.0d0  ; Rem = 1.0d0 ; ds = 5.0d-7; dTime = 4.0d-5 ! Ha = 100
         case (1001); Re = 100d0;   Ha = 1000.0d0  ; Rem = 1.0d0 ; ds = 1.0d-8; dTime = 9.0d-7 ! Ha = 1000

         case (1002); Re = 100d0;    Ha = 500.0d0 ; Rem = 1.0d0 ; ds = 1.0d-4; dTime = 1.0d-2
         case (1003); Re = 100d0;    Ha = 10.0d0 ; Rem = 1.0d0  ; ds = 1.0d-5; dTime = ds

         case default
           stop 'Incorrect benchmarkCase in MOONS'
         end select

         select case (benchmarkCase)
         case (1);   NmaxPPE = 5; NmaxB = 0; NmaxMHD = 60000
         case (50);  NmaxPPE = 5; NmaxB = 0; NmaxMHD = 1000000
         case (51);  NmaxPPE = 5; NmaxB = 0; NmaxMHD = 1000000
         
         ! case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 4000
         case (100); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 4000
         case (101); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 250000
         case (102); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 4000
         ! case (102); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 20000
         case (103); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 500000
         case (104); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 3000000

         case (105); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 6000
         case (106); NmaxPPE = 5; NmaxB = 50; NmaxMHD = 20000
         case (107); NmaxPPE = 5; NmaxB = 50; NmaxMHD = 60000
         case (108); NmaxPPE = 5; NmaxB = 50; NmaxMHD = 20000

         case (109); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 60000

         case (200); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 2000
         case (201); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 15000
         case (202); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 1000000

         ! case (250); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10
         case (250); NmaxPPE = 15; NmaxB = 5; NmaxMHD = 10**7 ! Case B2

         case (300); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 100000
         case (301); NmaxPPE = 5; NmaxB = 0; NmaxMHD = 100000

         ! case (1001); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 5*10**5 ! A
         ! case (1001); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**6 ! B
         case (1001); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 10**7 ! C
         case (1002); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 40000
         case (1003); NmaxPPE = 5; NmaxB = 5; NmaxMHD = 100000

         case default
           stop 'Incorrect benchmarkCase in MOONS'
         end select

         ! call getParams(benchmarkCase,Re,Ha,ds,dTime,NmaxB,NmaxPPE,NmaxB,NmaxMHD)

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

         ! call init(vecOps,gd)

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

         ! call computeCurrent(jx,jy,jz,Bx,By,Bz,Bx0,By0,Bz0,mu,Re,Ha,gd)

         call checkGrid(gd)

         write(*,*) ''
         write(*,*) 'Press enter if these parameters are okay.'
         write(*,*) ''

         ! ********************** PREP LOOP ******************************
         ! This is done in both MOONS and MHDSolver, need to fix this..
         if (restartU.or.restartB) then
           call readLastStepFromFile(n_mhd,dir//'parameters/','n_mhd')
         else; n_mhd = 0
         endif

         call writeKillSwitchToFile(.true.,dir//'parameters/','killSwitch')

         ! ******************* SET MHD SOLVER SETTINGS *******************
         call init(ss_MHD)

         call setMaxIterations(ss_MHD,n_mhd+NmaxMHD)
         call setIteration(ss_MHD,n_mhd)

         if ((solveInduction).and.(.not.solveCoupled).and.(solveBMethod.eq.1).and.restartU) then
           call setMaxIterations(ss_MHD,1)
         endif

         ! ********************* SET B SOLVER SETTINGS *******************

         ! call unitTestRelax(ind%B%x,ind%g,dir)
         ! call unitTestADI(ind%B%x,ind%g,dir)
         ! call unitTestMG(ind%B%x,ind%g,dir)
         call MHDSolver(mom,ind,gd,rd,ss_MHD,time,dir)

         ! if (calculateOmegaPsi) call calcOmegaPsi(u,v,w,gd,dir)

         ! ******************* DELETE ALLOCATED DERIVED TYPES ***********

         call delete(ind)
         call delete(mom)
         ! call delete(vecOps)
         call delete(gd)

         call computationComplete(time)
       end subroutine

       subroutine unitTestMG(B,gd,dir)
        implicit none
        type(grid),intent(in) :: gd
        real(dpn),dimension(:,:,:),intent(in) :: B
        character(len=*),intent(in) :: dir
        integer,parameter :: Nlevels = 8
        integer :: i
        type(grid),dimension(Nlevels) :: g

        do i = 1,Nlevels
          call init(g(i),gd)
        enddo

        call export(g(1),dir,'grid_1')
        do i = 1,Nlevels-1
          call restrict(g(i+1),g(i))
          call export(g(i+1),dir,'grid_'//int2str(i+1))
          write(*,*) 'Finished level',i
          ! call delete(g(i))
        enddo
        do i = 1,Nlevels
          call delete(g(i))
        enddo
       end subroutine

       subroutine unitTestADI(B,gd,dir)
        implicit none
        type(grid),intent(in) :: gd
        real(dpn),dimension(:,:,:),intent(in) :: B
        character(len=*),intent(in) :: dir
        real(dpn),dimension(:,:,:),allocatable :: u,u_exact,f
        type(myADI) :: ADI
        type(mySOR) :: SOR
        type(BCs) :: u_bcs
        type(myError) :: e
        type(solverSettings) :: ss
        integer :: i,j,k
        real(dpn) :: p,q,r
        real(dpn),dimension(:,:),allocatable :: bvals
        integer,dimension(3) :: s
        s = shape(B)
        s = s-1
        call setAllZero(u_bcs,s(1),s(2),s(3),1) ! Dirichlet
        ! call setAllZero(u_bcs,s(1),s(2),s(3),4) ! Neumann
        allocate(bvals(s(2),s(3)))

        call setGrid(u_bcs,gd)
        call checkBCs(u_bcs)

        allocate(u(s(1),s(2),s(3)))
        allocate(u_exact(s(1),s(2),s(3)))
        allocate(f(s(1),s(2),s(3)))
        p = 21.0; q = 3.0; r = 3.0
        do k = 1,s(3)
        do j = 1,s(2)
        do i = 1,s(1)
          ! u_exact(i,j,k) = sin(p*PI*gd%xni(i))*sin(q*PI*gd%yni(j))*sin(r*PI*gd%zni(k)) ! Dirichlet
          u_exact(i,j,k) = sin(p/2.0*PI*gd%c(1)%hn(i))*sin(q*PI*gd%c(2)%hn(j))*sin(r*PI*gd%c(3)%hn(k)) ! Neumann
          ! f(i,j,k) = -PI**2.0*((p/2.0)**2.0+q**2.0+r**2.0)*u_exact(i,j,k)
          bvals(j,k) = 1.0
          f(i,j,k) = 0.0
        enddo
        enddo
        enddo
        call setXmaxVals(u_bcs,bvals)
        deallocate(bvals)


        call init(ss)
        call setName(ss,'u                   ')
        call setMaxIterations(ss,10)
        call setSubtractMean(ss)
        call setAlpha(ADI,one)
        call myPoisson(ADI,u,f,u_bcs,gd,ss,e,2,.true.)
        ! call myPoisson(SOR,u,f,u_bcs,gd,ss,e,2,.true.)

        call compute(e,u_exact,u)
        call print(e,'u')

        call writeToFile(gd%c(1)%hn,gd%c(2)%hn,gd%c(3)%hn,u,dir,'u')
        call writeToFile(gd%c(1)%hn,gd%c(2)%hn,gd%c(3)%hn,u_exact,dir,'u_exact')
        call writeToFile(gd%c(1)%hn,gd%c(2)%hn,gd%c(3)%hn,f,dir,'f')

        deallocate(u,u_exact,f)
       end subroutine

       end module
