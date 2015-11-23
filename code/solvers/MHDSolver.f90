       module MHDSolver_mod
       use simParams_mod
       use VF_mod
       use IO_auxiliary_mod
       use myTime_mod
       use solverSettings_mod

       use energySolver_mod
       use momentumSolver_mod
       use inductionSolver_mod
       implicit none
       
#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       private
       public :: MHDSolver

       contains

       subroutine MHDSolver(nrg,mom,ind,ss_MHD,time,dir)
         implicit none
         type(energy),intent(inout) :: nrg
         type(momentum),intent(inout) :: mom
         type(induction),intent(inout) :: ind
         type(solverSettings),intent(inout) :: ss_MHD
         type(myTime),intent(inout) :: time
         character(len=*),intent(in) :: dir ! Output directory
         ! *********************** LOCAL VARIABLES **********************
         type(VF) :: F ! Forces added to momentum equation
         integer :: n_mhd
         logical :: continueLoop

         call computationInProgress(time)
         call init(F,mom%U)
         call assign(F,0.0_cp)

         ! ********************** PREP LOOP ******************************
         continueLoop = .true.
         n_mhd = getIteration(ss_MHD)

         call writeSwitchToFile(.true.,dir//'parameters/','killSwitch')
         call writeSwitchToFile(.false.,dir//'parameters/','exportNowU')
         call writeSwitchToFile(.false.,dir//'parameters/','exportNowB')
         call writeSwitchToFile(.false.,dir//'parameters/','exportNowT')
         ! ***************************************************************
         ! ********** SOLVE MHD EQUATIONS ********************************
         ! ***************************************************************
         do while (continueLoop)
           call startTime(time)
           call setIteration(ss_MHD,n_mhd)
           if (solveEnergy)    call solve(nrg,mom%U,mom%m,ss_MHD,dir)
           if (solveMomentum)  call solve(mom,F,ss_MHD,dir)
           ! if (solveInduction) call solve(ind,mom%U_E,mom%m,ss_MHD,dir)
           if (solveInduction) call solve(ind,mom%U_CC,mom%m,ss_MHD,dir)

           call assign(F,0.0_cp)
           if (addJCrossB)  call computeAddJCrossB(F,ind,ind%Ha,mom%Re,ind%Rem)
           if (addBuoyancy) call computeAddBuoyancy(F,nrg,mom%Gr,mom%Re)
           if (addGravity)  call computeAddGravity(F,nrg,mom%Fr)

           call checkCondition(ss_MHD,continueLoop) ! Check to leave loop
           if (.not.continueLoop) exit
           call stopTime(time,ss_MHD)               ! Get iteration time
           if (getPrintParams(ss_MHD)) then
             call estimateRemaining(time,ss_MHD)
             call print(time,'MHD solver')
           endif
           n_mhd = n_mhd + 1
           if (getPrintParams(ss_MHD)) then
             continueLoop = readSwitchFromFile(dir//'parameters/','killSwitch')
           endif
         enddo
         ! ***************************************************************
         ! ********** FINISHED SOLVING MHD EQUATIONS *********************
         ! ***************************************************************

         call stopTime(time,ss_MHD)
         call print(time,'MHD solver')
         call writeLastStepToFile(n_mhd,dir//'parameters/','n_mhd')
         call writeLastStepToFile(nrg%nstep,dir//'parameters/','n_nrg')
         call writeLastStepToFile(mom%nstep,dir//'parameters/','n_mom')
         call writeLastStepToFile(ind%nstep,dir//'parameters/','n_ind')

         call writeTime(time,dir,'MHD solver')

         ! **************** EXPORT ONE FINAL TIME ***********************
         if (solveMomentum)  call exportTransient(mom,ss_MHD,dir)
         if (solveInduction) call exportTransient(ind,ss_MHD)

         if (solveEnergy)    call exportRaw(nrg,nrg%m,dir)
         if (solveInduction) call exportRaw(ind,ind%m,dir)
         if (solveMomentum)  call exportRaw(mom,mom%m,F,dir)
         if (solveEnergy)    call export(nrg,nrg%m,dir)
         if (solveInduction) call export(ind,ind%m,dir)
         if (solveMomentum)  call export(mom,mom%m,dir)

         call delete(F)
         call computationComplete(time)
       end subroutine

       end module