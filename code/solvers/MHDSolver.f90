       module MHDSolver_mod
       use simParams_mod
       use VF_mod
       use IO_auxiliary_mod
       use myTime_mod
       use solverSettings_mod

       use energy_mod
       use momentum_mod
       use induction_mod
       use energy_aux_mod
       use induction_aux_mod
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
           if (solveEnergy)    call solve(nrg,mom%U  ,ss_MHD,dir)
           if (solveMomentum)  call solve(mom,F,ss_MHD,dir)
           if (solveInduction) call solve(ind,mom%U_E,ss_MHD,dir)

           call assign(F,0.0_cp)
           if (addJCrossB) then
             call compute_AddJCrossB(F,ind%B,ind%B0,ind%J,ind%m,&
                                     ind%D_fluid,mom%Ha,mom%Re,&
                                     ind%finite_Rem,ind%temp_CC_SF,&
                                     ind%temp_F1,ind%temp_F1_TF,&
                                     ind%temp_F2_TF,mom%temp_F)
           endif
           if (addBuoyancy) then
             call compute_AddBuoyancy(F,nrg%T,nrg%gravity,&
                                      mom%Gr,mom%Re,nrg%m,nrg%D,&
                                      nrg%temp_F,nrg%temp_CC1_VF)
           endif
           if (addGravity) then
             call compute_AddGravity(F,nrg%gravity,mom%Fr,nrg%m,&
                                     nrg%D,nrg%temp_F,nrg%temp_CC1_VF,&
                                     nrg%temp_CC2_VF)
           endif

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

         if (solveEnergy)    call export(nrg,nrg%m,dir)
         if (solveInduction) call export(ind,ind%m,dir)
         if (solveMomentum)  call export(mom,mom%m,F,dir)

         call delete(F)
         call computationComplete(time)
       end subroutine

       end module