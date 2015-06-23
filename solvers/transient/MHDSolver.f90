       module MHDSolver_mod
       use simParams_mod
       use scalarField_mod
       use vectorField_mod
       use IO_transientFields_mod
       use IO_auxiliary_mod
       use myTime_mod
       use griddata_mod
       use rundata_mod
       use norms_mod
       use ops_discrete_mod
       use ops_physics_mod
       use ops_aux_mod

       use solverSettings_mod
       use BCs_mod

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

       real(cp),parameter :: zero = real(0.0,cp)

       private
       public :: MHDSolver

       contains

       subroutine MHDSolver(nrg,mom,ind,gd,rd,ss_MHD,time,dir)
         ! MHDSolver solves for the primary variables in the momentum
         ! and induction equation with the prescribed inputs.
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(energy),intent(inout) :: nrg
         type(momentum),intent(inout) :: mom
         type(induction),intent(inout) :: ind
         type(griddata),intent(in) :: gd
         type(rundata),intent(in) :: rd
         type(solverSettings),intent(inout) :: ss_MHD
         type(myTime),intent(inout) :: time
         character(len=*),intent(in) :: dir ! Output directory
         ! *********************** LOCAL VARIABLES **********************
         type(vectorField) :: F ! Forces added to momentum equation
         integer :: n_mhd
         logical :: continueLoop,exportNow
         ! **************************************************************

         call computationInProgress(time)

         call allocateVectorField(F,mom%U)
         call assign(F,real(0.0,cp))

         ! ********************** PREP LOOP ******************************
         continueLoop = .true.
         exportNow = .false.
         n_mhd = getIteration(ss_MHD)

         call writeSwitchToFile(.true.,dir//'parameters/','killSwitch')
         call writeSwitchToFile(.false.,dir//'parameters/','exportNow')

         ! ********** SET MOMENTUM SOURCE TERMS TO ZERO ******************

         ! ********** SOLVE MHD EQUATIONS ********************************
         do while (continueLoop)

           call startTime(time)
           call setIteration(ss_MHD,n_mhd)

           if (solveEnergy)    call solve(nrg,mom%U,mom%g,ss_MHD,dir)
           if (solveMomentum)  call solve(mom,F,ss_MHD,dir)
           if (solveInduction) call solve(ind,mom%U,mom%g,ss_MHD,dir)

           call assign(F,real(0.0,cp))
           if (addJCrossB) call computeAddJCrossB(F,ind,mom%g,ind%Ha,mom%Re,ind%Rem)
           if (addBuoyancy) call computeAddBuoyancy(F,nrg,mom%g,mom%Gr,mom%Re,mom%Fr)

           ! ****************************** CHECK TO EXIT *********************************
           call checkCondition(ss_MHD,continueLoop)
           if (.not.continueLoop) exit
           ! ********************************** DISPLAY OUTPUT ****************************
           call stopTime(time,ss_MHD)
           if (getPrintParams(ss_MHD)) then
             call estimateRemaining(time,ss_MHD)
             ! call printGridData(gd)
             ! call printRunData(rd)
             call print(time,'MHD solver')
           endif
           ! **************************** EXPORT TRANSIENT DATA *****************************
           n_mhd = n_mhd + 1
           ! ************************ READ SWITCHES FROM FILE *******************************

           if (getPrintParams(ss_MHD)) then
             call readSwitchFromFile(continueLoop,dir//'parameters/','killSwitch')
             call readSwitchFromFile(exportNow,dir//'parameters/','exportNow')
           endif

           if (getExportRawSolution(ss_MHD).or.exportNow) then
             ! call exportRaw(mom,mom%g,dir)
             ! call exportRaw(ind,ind%g,dir)
           endif
           if (getExportSolution(ss_MHD).or.exportNow) then
             ! call export(mom,mom%g,dir)
             ! call export(ind,ind%g,dir)
             call writeSwitchToFile(.false.,dir//'parameters/','exportNow')
           endif

         enddo
         call stopTime(time,ss_MHD)
         ! call printGridData(gd)
         ! call printRunData(rd)
         call print(time,'MHD solver')
         ! ************************ WRITE LAST STEP TO FILE *******************************
         call writeLastStepToFile(n_mhd,dir//'parameters/','n_mhd')
         call writeLastStepToFile(nrg%nstep,dir//'parameters/','n_nrg')
         call writeLastStepToFile(mom%nstep,dir//'parameters/','n_mom')
         call writeLastStepToFile(ind%nstep,dir//'parameters/','n_ind')

         ! **************************** EXPORT TRANSIENT DATA *****************************
         call writeSwitchToFile(.false.,dir//'parameters/','exportNow')
         if (solveMomentum) call exportTransient(mom,ss_MHD,dir)
         if (solveInduction) call exportTransient(ind,ss_MHD)

         ! ********************* RECORD TIME ****************************
         call writeTime(time,dir,'MHD solver')

         ! **************** EXPORT ONE FINAL TIME ***********************

         if (solveMomentum) call exportRaw(mom,mom%g,dir)
         if (solveInduction) call exportRaw(ind,ind%g,dir)
         if (solveEnergy) call exportRaw(nrg,nrg%g,dir)
         if (solveMomentum) call export(mom,mom%g,dir)
         if (solveInduction) call export(ind,ind%g,dir)
         if (solveEnergy) call export(nrg,nrg%g,dir)

         call delete(F)

         ! ****************** DEALLOCATE LOCALS *************************

         call computationComplete(time)
       end subroutine

       end module
