       module MHDSolver_mod
       use simParams_mod
       use scalarField_mod
       use vectorField_mod
       use IO_transientFields_mod
       use IO_auxiliary_mod
       use myTime_mod
       use griddata_mod
       use rundata_mod
       use myError_mod
       use probe_transient_mod
       use ops_discrete_mod
       use ops_physics_mod
       use ops_aux_mod

       use solverSettings_mod
       use BCs_mod

       use myPoisson_mod
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

       subroutine MHDSolver(mom,ind,gd,rd,ss_MHD,time,dir)
         ! MHDSolver solves for the primary variables in the momentum
         ! and induction equation with the prescribed inputs.
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(induction),intent(inout) :: ind
         type(griddata),intent(in) :: gd
         type(rundata),intent(in) :: rd
         type(solverSettings),intent(inout) :: ss_MHD
         type(myTime),intent(inout) :: time
         character(len=*),intent(in) :: dir ! Output directory
         ! *********************** LOCAL VARIABLES **********************
         real(cp) :: Ha
         integer :: n_mhd
         logical :: continueLoop,exportNow
         ! **************************************************************

         call computationInProgress(time)
         Ha = getHa(rd)

         ! ********************** PREP LOOP ******************************
         continueLoop = .true.
         exportNow = .false.
         n_mhd = getIteration(ss_MHD)

         call writeSwitchToFile(.true.,dir//'parameters/','killSwitch')
         call writeSwitchToFile(.false.,dir//'parameters/','exportNow')

         ! ********** SET MOMENTUM SOURCE TERMS TO ZERO ******************
         call assign(mom%F,zero)
         call assign(ind%U_cct,zero)

         ! ********** SOLVE MHD EQUATIONS ********************************
         do while (continueLoop)

           call startTime(time)
           call setIteration(ss_MHD,n_mhd)

           ! ************** SOLVE MOMENTUM EQUATION **********************
           if (solveMomentum) then
             call solve(mom,mom%g,ss_MHD)
           endif

           ! ********* EMBED VELOCITY / SOLVE INDUCTION EQUATION *********
           if (solveInduction) then
             ! ind%B0%x = real(exp(dble(-ind%omega*ind%t)),cp)
             ! ind%B0%y = real(exp(dble(-ind%omega*ind%t)),cp)
             ! ind%B0%z = real(1.0,cp)

             ind%B0%x = real(exp(dble(-ind%omega*ind%t)),cp)
             ind%B0%y = real(0.0,cp)
             ind%B0%z = real(0.0,cp)

             ! ind%B0%x = real(0.0,cp)
             ! ind%B0%y = real(0.0,cp)
             ! ind%B0%z = exp(-ind%omega*ind%t)
             ! call embedVelocity(ind%U_cct,mom%U,mom%temp_CC,mom%g)
             ! call embedVelocity(ind,mom%U,mom%g)
             call solve(ind,mom%U,mom%g,ind%g,ss_MHD)
             call computeKineticEnergy(mom,ind%U_cct,Nici1,Nici2,ss_MHD)
             call compute_stabilityParams(mom,ss_MHD)
             call computeCurrent(ind%J_cc,ind%B,ind%B0,ind%mu,ind%g)
             call computeMagneticEnergy(ind,ind%B,ind%B0,Nici1,Nici2,mom%g,ss_MHD)
           endif

           ! ************************** COMPUTE DIVERGENCES *******************************
           if (getExportErrors(ss_MHD)) then
             call computeDivergence(mom,mom%g)
             call computeDivergence(ind,ind%g)
             call exportTransientFull(mom,mom%g,dir)
             call exportTransientFull(ind,ind%g,dir)
           endif

           ! ************************** COMPUTE J CROSS B *******************************
           if (solveCoupled) then
             call computeJCrossB(mom%F,ind,mom%g,ind%g,mom%Re,Ha)
           else
             call assign(mom%F,zero)
           endif

           ! ****************************** CHECK TO EXIT *********************************
           call checkCondition(ss_MHD,continueLoop)
           if (.not.continueLoop) exit
           ! ********************************** DISPLAY OUTPUT ****************************
           call stopTime(time,ss_MHD)
           if (getPrintParams(ss_MHD)) then
             call printPhysicalMinMax(mom%U,'u','v','w')
             if (solveInduction) then
               call printPhysicalMinMax(ind%B,'Bx','By','Bz')
               ! call printPhysicalMinMax(ind%B0,'B0x','B0y','B0z')
             endif
             call printPhysicalMinMax(mom%divU%phi,mom%divU%s,'divU')
             call printPhysicalMinMax(mom%Re_grid%phi,mom%Re_grid%s,'Re_grid')
             if (solveInduction) then
               call printPhysicalMinMax(ind%divB%phi,ind%divB%s,'divB')
               call printPhysicalMinMax(ind%divJ%phi,ind%divJ%s,'divJ')
             endif
             write(*,*) ' Time = ',mom%t
             call estimateRemaining(time,ss_MHD)
             call printGridData(gd)
             call printRunData(rd)
             call print(time,'MHD solver')
           endif
           ! **************************** EXPORT TRANSIENT DATA *****************************
           call exportTransient(mom,ss_MHD)
           call exportTransient(ind,ss_MHD)
           n_mhd = n_mhd + 1
           ! ************************ READ SWITCHES FROM FILE *******************************

           if (getPrintParams(ss_MHD)) then
             call readSwitchFromFile(continueLoop,dir//'parameters/','killSwitch')
             call readSwitchFromFile(exportNow,dir//'parameters/','exportNow')
           endif

           if (getExportRawSolution(ss_MHD).or.exportNow) then
             call exportRaw(mom,mom%g,dir)
             call exportRaw(ind,ind%g,dir)
           endif
           if (getExportSolution(ss_MHD).or.exportNow) then
             call export(mom,mom%g,dir)
             call export(ind,ind%g,dir)
             call writeSwitchToFile(.false.,dir//'parameters/','exportNow')
           endif

         enddo

         call stopTime(time,ss_MHD)
         call printGridData(gd)
         call printRunData(rd)
         call print(time,'MHD solver')

         ! ************************ WRITE LAST STEP TO FILE *******************************
         call writeLastStepToFile(n_mhd,dir//'parameters/','n_mhd')
         call writeLastStepToFile(mom%nstep,dir//'parameters/','n_mom')
         call writeLastStepToFile(ind%nstep,dir//'parameters/','n_ind')
         ! call writeLastStepToFile(nrg%nstep,dir//'parameters/','n_nrg')

         ! **************************** EXPORT TRANSIENT DATA *****************************
         call writeSwitchToFile(.false.,dir//'parameters/','exportNow')
         call exportTransient(mom,ss_MHD)
         call exportTransient(ind,ss_MHD)

         ! ********************* RECORD TIME ****************************
         call writeTime(time,dir,'MHD solver')

         ! **************** EXPORT ONE FINAL TIME ***********************

         call exportRaw(mom,mom%g,dir)
         call exportRaw(ind,ind%g,dir)
         call export(mom,mom%g,dir)
         call export(ind,ind%g,dir)

         ! ****************** DEALLOCATE LOCALS *************************

         call computationComplete(time)
       end subroutine

       end module
