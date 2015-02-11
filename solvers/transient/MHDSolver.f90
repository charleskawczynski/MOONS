       module MHDSolver_mod
       use simParams_mod
       use constants_mod
       use myAllocate_mod
       use myDebug_mod
       use scalarField_mod
       use vectorField_mod
       use myIO_mod
       use myTime_mod
       use griddata_mod
       use rundata_mod
       use myError_mod
       use vectorOps_mod

       use solverSettings_mod
       use BCs_mod

       ! adding documentation here...
       ! just added and committed to git
       ! But I made a change here
       ! One final difference
       use myPoisson_mod
       use momentumSolver_mod
       use inductionSolver_mod
       implicit none
       
       private
       public :: MHDSolver

       contains

       subroutine MHDSolver(mom,ind,gd,rd,ss_MHD,time,dir)
         ! MHDSolver solves for the primary variables in the momentum
         ! and induction equation with the prescribed inputs.
         ! Transient data:
         !        u(N_probe)
         !        div(u)
         !        Bx(N_probe)
         !        div(B)
         ! are exported with prescribed frequencies in the simParams
         ! file. The velocity is solved on the cell faces and B is
         ! solved on the cell corners.
         ! 
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
         real(dpn) :: Re,Ha,dtime
         integer :: n_ind,n_mhd
         ! **************************************************************
         logical :: continueLoop
         ! **************************************************************

         call computationInProgress(time)
         dtime = getDtime(rd)
         Ha = getHa(rd)
         Re = getRe(rd)

         ! ********************** PREP LOOP ******************************
         continueLoop = .true.
         n_mhd = getIteration(ss_MHD)

         if (restartB) then
         call readLastStepFromFile(n_ind,dir//'parameters/','n_ind')
         else; n_ind = 0
         endif
         call writeKillSwitchToFile(.true.,dir//'parameters/','killSwitch')

         ! ********** SET MOMENTUM SOURCE TERMS TO ZERO ******************
         ! call debug(1)
         ! mom%F = zero
         call assign(mom%F,zero)

         ! ********** SOLVE MHD EQUATIONS ********************************
         do while (continueLoop)

           call startTime(time)
           call setIteration(ss_MHD,n_mhd)

           ! ************** SOLVE MOMENTUM EQUATION **********************
           if (solveMomentum) then
             call solve(mom,gd,rd,ss_MHD)
           endif

           ! ********* EMBED VELOCITY / SOLVE INDUCTION EQUATION *********
           if (solveInduction) then
             call embedVelocity(mom%U,ind%U_cct,mom%temp,gd)
             call solve(ind,ind%U_cct,gd,rd,ss_MHD)
           endif

           ! ************************** COMPUTE DIVERGENCES *******************************
           if (getExportErrors(ss_MHD)) then
             call computeDivergence(mom,gd)
             call computeDivergence(ind,gd)
           endif

           ! ************************** COMPUTE J CROSS B *******************************
           if (solveInduction) then
             call computeCurrent(ind%J_cc,ind%B,ind%B0,ind%mu,gd)
             if (solveCoupled) then
               call computeJCrossB(mom%F,ind,gd,Re,Ha)
             endif
           else
             ! mom%F = zero
             call assign(mom%F,zero)
           endif

           ! ****************************** CHECK TO EXIT *********************************
           call checkCondition(ss_MHD,continueLoop)
           if (.not.continueLoop) then
             ! call debug(8)
             call stopTime(time,ss_MHD)
             call printGridData(gd)
             call printRunData(rd)
             call printTime(time,'MHD solver')
             exit
           endif
           ! ********************************** DISPLAY OUTPUT ****************************
           call stopTime(time,ss_MHD)
           if (getPrintParams(ss_MHD)) then
             ! call debug(9)
             write(*,'(A14,'//xyzfmt//')') ' maxval(u,v,w)',maxval(mom%U%x),maxval(mom%U%y),maxval(mom%U%z)
             write(*,'(A14,'//xyzfmt//')') ' minval(u,v,w)',minval(mom%U%x),minval(mom%U%y),minval(mom%U%z)
             if (solveInduction) then
               write(*,'(A17,'//xyzfmt//')') ' maxval(Bx,By,Bz)',maxval(ind%B%x),maxval(ind%B%y),maxval(ind%B%z)
               write(*,'(A17,'//xyzfmt//')') ' minval(Bx,By,Bz)',minval(ind%B%x),minval(ind%B%y),minval(ind%B%z)
             endif
             call estimateRemaining(time,ss_MHD)
             call printGridData(gd)
             call printRunData(rd)
             call printTime(time,'MHD solver')
           endif
           ! **************************** EXPORT TRANSIENT DATA *****************************
           call exportTransient(mom,gd,ss_MHD,dir)
           call exportTransient(ind,gd,ss_MHD,dir)
           n_mhd = n_mhd + 1
           if (solveInduction) n_ind = n_ind + 1
           ! ************************ READ KILL SWITCH FROM FILE ****************************
           ! call debug(10)
           if (getPrintParams(ss_MHD)) then
             call readKillSwitchFromFile(continueLoop,dir//'parameters/','killSwitch')
           endif

           ! if (getExportRawSolution(ss_MHD)) then
           !   call exportRaw(mom,gd,dir)
           !   call exportRaw(ind,gd,dir)
           ! endif
           ! if (getExportSolution(ss_MHD)) then
           !   call export(mom,gd,dir)
           !   call export(ind,gd,dir)
           ! endif

         enddo

         ! ************************ WRITE LAST STEP TO FILE *******************************
         call writeLastStepToFile(n_mhd,dir//'parameters/','n_mhd')
         call writeLastStepToFile(n_ind,dir//'parameters/','n_ind')

         ! **************************** EXPORT TRANSIENT DATA *****************************
         call exportTransient(mom,gd,ss_MHD,dir)
         call exportTransient(ind,gd,ss_MHD,dir)

         ! ********************* RECORD TIME ****************************
         call writeTime(time,dir,'MHD solver')

         ! **************** CALCULATE TOTAL MAGNETIC FIELD ************************
         ! ind%B = ind%B + ind%B0
         call add(ind%B,ind%B0)

         call exportRaw(mom,gd,dir)
         call exportRaw(ind,gd,dir)
         call export(mom,gd,dir)
         call export(ind,gd,dir)

         ! ****************** DEALLOCATE LOCALS *************************

         call computationComplete(time)
       end subroutine

       end module
