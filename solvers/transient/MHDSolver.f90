       module MHDSolver_mod
       use simParams_mod
       use constants_mod
       use myDebug_mod
       use scalarField_mod
       use vectorField_mod
       use myIO_mod
       use myTime_mod
       use griddata_mod
       use rundata_mod
       use myError_mod
       use delOps_mod

       use solverSettings_mod
       use BCs_mod

       use myPoisson_mod
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
         real(cp) :: Ha,energy
         integer :: n_ind,n_mhd
         ! **************************************************************
         logical :: continueLoop
         ! **************************************************************

         call computationInProgress(time)
         Ha = getHa(rd)

         ! ********************** PREP LOOP ******************************
         continueLoop = .true.
         n_mhd = getIteration(ss_MHD)

         if (restartB) then
         call readLastStepFromFile(n_ind,dir//'parameters/','n_ind')
         else; n_ind = 0
         endif
         call writeKillSwitchToFile(.true.,dir//'parameters/','killSwitch')

         ! ********** SET MOMENTUM SOURCE TERMS TO ZERO ******************
         call assign(mom%F,zero)

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
             call embedVelocity(mom%U,ind%U_cct,mom%temp,mom%g)
             call solve(ind,ind%U_cct,ind%g,ss_MHD)
             if (computeKU.and.getExportTransient(ss_MHD).or.n_mhd.eq.0) then
              call totalEnergy(energy,&
                ind%U_cct%x(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
                ind%U_cct%y(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
                ind%U_cct%z(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
                mom%g)
              call writeTransientToFile(n_mhd,energy,dir//'Ufield\','KU',n_mhd.eq.0)
             endif
           endif

           ! ************************** COMPUTE DIVERGENCES *******************************
           if (getExportErrors(ss_MHD)) then
             call computeDivergence(mom,mom%g)
             call computeDivergence(ind,ind%g)
           endif

           ! ************************** COMPUTE J CROSS B *******************************
           if (solveInduction) then
             call computeCurrent(ind%J_cc,ind%B,ind%B0,ind%mu,ind%g)
             if (solveCoupled) then
               call computeJCrossB(mom%F,ind,mom%g,ind%g,mom%Re,Ha)
             ! ind%B0%x = exp(-ind%omega*ind%t)
             ! ind%B0%y = exp(-ind%omega*ind%t)
             ! ind%B0%z = real(1.0,cp)
             else; call assign(mom%F,zero)
             endif
             if (computeKB.and.getExportTransient(ss_MHD).or.n_mhd.eq.0) then
              call totalEnergy(energy,ind%B%x,ind%B%y,ind%B%z,ind%g)
              call writeTransientToFile(n_mhd,energy,dir//'Bfield\','KB',n_mhd.eq.0)
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
             call print(time,'MHD solver')
             exit
           endif
           ! ********************************** DISPLAY OUTPUT ****************************
           call stopTime(time,ss_MHD)
           if (getPrintParams(ss_MHD)) then
             ! call debug(9)
             ! write(*,'(A14,'//xyzfmt//')') ' maxval(u,v,w)',maxval(mom%U%x),maxval(mom%U%y),maxval(mom%U%z)
             ! write(*,'(A14,'//xyzfmt//')') ' minval(u,v,w)',minval(mom%U%x),minval(mom%U%y),minval(mom%U%z)
             write(*,*) ' maxval(u,v,w)',maxval(mom%U%x),maxval(mom%U%y),maxval(mom%U%z)
             write(*,*) ' minval(u,v,w)',minval(mom%U%x),minval(mom%U%y),minval(mom%U%z)
             if (solveInduction) then
               ! write(*,'(A17,'//xyzfmt//')') ' maxval(Bx,By,Bz)',maxval(ind%B%x),maxval(ind%B%y),maxval(ind%B%z)
               ! write(*,'(A17,'//xyzfmt//')') ' minval(Bx,By,Bz)',minval(ind%B%x),minval(ind%B%y),minval(ind%B%z)
               write(*,*) ' maxval(Bx,By,Bz)',maxval(ind%B%x),maxval(ind%B%y),maxval(ind%B%z)
               write(*,*) ' minval(Bx,By,Bz)',minval(ind%B%x),minval(ind%B%y),minval(ind%B%z)
               ! write(*,*) ' maxval(B0x,B0y,B0z)',maxval(ind%B0%x),maxval(ind%B0%y),maxval(ind%B0%z)
               ! write(*,*) ' minval(B0x,B0y,B0z)',minval(ind%B0%x),minval(ind%B0%y),minval(ind%B0%z)
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
           if (solveInduction) n_ind = n_ind + 1
           ! ************************ READ KILL SWITCH FROM FILE ****************************

           if (getPrintParams(ss_MHD)) then
             call readKillSwitchFromFile(continueLoop,dir//'parameters/','killSwitch')
           endif

           if (getExportRawSolution(ss_MHD)) then
             call exportRaw(mom,mom%g,dir)
             call exportRaw(ind,ind%g,dir)
           endif
           if (getExportSolution(ss_MHD)) then
             call export(mom,mom%g,dir)
             call export(ind,ind%g,dir)
           endif

         enddo

         ! ************************ WRITE LAST STEP TO FILE *******************************
         call writeLastStepToFile(n_mhd,dir//'parameters/','n_mhd')
         call writeLastStepToFile(n_ind,dir//'parameters/','n_ind')

         ! **************************** EXPORT TRANSIENT DATA *****************************
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
