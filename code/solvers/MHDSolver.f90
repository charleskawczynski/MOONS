       module MHDSolver_mod
       use simParams_mod
       use VF_mod
       use IO_auxiliary_mod
       use stop_clock_mod

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

       subroutine MHDSolver(nrg,mom,ind,dir,N_timesteps)
         implicit none
         type(energy),intent(inout) :: nrg
         type(momentum),intent(inout) :: mom
         type(induction),intent(inout) :: ind
         character(len=*),intent(in) :: dir ! Output directory
         integer,intent(in) :: N_timesteps
         type(stop_clock) :: sc
         type(VF) :: F ! Forces added to momentum equation
         integer :: n_step,i
         logical :: continueLoop
         logical,dimension(6) :: print_export

         call init(F,mom%U)
         call assign(F,0.0_cp)
         continueLoop = .true.

         call writeSwitchToFile(.true.,dir//'parameters/','killSwitch')
         call writeSwitchToFile(.false.,dir//'parameters/','exportNowU')
         call writeSwitchToFile(.false.,dir//'parameters/','exportNowB')
         call writeSwitchToFile(.false.,dir//'parameters/','exportNowT')
         ! ***************************************************************
         ! ********** SOLVE MHD EQUATIONS ********************************
         ! ***************************************************************
         call init(sc,N_timesteps)
         do n_step=1,N_timesteps
           print_export = (/((mod(n_step,10**i).eq.1).and.(n_step.ne.1),i=1,6)/) ! .ne. 1 avoids initial export

           call tic(sc)
           if (solveEnergy)    call solve(nrg,mom%U,  print_export,dir)
           if (solveMomentum)  call solve(mom,F,      print_export,dir)
           if (solveInduction) call solve(ind,mom%U_E,print_export,dir)

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

           call toc(sc)
           if (print_export(1)) then
             call print(sc)
             continueLoop = readSwitchFromFile(dir//'parameters/','killSwitch')
             if (.not.continueLoop) then; call toc(sc); exit; endif
           endif
         enddo
         call print(sc)
         call export(sc,dir)
         ! ***************************************************************
         ! ********** FINISHED SOLVING MHD EQUATIONS *********************
         ! ***************************************************************
         call writeLastStepToFile(n_step,dir//'parameters/','n_step')
         call writeLastStepToFile(nrg%nstep,dir//'parameters/','n_nrg')
         call writeLastStepToFile(mom%nstep,dir//'parameters/','n_mom')
         call writeLastStepToFile(ind%nstep,dir//'parameters/','n_ind')

         ! **************** EXPORT ONE FINAL TIME ***********************
         if (solveMomentum)  call exportTransient(mom,dir)
         if (solveInduction) call exportTransient(ind)

         if (solveEnergy)    call export(nrg,nrg%m,dir)
         if (solveInduction) call export(ind,ind%m,dir)
         if (solveMomentum)  call export(mom,mom%m,F,dir)

         call delete(F)
       end subroutine

       end module