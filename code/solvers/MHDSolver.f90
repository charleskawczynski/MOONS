       module MHDSolver_mod
       use current_precision_mod
       use simParams_mod
       use VF_mod
       use IO_auxiliary_mod
       use string_mod
       use dir_tree_mod
       use stop_clock_mod

       use energy_mod
       use momentum_mod
       use induction_mod
       use energy_aux_mod
       use induction_aux_mod
       implicit none
       
       private
       public :: MHDSolver

       contains

       subroutine MHDSolver(nrg,mom,ind,DT,N_timesteps)
         implicit none
         type(energy),intent(inout) :: nrg
         type(momentum),intent(inout) :: mom
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT ! Output directory
         integer,intent(in) :: N_timesteps
         type(stop_clock) :: sc
         type(VF) :: F ! Forces added to momentum equation
         integer :: n_step,i
         logical :: continueLoop
         logical,dimension(6) :: print_export

         call init(F,mom%U)
         call assign(F,0.0_cp)
         continueLoop = .true.

         call writeSwitchToFile(.true.,str(DT%params),'killSwitch')
         call writeSwitchToFile(.false.,str(DT%params),'exportNow')
         call writeSwitchToFile(.false.,str(DT%params),'exportNowU')
         call writeSwitchToFile(.false.,str(DT%params),'exportNowB')
         call writeSwitchToFile(.false.,str(DT%params),'exportNowT')
         call writeIntegerToFile(mom%N_mom,str(DT%params),'N_mom')
         call writeIntegerToFile(mom%N_PPE,str(DT%params),'N_PPE')

         write(*,*) 'Working directory = ',str(DT%tar)
         ! ***************************************************************
         ! ********** SOLVE MHD EQUATIONS ********************************
         ! ***************************************************************
         call init(sc,N_timesteps)
         do n_step=1,N_timesteps
           if (n_step.lt.3) then; print_export(1:3) = .true.
           else; print_export = (/((mod(n_step,10**i).eq.1).and.(n_step.ne.1),i=1,6)/)
           endif

           call tic(sc)
           if (solveEnergy)    call solve(nrg,mom%U,  print_export,DT)
           if (solveMomentum)  call solve(mom,F,      print_export,DT)
           if (solveInduction) call solve(ind,mom%U_E,print_export,DT)

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
             continueLoop = readSwitchFromFile(str(DT%params),'killSwitch')
             write(*,*) 'Working directory = ',str(DT%tar)
             if (.not.continueLoop) then; call toc(sc); exit; endif
             call writeSwitchToFile(.false.,str(DT%params),'exportNow')
           endif
         enddo
         call print(sc)
         call export(sc,str(DT%root))
         ! ***************************************************************
         ! ********** FINISHED SOLVING MHD EQUATIONS *********************
         ! ***************************************************************
         call writeLastStepToFile(n_step,str(DT%params),'n_step')
         call writeLastStepToFile(nrg%nstep,str(DT%params),'nstep_nrg')
         call writeLastStepToFile(mom%nstep,str(DT%params),'nstep_mom')
         call writeLastStepToFile(ind%nstep,str(DT%params),'nstep_ind')

         ! **************** EXPORT ONE FINAL TIME ***********************
         if (solveMomentum)  call exportTransient(mom)
         if (solveInduction) call exportTransient(ind)

         if (solveEnergy)    call export(nrg,nrg%m,DT)
         if (solveInduction) call export(ind,ind%m,DT)
         call export(mom,mom%m,F,DT)

         call delete(F)
       end subroutine

       end module