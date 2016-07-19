       module MHDSolver_mod
       use current_precision_mod
       use simParams_mod
       use VF_mod
       use IO_auxiliary_mod
       use string_mod
       use path_mod
       use dir_tree_mod
       use stop_clock_mod
       use print_export_mod

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
         integer :: n_step
         ! integer :: i
         logical :: continueLoop
         type(print_export) :: PE

         call init(F,mom%U)
         call assign(F,0.0_cp)
         continueLoop = .true.
         ! tau_inv = 1.0_cp

         call writeSwitchToFile(.true.,str(DT%params),'killSwitch')
         call writeSwitchToFile(.false.,str(DT%params),'exportNow')
         call writeSwitchToFile(.false.,str(DT%params),'exportNowU')
         call writeSwitchToFile(.false.,str(DT%params),'exportNowB')
         call writeSwitchToFile(.false.,str(DT%params),'exportNowT')

         write(*,*) 'Working directory = ',str(DT%tar)
         ! ***************************************************************
         ! ********** SOLVE MHD EQUATIONS ********************************
         ! ***************************************************************
         call init(sc,N_timesteps)
         do n_step=1,N_timesteps
           call init(PE,n_step)

           call tic(sc)

           ! call assign(F,mom%U)               ! For Q2D model
           ! call multiply(F,-1.0_cp/100.05_cp) ! For Q2D model

           ! CFL condition:
           ! mom%dTime = 0.3_cp*minval((/(mom%m%dhmin(i),i=1,3)/))/maxval((/max(mom%U),0.01_cp/))

           ! mom%dTime = 0.3_cp*(mom%m%dhmin(1)/max((/mom%U%x,1.0_cp/)) +&
           !                     mom%m%dhmin(2)/max((/mom%U%y,1.0_cp/)) +&
           !                     mom%m%dhmin(3)/max((/mom%U%z,1.0_cp/)) )
           ! nrg%dTime = mom%dTime

           if (solveEnergy)    call solve(nrg,mom%U,  PE,DT)
           if (solveMomentum)  call solve(mom,F,      PE,DT)
           if (solveInduction) call solve(ind,mom%U_E,PE,DT)

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
                                      nrg%temp_F,nrg%temp_CC1_VF,mom%temp_F)
           endif
           if (addGravity) then
             call compute_AddGravity(F,nrg%gravity,mom%Fr,nrg%m,&
                                     nrg%D,nrg%temp_F,nrg%temp_CC1_VF,&
                                     mom%temp_F)
           endif

           call toc(sc)
           if (PE%info) then
             call print(sc)
             continueLoop = readSwitchFromFile(str(DT%params),'killSwitch')
             write(*,*) 'Working directory = ',str(DT%tar)
             if (.not.continueLoop) then; call toc(sc); exit; endif
             call writeSwitchToFile(.false.,str(DT%params),'exportNow')
           endif
         enddo
         call print(sc)
         call export(sc,str(DT%out_dir))
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