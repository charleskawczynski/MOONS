       module MHDSolver_mod
       use current_precision_mod
       use simParams_mod
       use VF_mod
       use IO_tools_mod
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

       subroutine MHDSolver(nrg,mom,ind,DT,n_dt_start,n_step,n_dt_stop)
         implicit none
         type(energy),intent(inout) :: nrg
         type(momentum),intent(inout) :: mom
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT ! Output directory
         integer,intent(in) :: n_dt_start,n_dt_stop
         integer,intent(inout) :: n_step
         type(stop_clock) :: sc
         type(VF) :: F ! Forces added to momentum equation
         integer :: un
         logical :: continueLoop
         type(print_export) :: PE

         call init(F,mom%U)
         call assign(F,0.0_cp)
         continueLoop = .true.

         call writeSwitchToFile(.true.,str(DT%params),'killSwitch')
         call writeSwitchToFile(.false.,str(DT%params),'exportNow')
         call writeSwitchToFile(.false.,str(DT%params),'exportNowU')
         call writeSwitchToFile(.false.,str(DT%params),'exportNowB')
         call writeSwitchToFile(.false.,str(DT%params),'exportNowT')

         write(*,*) 'Working directory = ',str(DT%tar)
         ! ***************************************************************
         ! ********** SOLVE MHD EQUATIONS ********************************
         ! ***************************************************************
         call init(sc,n_dt_stop-n_step)
         do n_step=n_step,n_dt_stop
           call init(PE,n_step,export_planar)

           call tic(sc)

           if (solveEnergy)    call solve(nrg,mom%U,  PE,DT)
           if (solveMomentum)  call solve(mom,F,      PE,DT)
           if (solveInduction) call solve(ind,mom%U_E,PE,DT)

           ! Q-2D implementation:
           ! call assign_negative(F%x,mom%U%x); call multiply(F%x,1.0_cp/(mom%Re/mom%Ha))
           ! call assign_negative(F%y,mom%U%y); call multiply(F%y,1.0_cp/(mom%Re/mom%Ha))
           ! call assign(F%z,0.0_cp)

           ! call assign(F,0.0_cp)

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
             ! oldest_modified_file violates intent, but this 
             ! would be better to update outside the solvers.
             ! call oldest_modified_file(DT%restart,DT%restart1,DT%restart2,'p.dat')
             call print(sc)
             continueLoop = readSwitchFromFile(str(DT%params),'killSwitch')
             call writeSwitchToFile(.false.,str(DT%params),'exportNow')
             write(*,*) 'Working directory = ',str(DT%tar)
             if (.not.continueLoop) then; call toc(sc); exit; endif
           endif
         enddo
         call print(sc)
         call export(sc,str(DT%LDC))
         ! ***************************************************************
         ! ********** FINISHED SOLVING MHD EQUATIONS *********************
         ! ***************************************************************
         call writeLastStepToFile(n_step,str(DT%params),'n_step')
         call writeLastStepToFile(nrg%nstep,str(DT%params),'nstep_nrg')
         call writeLastStepToFile(mom%nstep,str(DT%params),'nstep_mom')
         call writeLastStepToFile(ind%nstep,str(DT%params),'nstep_ind')

         un = newAndOpen(str(DT%restart),'n_dt_start,n_step,n_dt_stop')
         write(un,*) n_dt_start,n_step,n_dt_stop
         call closeAndMessage(un,str(DT%restart),'n_dt_start,n_step,n_dt_stop')

         ! **************** EXPORT ONE FINAL TIME ***********************
         if (solveMomentum)  call exportTransient(mom)
         if (solveInduction) call exportTransient(ind)

         if (solveEnergy) then;    call export_tec(nrg,DT);   ; endif ! call export(nrg,DT); endif
         if (solveInduction) then; call export_tec(ind,DT);   ; endif ! call export(ind,DT); endif
         if (solveMomentum) then;  call export_tec(mom,DT,F); ; endif ! call export(mom,DT); endif

         call delete(F)
       end subroutine

       end module