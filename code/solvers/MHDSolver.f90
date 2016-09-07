       module MHDSolver_mod
       use current_precision_mod
       use sim_params_mod
       use VF_mod
       use IO_tools_mod
       use IO_auxiliary_mod
       use string_mod
       use path_mod
       use dir_tree_mod
       use stop_clock_mod
       use print_export_mod
       use export_now_mod

       use time_marching_params_mod
       use energy_mod
       use momentum_mod
       use induction_mod
       use energy_aux_mod
       use induction_aux_mod
       implicit none
       
       private
       public :: MHDSolver

       contains

       subroutine MHDSolver(nrg,mom,ind,DT,SP,coupled)
         implicit none
         type(energy),intent(inout) :: nrg
         type(momentum),intent(inout) :: mom
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(inout) :: coupled
         type(stop_clock) :: sc
         type(VF) :: F ! Forces added to momentum equation
         integer :: n_step
         logical :: continueLoop
         type(print_export) :: PE
         type(export_now) :: EN

         call init(F,mom%U)
         call assign(F,0.0_cp)
         continueLoop = .true.

         call writeSwitchToFile(.true.,str(DT%params),'killSwitch')

         call init(EN,str(DT%export_now),'EN'); call export(EN)
         call init(PE,SP%export_planar,str(DT%PE),'PE')
         call init(sc,coupled%n_step_stop-coupled%n_step,str(DT%wall_clock),'WALL_CLOCK_TIME_INFO')

         write(*,*) 'Working directory = ',str(DT%tar)
         ! ***************************************************************
         ! ********** SOLVE MHD EQUATIONS ********************************
         ! ***************************************************************
         do n_step = coupled%n_step,coupled%n_step_stop

           call tic(sc)
           call update(PE,coupled%n_step)

           if (SP%solveEnergy)    call solve(nrg,mom%U,  PE,EN,DT)
           if (SP%solveMomentum)  call solve(mom,F,      PE,EN,DT)
           if (SP%solveInduction) call solve(ind,mom%U_E,PE,EN,DT)

           call assign(F,0.0_cp) ! DO NOT REMOVE THIS, FOLLOW THE COMPUTE_ADD PROCEDURE BELOW

           if (SP%addJCrossB) then
             call compute_AddJCrossB(F,ind%B,ind%B0,ind%J,ind%m,&
                                     ind%D_fluid,mom%Ha,mom%Re,&
                                     ind%finite_Rem,ind%temp_CC_SF,&
                                     ind%temp_F1,ind%temp_F1_TF,&
                                     ind%temp_F2_TF,mom%temp_F)
           endif
           if (SP%add_Q2D_JCrossB) then
             call compute_add_Q2D_JCrossB(F,mom%U,mom%Ha,mom%Re,mom%temp_F)
           endif
           if (SP%addBuoyancy) then
             call compute_AddBuoyancy(F,nrg%T,nrg%gravity,&
                                      mom%Gr,mom%Re,nrg%m,nrg%D,&
                                      nrg%temp_F,nrg%temp_CC1_VF,mom%temp_F)
           endif
           if (SP%addGravity) then
             call compute_AddGravity(F,nrg%gravity,mom%Fr,nrg%m,&
                                     nrg%D,nrg%temp_F,nrg%temp_CC1_VF,&
                                     mom%temp_F)
           endif

           call iterate_step(coupled)
           call import(EN); call update(EN); call export(EN)

           call toc(sc)
           if (PE%info) then
             ! oldest_modified_file violates intent, but this 
             ! would be better to update outside the solvers, 
             ! since it should be updated for all solver variables.
             ! call oldest_modified_file(DT%restart,DT%restart1,DT%restart2,'p.dat')
             call print(sc)
             call export(sc,coupled%t)
             if (SP%solveMomentum) then
               call import(mom%ISP_U)
               call import(mom%ISP_P); call init(mom%PCG_P%ISP,mom%ISP_P)
             endif
             if (SP%solveInduction) then
               call import(ind%ISP_B);   call init(ind%PCG_B%ISP,ind%ISP_B)
               call import(ind%ISP_phi); call init(ind%PCG_cleanB%ISP,ind%ISP_phi)
             endif
             if (SP%solveEnergy) then; call import(nrg%ISP_T)
                                       call init(nrg%PCG_T%ISP,nrg%ISP_T)
             endif

             continueLoop = readSwitchFromFile(str(DT%params),'killSwitch')
             write(*,*) 'Working directory = ',str(DT%tar)
             if (.not.continueLoop) then; call toc(sc); exit; endif
           endif
         enddo

         call print(sc)
         call export(sc)
         call delete(PE)
         call delete(EN)
         ! ***************************************************************
         ! ********** FINISHED SOLVING MHD EQUATIONS *********************
         ! ***************************************************************
         call export(nrg%TMP)
         call export(mom%TMP)
         call export(ind%TMP)
         call export(coupled)

         ! **************** EXPORT ONE FINAL TIME ***********************
         if (SP%solveMomentum)  call exportTransient(mom)
         if (SP%solveInduction) call exportTransient(ind)

         if (SP%solveEnergy) then;    call export_tec(nrg,DT);   endif ! call export(nrg,DT); endif
         if (SP%solveInduction) then; call export_tec(ind,DT);   endif ! call export(ind,DT); endif
         if (SP%solveMomentum) then;  call export_tec(mom,DT,F); endif ! call export(mom,DT); endif

         call delete(F)
       end subroutine

       end module