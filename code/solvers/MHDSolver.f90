       module MHDSolver_mod
       use current_precision_mod
       use sim_params_mod
       use VF_mod
       use IO_tools_mod
       use string_mod
       use path_mod
       use dir_tree_mod
       use stop_clock_mod
       use print_export_mod
       use export_now_mod
       use refine_mesh_mod
       use kill_switch_mod

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
         type(print_export) :: PE
         type(export_now) :: EN
         type(refine_mesh) :: RM
         type(kill_switch) :: KS

         call init(F,mom%U)
         call assign(F,0.0_cp)

         call init(KS,str(DT%params),'kill_switch'); call export(KS)
         call init(EN,str(DT%export_now),'EN'); call export(EN)
         call init(RM,str(DT%refine_mesh),'RM'); call export(RM)
         call init(PE,SP%export_planar,str(DT%PE),'PE'); call export(PE)
         call init(sc,coupled%n_step_stop-coupled%n_step,str(DT%wall_clock),'WALL_CLOCK_TIME_INFO')

         write(*,*) 'Working directory = ',str(DT%tar)

         write(*,*) '***************************************************************'
         write(*,*) '****************** ENTERING MAIN LOOP *************************'
         write(*,*) '***************************************************************'
         do while ((.not.KS%terminate_loop).and.(coupled%n_step.lt.coupled%n_step_stop))
           call tic(sc)
           call update(PE,coupled%n_step)

           ! if (SP%solveDensity)    call solve(dens,mom%U,  PE,EN,DT)
           if (SP%solveEnergy)    call solve(nrg,mom%U,  PE,EN,DT)
           if (SP%solveMomentum)  call solve(mom,F,      PE,EN,DT)
           if (SP%solveInduction) call solve(ind,mom%U_E,PE,EN,DT)

           if (RM%any_next) then
             call prolongate(RM)
             ! call prolongate(nrg,DT,RM,PE)
             call prolongate(mom,F,DT,RM,PE)
             ! call prolongate(ind,DT,RM,PE)

             ! call prolongate(nrg%TMP)
             call prolongate(mom%TMP)
             ! call prolongate(ind%TMP)
             call prolongate(coupled)
             call reset_Nmax(sc,coupled%n_step_stop-coupled%n_step)
           endif

           call assign(F,0.0_cp) ! DO NOT REMOVE THIS, FOLLOW THE COMPUTE_ADD PROCEDURE BELOW

           if (SP%addJCrossB) then
             call compute_AddJCrossB(F,ind%B,ind%B0,ind%J,ind%m,&
                                     ind%MD_fluid,mom%Ha,mom%Re,&
                                     ind%finite_Rem,ind%temp_CC_SF,&
                                     ind%temp_F1,ind%temp_F1_TF,&
                                     ind%temp_F2_TF,mom%temp_F)
           endif
           if (SP%add_Q2D_JCrossB) then
             call compute_add_Q2D_JCrossB(F,mom%U,mom%Ha,mom%Re,mom%temp_F)
           endif
           if (SP%addBuoyancy) then
             call compute_AddBuoyancy(F,nrg%T,nrg%gravity,&
                                      mom%Gr,mom%Re,nrg%m,nrg%MD,&
                                      nrg%temp_F,nrg%temp_CC1_VF,mom%temp_F)
           endif
           if (SP%addGravity) then
             call compute_AddGravity(F,nrg%gravity,mom%Fr,nrg%m,&
                                     nrg%MD,nrg%temp_F,nrg%temp_CC1_VF,&
                                     mom%temp_F)
           endif

           call iterate_step(coupled)
           call import(PE)

           call import(EN)
           call update(EN)
           if (EN%any_next) call export(EN)

           call import(RM)
           call update(RM)
           if (RM%any_next) call export(RM)

           call toc(sc)
           if (PE%info) then
             ! oldest_modified_file violates intent, but this
             ! would be better to update outside the solvers,
             ! since it should be updated for all solver variables.
             ! call oldest_modified_file(DT%restart,DT%restart1,DT%restart2,'p.dat')
             call print(sc)
             call export(sc,coupled%t)
             if (SP%solveMomentum) then
               call import(mom%ISP_U); call init(mom%PCG_U%ISP,mom%ISP_U)
               call import(mom%ISP_P); call init(mom%PCG_P%ISP,mom%ISP_P)
             endif
             if (SP%solveInduction) then
               call import(ind%ISP_B);   call init(ind%PCG_B%ISP,ind%ISP_B)
               call import(ind%ISP_phi); call init(ind%PCG_cleanB%ISP,ind%ISP_phi)
             endif
             if (SP%solveEnergy) then
               call import(nrg%ISP_T); call init(nrg%PCG_T%ISP,nrg%ISP_T)
             endif
             ! call import(coupled)
             ! if (SP%coupled_time_step) then
             !   call couple_time_step(nrg%TMP,coupled)
             !   call couple_time_step(mom%TMP,coupled)
             !   call couple_time_step(ind%TMP,coupled)
             ! endif

             write(*,*) 'Working directory = ',str(DT%tar)
             call import(KS)
           endif
         enddo
         call print(sc)
         call export(sc)

         ! ***************************************************************
         ! ********** FINISHED SOLVING MHD EQUATIONS *********************
         ! ***************************************************************
         call export(nrg%TMP)
         call export(mom%TMP)
         call export(ind%TMP)
         call export(coupled)

         ! **************** EXPORT ONE FINAL TIME ***********************
         if (SP%solveMomentum)  call export_transient(mom)
         if (SP%solveInduction) call export_transient(ind)

         if (SP%solveEnergy) then;    call export_tec(nrg,DT);   endif ! call export(nrg,DT); endif
         if (SP%solveMomentum) then;  call export_tec(mom,DT,F); endif ! call export(mom,DT); endif
         if (SP%solveInduction) then; call export_tec(ind,DT);   endif ! call export(ind,DT); endif

         call delete(PE)
         call delete(sc)
         call delete(EN)
         call delete(KS)
         call delete(F)
       end subroutine

       end module