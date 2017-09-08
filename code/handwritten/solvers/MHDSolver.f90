       module MHDSolver_mod
       use current_precision_mod
       use sim_params_mod
       use var_set_mod
       use var_set_extend_mod
       use VF_extend_mod
       use IO_tools_mod
       use string_mod
       use path_extend_mod
       use dir_tree_mod
       use stop_clock_mod
       use stop_clock_extend_mod
       use export_frequency_mod
       use export_frequency_extend_mod
       use export_now_mod
       use export_safe_mod
       use refine_mesh_mod
       use kill_switch_mod
       use probe_extend_mod
       use RK_Params_mod
       use time_statistics_extend_mod

       use ops_embedExtract_mod
       use time_marching_params_mod
       use time_marching_params_extend_mod
       use add_all_energy_sources_mod
       use add_all_momentum_sources_mod
       use add_all_induction_sources_mod
       use induction_aux_mod
       use energy_mod
       use momentum_mod
       use induction_mod
       use momentum_sources_mod
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
         type(sim_params),intent(inout) :: SP
         type(time_marching_params),intent(inout) :: coupled
         type(stop_clock) :: sc
         type(export_frequency) :: EF
         type(export_now) :: EN
         type(export_safe) :: ES
         type(refine_mesh) :: RM
         type(kill_switch) :: KS
         logical :: refine_mesh_now_all
         integer :: i_RK

         refine_mesh_now_all = .false.

         call init(KS,str(DT%params),'kill_switch'); call export(KS)
         call init(EN,str(DT%export_now),'EN'); call export(EN)
         call init(RM,str(DT%refine_mesh),'RM'); call export(RM)
         call init(EF,SP%EF); call export(SP%EF)
         call init(ES,SP%SCP%export_safe_period)
         call init(sc,str(DT%wall_clock),'WALL_CLOCK_TIME_INFO')

         call export_ISP(SP%VS)
         call export_TMP(SP%VS)
         call export(coupled)

         if (SP%FCL%export_heavy) write(*,*) 'Working directory = ',str(DT%tar)

         write(*,*) '***************************************************************'
         write(*,*) '****************** ENTERING MAIN LOOP *************************'
         write(*,*) '***************************************************************'
         do while ((.not.KS%terminate_loop).and.(coupled%t.lt.coupled%t_final-coupled%dt*0.5_cp))
           call tic(sc)
           if (SP%FCL%print_every_MHD_step) write(*,*) 'coupled%n_step = ',coupled%n_step
           ! if (EF%info%export_now) call print(SP)

           do i_RK=1,coupled%RKP%n_stages
             call assign_RK_stage(SP%VS%T%TMP,i_RK)
             call assign_RK_stage(SP%VS%U%TMP,i_RK)
             call assign_RK_stage(SP%VS%B%TMP,i_RK)
             call assign_RK_stage(coupled,i_RK)
             call update(EF,coupled%n_step,i_RK.ne.coupled%RKP%n_stages)
             ! if (SP%VS%rho%SS%solve)    call solve(dens,mom%U,  EF,EN,DT)
             if (SP%VS%T%SS%solve) then
               call embedFace(nrg%U_F,mom%U,nrg%MD)
               call embedCC(nrg%U_CC,mom%U_CC,nrg%MD)
               call add_all_energy_sources(nrg%F,nrg%Fnm1,nrg%L,nrg,SP%VS%U%TMP,SP,ind)
               call solve(nrg,SP,nrg%F,nrg%Fnm1,nrg%L,SP%VS%T%TMP,EF)
             endif
             if (SP%VS%U%SS%solve) then
               call add_all_momentum_sources(mom%F,mom%Fnm1,mom%L,mom,SP%VS%U%TMP,SP,ind,nrg)
               call solve(mom,SP,mom%F,mom%Fnm1,mom%L,SP%VS%U%TMP,EF)
             endif
             if (SP%VS%B%SS%solve) then
               call add_all_induction_sources(ind%F,ind%Fnm1,ind%L,ind,SP%VS%B%TMP,SP,mom)
               call solve(ind,SP,ind%F,ind%Fnm1,ind%L,SP%VS%B%TMP,EF)
             endif
             call iterate_RK(SP%VS%T%TMP)
             call iterate_RK(SP%VS%U%TMP)
             call iterate_RK(SP%VS%B%TMP)
             call iterate_RK(coupled)
           enddo
           call update(EF,coupled%n_step,.false.)

           if (SP%VS%T%SS%solve) call iterate_step(SP%VS%T%TMP)
           if (SP%VS%U%SS%solve) call iterate_step(SP%VS%U%TMP)
           if (SP%VS%B%SS%solve) call iterate_step(SP%VS%B%TMP)
           call iterate_step(coupled)

           if (SP%VS%T%SS%solve) call export_unsteady(nrg,SP,SP%VS%T%TMP,EF,EN,DT)
           if (SP%VS%U%SS%solve) call export_unsteady(mom,SP,SP%VS%U%TMP,EF,EN,DT)
           if (SP%VS%B%SS%solve) call export_unsteady(ind,SP,SP%VS%B%TMP,EF,EN,DT)

           ! Statistics
           call update(mom%TS,mom%m,mom%U,SP%VS%U%TMP,mom%temp_F1,mom%temp_CC_VF,mom%TF_CC)

           if (EN%any_now) then
             call export_ISP(SP%VS)
             call export_TMP(SP%VS)
             call export(coupled)
           endif

           ! call import(SP%DP,str(DT%dimensionless_params),'dimensionless_params')
           call import_exit_criteria(mom%PCG_U%ISP)
           call import_exit_criteria(mom%PCG_P%ISP)
           call import_exit_criteria(ind%PCG_B%ISP)
           call import_exit_criteria(ind%PCG_cleanB%ISP)
           call import_exit_criteria(SP%VS)
           call import_TMP_dt(SP%VS)
           call import_dt(coupled)
           if (SP%SCP%couple_time_steps) call couple_time_step(SP%VS,coupled)

           ! call print(coupled%RKP,'coupled')
           call update(ES,sc%t_passed)

           call import(EN)
           call update(EN,ES%export_now)
           if (EN%any_next) call export(EN)

           call import(RM)
           call update(RM)
           if (RM%any_next) call export(RM)

           call toc(sc,coupled)
           if (EF%info%export_now) then
             ! oldest_modified_file violates intent, but this
             ! would be better to update outside the solvers,
             ! since it should be updated for all solver variables.
             ! call oldest_modified_file(DT%restart,DT%restart1,DT%restart2,'p.dat')
             if (SP%FCL%export_heavy) call print_light(sc,coupled)
             if (.not.SP%FCL%export_heavy) then
               write(*,*) ''
               call print_light(sc,coupled)
             endif
             call export(sc,coupled%t)
             if (SP%FCL%export_heavy) write(*,*) 'Working directory = ',str(DT%tar)
             call import(KS)
           endif
           ! call import(EF)
         enddo
         call print_light(sc,coupled)
         call export(sc,coupled)

         ! ***************************************************************
         ! ********** FINISHED SOLVING MHD EQUATIONS *********************
         ! ***************************************************************
         call export_ISP(SP%VS)
         call export_TMP(SP%VS)
         call export(coupled)

         ! **************** EXPORT ONE FINAL TIME ***********************
         if (SP%FCL%export_final_tec) then
         if (SP%VS%T%SS%initialize) call export_tec(nrg,SP,DT)
         if (SP%VS%U%SS%initialize) call export_tec(mom,SP,DT)
         if (SP%VS%B%SS%initialize) call export_tec(ind,SP,DT)
         endif

         if (SP%FCL%export_final_restart) then
         if (SP%VS%T%SS%initialize) call export(nrg,SP,DT)
         if (SP%VS%U%SS%initialize) call export(mom,SP,DT)
         if (SP%VS%B%SS%initialize) call export(ind,SP,DT)
         endif

         call delete(EF)
         call delete(sc)
         call delete(EN)
         call delete(ES)
         call delete(KS)
         call delete(RM)
       end subroutine

       end module