       module MHDSolver_mod
       use current_precision_mod
       use sim_params_mod
       use var_set_mod
       use VF_mod
       use IO_tools_mod
       use string_mod
       use path_mod
       use dir_tree_mod
       use stop_clock_mod
       use export_frequency_mod
       use export_now_mod
       use export_safe_mod
       use refine_mesh_mod
       use kill_switch_mod
       use probe_mod

       use ops_embedExtract_mod
       use time_marching_params_mod
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

         refine_mesh_now_all = .false.

         call init(KS,str(DT%params),'kill_switch'); call export(KS)
         call init(EN,str(DT%export_now),'EN'); call export(EN)
         call init(RM,str(DT%refine_mesh),'RM'); call export(RM)
         call init(EF,SP%EF); call export(SP%EF)
         call init(ES,SP%export_safe_period)
         call init(sc,coupled%n_step_stop-coupled%n_step,str(DT%wall_clock),'WALL_CLOCK_TIME_INFO')

         if (SP%export_heavy) write(*,*) 'Working directory = ',str(DT%tar)

         write(*,*) '***************************************************************'
         write(*,*) '****************** ENTERING MAIN LOOP *************************'
         write(*,*) '***************************************************************'
         do while ((.not.KS%terminate_loop).and.(coupled%n_step.lt.coupled%n_step_stop))
           call tic(sc)
           call update(EF,coupled%n_step)
           if (SP%print_every_MHD_step) write(*,*) 'coupled%n_step = ',coupled%n_step

           ! if (SP%VS%rho%SS%solve)    call solve(dens,mom%U,  EF,EN,DT)
           if (SP%VS%T%SS%solve) then
             call embedFace(nrg%U_F,mom%U,nrg%MD)
             call embedCC(nrg%U_CC,mom%U_CC,nrg%MD)
             call add_all_energy_sources(nrg%F,nrg%Fnm1,nrg,ind,SP)
             call solve(nrg,SP,nrg%F,nrg%Fnm1,SP%VS%T%TMP,EF,EN,DT)
           endif
           if (SP%VS%U%SS%solve) then
             call add_all_momentum_sources(mom%F,mom%Fnm1,nrg,mom,ind,SP)
             call solve(mom,SP,mom%F,mom%Fnm1,SP%VS%U%TMP,EF,EN,DT)
           endif
           if (SP%VS%B%SS%solve) then
            call embedVelocity_E(ind%U_E,mom%U_E,ind%MD_fluid)
            call add_all_induction_sources(ind%F,ind%Fnm1,mom,ind,SP%VS%B%TMP,SP)
            call solve(ind,SP,ind%F,ind%Fnm1,SP%VS%B%TMP ,EF,EN,DT)
           endif

           if (EN%any_now) then;
             call export_TMP(SP%VS); call export(coupled,str(DT%params))
           endif

           call iterate_step(coupled)
           call update(ES,sc%t_passed)

           call import(EN)
           call update(EN,ES%export_now)
           if (EN%any_next) call export(EN)

           call import(RM)
           call update(RM)
           if (RM%any_next) call export(RM)

           call toc(sc)
           if (EF%info%export_now) then
             ! oldest_modified_file violates intent, but this
             ! would be better to update outside the solvers,
             ! since it should be updated for all solver variables.
             ! call oldest_modified_file(DT%restart,DT%restart1,DT%restart2,'p.dat')
             if (SP%export_heavy) call print(sc,coupled)
             if (.not.SP%export_heavy) then
               write(*,*) ''
               call print_light(sc,coupled)
             endif
             call export(sc,coupled%t)
             if (SP%export_heavy) write(*,*) 'Working directory = ',str(DT%tar)
             call import(KS)
           endif
           ! call import(EF)
         enddo
         call print(sc,coupled)
         call export(sc,coupled)

         ! ***************************************************************
         ! ********** FINISHED SOLVING MHD EQUATIONS *********************
         ! ***************************************************************
         call export_TMP(SP%VS)
         call export(coupled,str(DT%params))

         ! **************** EXPORT ONE FINAL TIME ***********************
         if (SP%VS%T%SS%initialize) call export_tec(nrg,SP,DT)
         if (SP%VS%U%SS%initialize) call export_tec(mom,SP,DT)
         if (SP%VS%B%SS%initialize) call export_tec(ind,SP,DT)

         call delete(EF)
         call delete(sc)
         call delete(EN)
         call delete(ES)
         call delete(KS)
         call delete(RM)
       end subroutine

       end module