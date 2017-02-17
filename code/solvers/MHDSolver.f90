       module MHDSolver_mod
       use current_precision_mod
       use sim_params_mod
       use VF_mod
       use IO_tools_mod
       use string_mod
       use path_mod
       use dir_tree_mod
       use stop_clock_mod
       use export_frequency_mod
       use export_now_mod
       use refine_mesh_mod
       use kill_switch_mod
       use probe_mod

       use time_marching_params_mod
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
         type(VF) :: F,Fnm1 ! Forces added to momentum equation
         type(export_frequency) :: EF
         type(export_now) :: EN
         type(refine_mesh) :: RM
         type(kill_switch) :: KS
         logical :: refine_mesh_now_all

         call init(F,mom%U)
         call init(Fnm1,mom%U)
         call assign(F,0.0_cp)
         call assign(Fnm1,0.0_cp)
         refine_mesh_now_all = .false.

         call init(KS,str(DT%params),'kill_switch'); call export(KS)
         call init(EN,str(DT%export_now),'EN'); call export(EN)
         call init(RM,str(DT%refine_mesh),'RM'); call export(RM)
         call init(EF,SP%EF); call export(SP%EF)
         call init(sc,coupled%n_step_stop-coupled%n_step,str(DT%wall_clock),'WALL_CLOCK_TIME_INFO')

         write(*,*) 'Working directory = ',str(DT%tar)

         write(*,*) '***************************************************************'
         write(*,*) '****************** ENTERING MAIN LOOP *************************'
         write(*,*) '***************************************************************'
         do while ((.not.KS%terminate_loop).and.(coupled%n_step.lt.coupled%n_step_stop))
           call tic(sc)
           call update(EF,coupled%n_step)
           if (SP%print_every_MHD_step) write(*,*) 'coupled%n_step = ',coupled%n_step

           ! if (SP%VS%rho%SS%solve)    call solve(dens,mom%U,  EF,EN,DT)
           if (SP%VS%T%SS%solve) then
             call solve(nrg,mom%U,  nrg%SP%VS%T%TMP ,EF,EN,DT)
           endif
           if (SP%VS%U%SS%solve) then
             call add_all_momentum_sources(F,Fnm1,nrg,mom,ind,SP)
             call solve(mom,F,Fnm1, mom%SP%VS%U%TMP ,EF,EN,DT)
           endif
           if (SP%VS%B%SS%solve) then
            call embedVelocity_E(ind%U_E,mom%U_E,ind%MD_fluid)
            call add_all_induction_sources(ind%F,ind%Fnm1,mom,ind,ind%SP%VS%B%TMP,SP)
            call solve(ind,ind%F,ind%Fnm1,ind%SP%VS%B%TMP ,EF,EN,DT)
           endif

           call iterate_step(coupled)

           call import(EN)
           call update(EN)
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
             call print(sc,coupled)
             call export(sc,coupled%t)
             write(*,*) 'Working directory = ',str(DT%tar)
             call import(KS)
           endif
           ! call import(EF)
         enddo
         call print(sc,coupled)
         call export(sc,coupled)

         ! ***************************************************************
         ! ********** FINISHED SOLVING MHD EQUATIONS *********************
         ! ***************************************************************
         if (SP%VS%T%SS%initialize) call export(SP%VS%T%TMP)
         if (SP%VS%U%SS%initialize) call export(SP%VS%U%TMP)
         if (SP%VS%B%SS%initialize) call export(SP%VS%B%TMP)
         call export(coupled)

         ! **************** EXPORT ONE FINAL TIME ***********************
         if (SP%VS%T%SS%initialize) call export_tec(nrg,DT)
         if (SP%VS%U%SS%initialize) call export_tec(mom,DT,F,Fnm1)
         if (SP%VS%B%SS%initialize) call export_tec(ind,DT)

         call delete(EF)
         call delete(sc)
         call delete(EN)
         call delete(KS)
         call delete(F)
         call delete(Fnm1)
         call delete(RM)
       end subroutine

       end module