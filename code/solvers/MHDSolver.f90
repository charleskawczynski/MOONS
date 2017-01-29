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
       use probe_mod
       use dynamic_refine_mesh_mod

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
         type(sim_params),intent(inout) :: SP
         type(time_marching_params),intent(inout) :: coupled
         type(stop_clock) :: sc
         type(VF) :: F,Fnm1 ! Forces added to momentum equation
         type(print_export) :: PE
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
         call init(PE,SP%PE); call export(PE)
         call init(sc,coupled%n_step_stop-coupled%n_step,str(DT%wall_clock),'WALL_CLOCK_TIME_INFO')

         write(*,*) 'Working directory = ',str(DT%tar)

         write(*,*) '***************************************************************'
         write(*,*) '****************** ENTERING MAIN LOOP *************************'
         write(*,*) '***************************************************************'
         do while ((.not.KS%terminate_loop).and.(coupled%n_step.lt.coupled%n_step_stop))
           call tic(sc)
           call update(PE,coupled%n_step)
           ! write(*,*) 'coupled%n_step = ',coupled%n_step

           if (SP%DMR%dynamic_refinement) then
             if (refine_mesh_now_all.or.RM%any_next) PE%transient_0D = .true.
           endif

           ! if (SP%solveDensity)    call solve(dens,mom%U,  PE,EN,DT)
           if (SP%VS%T%SS%solve) call solve(nrg,mom%U,  PE,EN,DT)
           if (SP%VS%U%SS%solve) call solve(mom,F,Fnm1, PE,EN,DT)
           if (SP%VS%B%SS%solve) call solve(ind,mom%U_E,PE,EN,DT)

           if (SP%DMR%dynamic_refinement.or.RM%any_next) then
             call dynamic_refine_mesh(nrg,mom,ind,DT,SP,coupled,sc,F,Fnm1,PE,RM,KS,refine_mesh_now_all)
           endif

           call assign(Fnm1,F)
           call assign(F,0.0_cp) ! DO NOT REMOVE THIS, FOLLOW THE COMPUTE_ADD PROCEDURE BELOW

           if (SP%MF%JCrossB) then
             call compute_AddJCrossB(F,ind%B,ind%B0,ind%J,ind%m,&
                                     ind%MD_fluid,mom%SP%DP%N,&
                                     ind%SP%finite_Rem,ind%temp_CC_SF,&
                                     ind%temp_F1,ind%temp_F1_TF,&
                                     ind%temp_F2_TF,mom%temp_F1)
           endif
           if (SP%MF%Q2D_JCrossB) then
             call compute_add_Q2D_JCrossB(F,mom%U,mom%SP%DP%tau,mom%temp_F1)
           endif
           if (SP%MF%Buoyancy) then
             call compute_AddBuoyancy(F,nrg%T,nrg%gravity,&
                                      mom%SP%DP%Gr,mom%SP%DP%Re,nrg%m,nrg%MD,&
                                      nrg%temp_F,nrg%temp_CC1_VF,mom%temp_F1)
           endif
           if (SP%MF%Gravity) then
             call compute_AddGravity(F,nrg%gravity,mom%SP%DP%Fr,nrg%m,&
                                     nrg%MD,nrg%temp_F,nrg%temp_CC1_VF,&
                                     mom%temp_F1)
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
             call print(sc,coupled)
             call export(sc,coupled%t)
             write(*,*) 'Working directory = ',str(DT%tar)
             call import(KS)
           endif
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

         call delete(PE)
         call delete(sc)
         call delete(EN)
         call delete(KS)
         call delete(F)
         call delete(Fnm1)
         call delete(RM)
       end subroutine

       end module