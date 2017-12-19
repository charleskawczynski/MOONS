       module MOONS_solver_mod
       use current_precision_mod
       use sim_params_mod
       use var_set_extend_mod
       use VF_extend_mod
       use IO_tools_mod
       use string_mod
       use path_extend_mod
       use dir_tree_mod
       use stop_clock_extend_mod
       use export_frequency_mod
       use export_frequency_extend_mod
       use export_now_mod
       use export_now_extend_mod
       use export_safe_extend_mod
       use kill_switch_mod
       use kill_switch_extend_mod
       use probe_extend_mod
       use RK_Params_mod
       use time_statistics_extend_mod

       use time_marching_params_mod
       use time_marching_params_extend_mod
       use add_all_energy_sources_mod
       use add_all_momentum_sources_mod
       use add_all_induction_sources_mod
       use momentum_sources_mod
       use induction_sources_mod
       use induction_aux_mod
       use ops_interp_mod
       use energy_extend_mod
       use momentum_extend_mod
       use induction_extend_mod
       use momentum_sources_mod
       use MOONS_export_full_restart_mod
       use MOONS_simulate_crash_mod
       use MOONS_mod
       implicit none

       private
       public :: solve
       interface solve;   module procedure solve_MOONS;    end interface

       contains

       subroutine solve_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         integer :: i_RK
         call compute_J_ind(M%GE%ind,M%C%SP)
         call face2CellCenter(M%GE%mom%U_CC,M%GE%mom%U,M%GE%mom%m)
         call face2edge_no_diag(M%GE%mom%U_E,M%GE%mom%U,M%GE%mom%m)
         write(*,*) '***************************************************************'
         write(*,*) '****************** ENTERING MAIN LOOP *************************'
         write(*,*) '***************************************************************'
         do while ((.not.M%C%KS%terminate_loop).and.(M%C%SP%coupled%t.lt.M%C%SP%coupled%TS%t_final-M%C%SP%coupled%TS%dt*0.5_cp))

           call tic(M%C%sc)

           call update(M%C%ES,M%C%sc%t_passed)

           if (M%C%SP%FCL%print_every_MHD_step) then
             write(*,*) 'M%C%SP%coupled%n_step = ',M%C%SP%coupled%n_step
           endif
           ! if (M%C%SP%EF%info%export_now) call print(M%C%SP)

           do i_RK=1,M%C%SP%coupled%RKP%n_stages
             call assign_RK_stage(M%C%SP%VS%T%TMP,i_RK)
             call assign_RK_stage(M%C%SP%VS%U%TMP,i_RK)
             call assign_RK_stage(M%C%SP%VS%B%TMP,i_RK)
             call assign_RK_stage(M%C%SP%coupled,i_RK)
             call update(M%C%SP%EF,M%C%SP%coupled%t,M%C%SP%coupled%n_step,i_RK.ne.M%C%SP%coupled%RKP%n_stages)
             ! if (M%C%SP%VS%rho%SS%solve)    call solve(dens,M%GE%mom%U,  M%C%SP%EF,M%C%EN,M%C%DT)
             if (M%C%SP%VS%T%SS%solve) then
               call add_all_energy_sources(M%GE%nrg%F,M%GE%nrg%Fnm1,M%GE%nrg%L,M%GE%nrg,M%C%SP%VS%U%TMP,M%C%SP,M%GE%ind,M%GE%mom)
               call solve(M%GE%nrg,M%C%SP,M%GE%nrg%F,M%GE%nrg%Fnm1,M%GE%nrg%L,M%C%SP%VS%T%TMP,M%C%SP%EF)
             endif
             if (M%C%SP%VS%U%SS%solve) then
               call add_all_momentum_sources(M%GE%mom%F,M%GE%mom%Fnm1,M%GE%mom%L,M%GE%mom,M%C%SP%VS%U%TMP,M%C%SP,M%GE%ind,M%GE%nrg)
               call solve(M%GE%mom,M%C%SP,M%GE%mom%F,M%GE%mom%Fnm1,M%GE%mom%L,M%C%SP%VS%U%TMP,M%C%SP%EF)
             endif
             if (M%C%SP%VS%B%SS%solve) then
               call add_all_induction_sources(M%GE%ind%F,M%GE%ind%Fnm1,M%GE%ind%L,M%GE%ind,M%C%SP%VS%B%TMP,M%C%SP,M%GE%mom)
               call solve(M%GE%ind,M%C%SP,M%GE%ind%F,M%GE%ind%Fnm1,M%GE%ind%L,M%C%SP%VS%B%TMP,M%C%SP%EF)
             endif
             call iterate_RK(M%C%SP%VS%T%TMP)
             call iterate_RK(M%C%SP%VS%U%TMP)
             call iterate_RK(M%C%SP%VS%B%TMP)
             call iterate_RK(M%C%SP%coupled)
           enddo
           call update(M%C%SP%EF,M%C%SP%coupled%t,M%C%SP%coupled%n_step,.false.)

           if (M%C%SP%VS%T%SS%solve) call iterate_step(M%C%SP%VS%T%TMP)
           if (M%C%SP%VS%U%SS%solve) call iterate_step(M%C%SP%VS%U%TMP)
           if (M%C%SP%VS%B%SS%solve) call iterate_step(M%C%SP%VS%B%TMP)
           call iterate_step(M%C%SP%coupled)

           if (M%C%ES%export_now) then ! Sync probes with export_structured.
             M%C%SP%EF%unsteady_0D%export_now = .true.
           endif

           if (M%C%SP%VS%T%SS%solve) call export_unsteady(M%GE%nrg,M%C%SP,M%C%SP%VS%T%TMP,M%C%SP%EF,M%C%EN,M%C%DT)
           if (M%C%SP%VS%U%SS%solve) call export_unsteady(M%GE%mom,M%C%SP,M%C%SP%VS%U%TMP,M%C%SP%EF,M%C%EN,M%C%DT)
           if (M%C%SP%VS%B%SS%solve) call export_unsteady(M%GE%ind,M%C%SP,M%C%SP%VS%B%TMP,M%C%SP%EF,M%C%EN,M%C%DT)

           ! Statistics
           call update(M%GE%mom%TS,M%GE%mom%m,M%GE%mom%U,M%C%SP%VS%U%TMP,M%GE%mom%temp_F1,&
           M%GE%mom%temp_CC_VF,M%GE%mom%TF_CC,M%C%SP%EF%unsteady_0D%export_now)

           call toc(M%C%sc,M%C%SP%coupled)

           if (M%C%ES%export_now) then
             call MOONS_export_full_restart(M)
           endif

           call simulate_crash(M)

           call import_structured(M%C%SP%DP)
           call import_structured(M%GE%mom%PCG_U%ISP%EC)
           call import_structured(M%GE%mom%PCG_P%ISP%EC)
           call import_structured(M%GE%ind%PCG_B%ISP%EC)
           call import_structured(M%GE%ind%PCG_B%ISP%EC)
           call import_structured(M%GE%ind%PCG_cleanB%ISP%EC)
           call import_exit_criteria(M%C%SP%VS)
           call import_TMP_dt(M%C%SP%VS)
           call import_structured(M%C%SP%coupled%TS)
           if (M%C%SP%SCP%couple_time_steps) call couple_time_step(M%C%SP%VS,M%C%SP%coupled)
           call import_structured(M%C%EN)

           call update(M%C%EN,M%C%ES%export_now)
           if (M%C%EN%any_next) call export_structured(M%C%EN) ! May be needed to avoid constant exporting

           if (M%C%SP%EF%info%export_now) then
             if (M%C%SP%FCL%export_heavy) then
               call print(M%C%sc,M%C%SP%coupled)
               write(*,*) 'Working directory = ',str(M%C%DT%tar)
               write(*,*) 'Restart directory = ',str(M%C%DT%restart)
             else
               call print_light(M%C%sc,M%C%SP%coupled)
             endif
             call export(M%C%sc,M%C%SP%coupled%t)
             call import_structured(M%C%KS)
           endif
         enddo
         write(*,*) '***************************************************************'
         write(*,*) '******************* EXITING MAIN LOOP *************************'
         write(*,*) '***************************************************************'
       end subroutine
       end module