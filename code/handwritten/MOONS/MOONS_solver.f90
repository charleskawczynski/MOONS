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
       use refine_mesh_extend_mod
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
       use energy_extend_mod
       use momentum_extend_mod
       use induction_extend_mod
       use momentum_sources_mod
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
         write(*,*) '***************************************************************'
         write(*,*) '****************** ENTERING MAIN LOOP *************************'
         write(*,*) '***************************************************************'
         do while ((.not.M%KS%terminate_loop).and.(M%SP%coupled%t.lt.M%SP%coupled%t_final-M%SP%coupled%dt*0.5_cp))

           call tic(M%SC)

           if (M%SP%FCL%print_every_MHD_step) then
             write(*,*) 'M%SP%coupled%n_step = ',M%SP%coupled%n_step
           endif
           ! if (M%SP%EF%info%export_now) call print(M%SP)

           do i_RK=1,M%SP%coupled%RKP%n_stages
             call assign_RK_stage(M%SP%VS%T%TMP,i_RK)
             call assign_RK_stage(M%SP%VS%U%TMP,i_RK)
             call assign_RK_stage(M%SP%VS%B%TMP,i_RK)
             call assign_RK_stage(M%SP%coupled,i_RK)
             call update(M%SP%EF,M%SP%coupled%n_step,i_RK.ne.M%SP%coupled%RKP%n_stages)
             ! if (M%SP%VS%rho%SS%solve)    call solve(dens,M%mom%U,  M%SP%EF,M%EN,M%DT)
             if (M%SP%VS%T%SS%solve) then
               call add_all_energy_sources(M%nrg%F,M%nrg%Fnm1,M%nrg%L,M%nrg,M%SP%VS%U%TMP,M%SP,M%ind,M%mom)
               call solve(M%nrg,M%SP,M%nrg%F,M%nrg%Fnm1,M%nrg%L,M%SP%VS%T%TMP,M%SP%EF)
             endif
             if (M%SP%VS%U%SS%solve) then
               call add_all_momentum_sources(M%mom%F,M%mom%Fnm1,M%mom%L,M%mom,M%SP%VS%U%TMP,M%SP,M%ind,M%nrg)
               call solve(M%mom,M%SP,M%mom%F,M%mom%Fnm1,M%mom%L,M%SP%VS%U%TMP,M%SP%EF)
             endif
             if (M%SP%VS%B%SS%solve) then
               call add_all_induction_sources(M%ind%F,M%ind%Fnm1,M%ind%L,M%ind,M%SP%VS%B%TMP,M%SP,M%mom)
               call solve(M%ind,M%SP,M%ind%F,M%ind%Fnm1,M%ind%L,M%SP%VS%B%TMP,M%SP%EF)
             endif
             call iterate_RK(M%SP%VS%T%TMP)
             call iterate_RK(M%SP%VS%U%TMP)
             call iterate_RK(M%SP%VS%B%TMP)
             call iterate_RK(M%SP%coupled)
           enddo
           call update(M%SP%EF,M%SP%coupled%n_step,.false.)

           if (M%SP%VS%T%SS%solve) call iterate_step(M%SP%VS%T%TMP)
           if (M%SP%VS%U%SS%solve) call iterate_step(M%SP%VS%U%TMP)
           if (M%SP%VS%B%SS%solve) call iterate_step(M%SP%VS%B%TMP)
           call iterate_step(M%SP%coupled)

           if (M%SP%VS%T%SS%solve) call export_unsteady(M%nrg,M%SP,M%SP%VS%T%TMP,M%SP%EF,M%EN,M%DT)
           if (M%SP%VS%U%SS%solve) call export_unsteady(M%mom,M%SP,M%SP%VS%U%TMP,M%SP%EF,M%EN,M%DT)
           if (M%SP%VS%B%SS%solve) call export_unsteady(M%ind,M%SP,M%SP%VS%B%TMP,M%SP%EF,M%EN,M%DT)

           ! Statistics
           call update(M%mom%TS,M%mom%m,M%mom%U,M%SP%VS%U%TMP,M%mom%temp_F1,M%mom%temp_CC_VF,M%mom%TF_CC)

           if (M%EN%any_now) then
             call export_ISP(M%SP%VS)
             call export_TMP(M%SP%VS)
             call export(M%SP%coupled)
           endif

           ! call import(M%SP%DP,str(M%DT%dimensionless_params),'dimensionless_params')
           call import_exit_criteria(M%mom%PCG_U%ISP)
           call import_exit_criteria(M%mom%PCG_P%ISP)
           call import_exit_criteria(M%ind%PCG_B%ISP)
           call import_exit_criteria(M%ind%PCG_cleanB%ISP)
           call import_exit_criteria(M%SP%VS)
           call import_TMP_dt(M%SP%VS)
           call import_dt(M%SP%coupled)
           if (M%SP%SCP%couple_time_steps) call couple_time_step(M%SP%VS,M%SP%coupled)

           call update(M%ES,M%SC%t_passed)

           call import(M%EN)
           call update(M%EN,M%ES%export_now)
           if (M%EN%any_next) call export(M%EN)

           call import(M%RM)
           call update(M%RM)
           if (M%RM%any_next) call export(M%RM)

           call toc(M%SC,M%SP%coupled)
           if (M%SP%EF%info%export_now) then
             ! oldest_modified_file violates intent, but this
             ! would be better to update outside the solvers,
             ! since it should be updated for all solver variables.
             ! call oldest_modified_file(M%DT%restart,M%DT%restart1,M%DT%restart2,'p.dat')
             if (M%SP%FCL%export_heavy) then
               call print(M%SC,M%SP%coupled)
             else
               call print_light(M%SC,M%SP%coupled)
             endif
             call export(M%SC,M%SP%coupled%t)
             call import(M%KS)
           endif
           ! call import(M%SP%EF)
         enddo
         write(*,*) '***************************************************************'
         write(*,*) '******************* EXITING MAIN LOOP *************************'
         write(*,*) '***************************************************************'
       end subroutine
       end module