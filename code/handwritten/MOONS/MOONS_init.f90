       module MOONS_init_mod
       use current_precision_mod
       use IO_tools_mod
       use IO_import_mod
       use IO_export_mod
       use inquire_funcs_mod
       use matrix_visualization_mod

       use version_mod
       use mesh_extend_mod
       use mesh_domain_extend_mod
       use generate_mesh_generic_mod
       use VF_extend_mod
       use string_mod
       use path_extend_mod
       use dir_tree_extend_mod
       use var_set_extend_mod
       use export_analytic_mod
       use mirror_props_mod
       use export_mesh_aux_mod
       use restart_file_mod

       use iter_solver_params_mod
       use time_marching_params_mod
       use sim_params_mod
       use sim_params_aux_mod
       use sim_params_extend_mod
       use export_raw_processed_symmetry_mod
       use export_raw_processed_mod
       use import_raw_mod
       use ops_mirror_field_mod

       use stop_clock_extend_mod
       use kill_switch_extend_mod
       use export_frequency_extend_mod
       use export_now_extend_mod
       use export_safe_extend_mod
       use refine_mesh_extend_mod

       use energy_extend_mod
       use momentum_extend_mod
       use induction_extend_mod
       use MOONS_mod

       implicit none

       private
       public :: init
       interface init;   module procedure init_MOONS;   end interface

       contains

       subroutine init_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         if (M%C%SP%FCL%restart_meshes) then
           call import(M%GE%mom%m,str(M%C%DT%mesh_restart),'m_mom')
           call import(M%GE%ind%m,str(M%C%DT%mesh_restart),'m_ind')
           call import(M%GE%ind%MD_sigma,str(M%C%DT%mesh_restart),'MD_sigma')
           call import(M%GE%ind%MD_fluid,str(M%C%DT%mesh_restart),'MD_fluid')
         else
           call generate_mesh_generic(M%GE%mom%m,M%C%SP%MP_mom,M%C%SP%DP,'momentum in MOONS.f90')
           call generate_mesh_generic(M%GE%ind%m_sigma,M%C%SP%MP_sigma,M%C%SP%DP,'sigma in MOONS.f90')
           call generate_mesh_generic(M%GE%ind%m,M%C%SP%MP_ind,M%C%SP%DP,'induction in MOONS.f90')

           if (M%C%SP%VS%U%SS%initialize) then; call init_props(M%GE%mom%m); call patch(M%GE%mom%m); endif
           if (M%C%SP%VS%B%SS%initialize) then; call init_props(M%GE%ind%m); call patch(M%GE%ind%m); endif
           if (M%C%SP%VS%U%SS%initialize) then; call init_apply_BC_order(M%GE%mom%m,M%C%SP%GP%apply_BC_order); endif
           if (M%C%SP%VS%B%SS%initialize) then; call init_apply_BC_order(M%GE%ind%m,M%C%SP%GP%apply_BC_order); endif

           if (M%C%SP%VS%U%SS%initialize.and.M%C%SP%VS%B%SS%initialize) then
             call init(M%GE%ind%MD_fluid,M%GE%mom%m,M%GE%ind%m)
             call init(M%GE%ind%MD_sigma,M%GE%ind%m_sigma,M%GE%ind%m)
             call init_props(M%GE%ind%MD_fluid%m_R1); call patch(M%GE%ind%MD_fluid%m_R1)
             call init_props(M%GE%ind%MD_fluid%m_R2); call patch(M%GE%ind%MD_fluid%m_R2)
             call init_props(M%GE%ind%MD_sigma%m_R1); call patch(M%GE%ind%MD_sigma%m_R1)
             call init_props(M%GE%ind%MD_sigma%m_R2); call patch(M%GE%ind%MD_sigma%m_R2)
           endif

         endif

         call export_mesh_aux(M%C%SP,M%C%DT,M%GE%mom%m,M%GE%ind%m)

         ! ******************** EXPORT GRIDS **************************** Export mesh (to plot)
         if (M%C%SP%EL%export_meshes) then
           call export_mesh(M%GE%mom%m,str(M%C%DT%meshes),'m_mom',1)
           call export_mesh(M%GE%ind%m,str(M%C%DT%meshes),'m_ind',1)
           call export_mesh(M%GE%ind%MD_sigma%m_R2,str(M%C%DT%meshes),'mesh_MD_sigma',1)
           call export_mesh(M%GE%ind%MD_fluid%m_R2,str(M%C%DT%meshes),'mesh_MD_fluid',1)

           if (M%C%SP%MP%mirror) then
             call mirror_mesh(M%m_temp,M%GE%mom%m,M%C%SP%MP)
             call export_mesh(M%m_temp,str(M%C%DT%meshes),'mesh_mom_mirror',1)
             call mirror_mesh(M%m_temp,M%GE%mom%m,M%C%SP%MP)
             call export_mesh(M%m_temp,str(M%C%DT%meshes),'mesh_ind_mirror',1)
             call mirror_mesh(M%m_temp,M%GE%ind%MD_sigma%m_R1,M%C%SP%MP)
             call export_mesh(M%m_temp,str(M%C%DT%meshes),'mesh_MD_sigma_mirror',1)
             call mirror_mesh(M%m_temp,M%GE%ind%MD_fluid%m_R1,M%C%SP%MP)
             call export_mesh(M%m_temp,str(M%C%DT%meshes),'mesh_MD_fluid_mirror',1)
             call delete(M%m_temp)
           endif
         endif

         call export(M%GE%mom%m,str(M%C%DT%mesh_restart),'m_mom')
         call export(M%GE%ind%m,str(M%C%DT%mesh_restart),'m_ind')
         call export(M%GE%ind%MD_sigma,str(M%C%DT%mesh_restart),'MD_sigma')
         call export(M%GE%ind%MD_fluid,str(M%C%DT%mesh_restart),'MD_fluid')

         if (file_exists('','mesh_generation_error')) then
           stop 'Error: non-converged mesh, inspect mesh.'
         endif

         if (M%C%SP%FCL%stop_after_mesh_export) then
           stop 'Exported meshes. Turn off stop_after_mesh_export in sim_params.f90 to run sim.'
         endif

         ! Initialize energy,momentum,induction
         if (M%C%SP%VS%T%SS%initialize) call init(M%GE%nrg,M%C%SP,M%C%DT)
         if (M%C%SP%VS%U%SS%initialize) call init(M%GE%mom,M%C%SP,M%C%DT)
         if (M%C%SP%VS%B%SS%initialize) call init(M%GE%ind,M%C%SP,M%C%DT)

         ! ********************* EXPORT RAW ICs *************************

         if (M%C%SP%EL%export_ICs.and.M%C%SP%VS%T%SS%initialize) call export_tec(M%GE%nrg,M%C%SP,M%C%DT)
         if (M%C%SP%EL%export_ICs.and.M%C%SP%VS%U%SS%initialize) call export_tec(M%GE%mom,M%C%SP,M%C%DT)
         if (M%C%SP%EL%export_ICs.and.M%C%SP%VS%B%SS%initialize) call export_tec(M%GE%ind,M%C%SP,M%C%DT)

         if (M%C%SP%FCL%print_mesh_before_solve) then
           if (M%C%SP%VS%T%SS%initialize) call print(M%GE%nrg%m)
           if (M%C%SP%VS%U%SS%initialize) call print(M%GE%mom%m)
           if (M%C%SP%VS%B%SS%initialize) call print(M%GE%ind%m)
         endif

         ! ******************** PREP TIME START/STOP ********************

         if (M%C%SP%FCL%stop_before_solve) then
           stop 'Exported ICs. Turn off stop_before_solve in sim_params.f90 to run sim.'
         endif

         ! call init(M%C%KS,str(M%C%DT%params),'kill_switch')
         ! call init(M%C%EN,str(M%C%DT%export_now),'EN')
         ! call init(M%C%RM,str(M%C%DT%refine_mesh),'RM')

         call init(M%C%ES,M%C%SP%SCP%export_safe_period)
         call init(M%C%sc,str(M%C%DT%wall_clock),'WALL_CLOCK_TIME_INFO')

         ! call export_ISP(M%C%SP%VS)
         ! call export_TMP(M%C%SP%VS)
         ! call export(M%C%SP%coupled)

         if (M%C%SP%FCL%export_heavy) then
           write(*,*) 'Working directory = ',str(M%C%DT%tar)
         endif

       end subroutine

       end module