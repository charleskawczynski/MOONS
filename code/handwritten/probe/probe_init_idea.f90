       module MOONS_config_mod
       use current_precision_mod
       use IO_tools_mod
       use IO_import_mod
       use IO_export_mod
       use inquire_funcs_mod
       use matrix_visualization_mod
       use dir_manip_mod

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

       use energy_extend_mod
       use momentum_extend_mod
       use induction_extend_mod
       use MOONS_mod

       implicit none

       private
       public :: config
       interface config;         module procedure config_MOONS;         end interface

       contains

       subroutine config_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         write(*,*) ' ************** STARTED CONFIGURING MOONS ************** '
         call delete_file('','mesh_generation_error')
#ifdef fopenmp
         call omp_set_num_threads(12) ! Set number of openMP threads

#endif
         if (file_exists(str(M%C%DT%restart1),'primitives')) then
           write(*,*) 'Restarting from ',str(M%C%DT%restart1)
           call import_structured(M%C,str(M%C%DT%restart1))
           call init(M%C%DT%restart,M%C%DT%restart1)
           M%C%SP%FCL%restart_all = .true.
         elseif (file_exists(str(M%C%DT%restart2),'primitives')) then
           write(*,*) 'Restarting from ',str(M%C%DT%restart2)
           call import_structured(M%C,str(M%C%DT%restart2))
           call init(M%C%DT%restart,M%C%DT%restart2)
           M%C%SP%FCL%restart_all = .true.
         else
           call init(M%C%SP)
         endif
         call display_compiler_info(str(M%C%DT%params),'compiler_info')
         call print_version()
         call export_version(str(M%C%DT%LDC))
         call init(M%C%sc,str(M%C%DT%wall_clock),'WALL_CLOCK_TIME_INFO')
         call config_probes(M)
         write(*,*) ' ************** FINISHED CONFIGURING MOONS ************* '
       end subroutine

       subroutine config_probes(M)
         implicit none
         type(MOONS),intent(inout) :: M
         logical :: L
         type(string_list) :: SL
         write(*,*) ' ************* STARTED CONFIGURING PROBES ************** '
         L = M%C%SP%FCL%restart_all

         if (M%C%SP%VS%U%SS%initialize) then
           call init(M%C%SP%PS_mom%data_probe,str(M%C%DT%U%probes),'momentum_data')
           call init(SL,'Time')
           call add(SL,'divU')
           call add(SL,'KE')
           call add(SL,'probe_Q')
           call set_var_names(M%C%SP%PS_mom%data_probe,SL)
           call delete(SL)
           write(*,*) '     Momentum probes initialized'
         endif

         if (M%C%SP%VS%B%SS%initialize) then
             call init(M%C%SP%PS_ind%data_probe,str(M%C%DT%B%probes),'induction_data')
             call init(SL,'Time')
             call add(SL,'L2(Btot)')
             call add(SL,'L2(Btot_x)')
             call add(SL,'L2(Btot_y)')
             call add(SL,'L2(Btot_z)')
             call add(SL,'L2(B0)')
             call add(SL,'L2(B0_x)')
             call add(SL,'L2(B0_y)')
             call add(SL,'L2(B0_z)')
             call add(SL,'L2(B1)')
             call add(SL,'L2(B1_x)')
             call add(SL,'L2(B1_y)')
             call add(SL,'L2(B1_z)')
             call add(SL,'L2(J)')
             call add(SL,'L2(J_x)')
             call add(SL,'L2(J_y)')
             call add(SL,'L2(J_z)')
             call add(SL,'max(JxB)')
             call add(SL,'max(JxB_x)')
             call add(SL,'max(JxB_y)')
             call add(SL,'max(JxB_z)')
             call add(SL,'L2(divB)')
             call add(SL,'L2(divJ)')
             call add(SL,'JE')
             call add(SL,'JE_fluid')
             call add(SL,'ME')
             call add(SL,'ME_fluid')
             call add(SL,'ME_conductor')
             call add(SL,'ME0')
             call add(SL,'ME0_fluid')
             call add(SL,'ME0_conductor')
             call add(SL,'ME1')
             call add(SL,'ME1_fluid')
             call add(SL,'ME1_conductor')
             call set_var_names(M%C%SP%PS_ind%data_probe,SL)
             call delete(SL)

           if (M%C%SP%IT%unsteady_B0%add) then
                 call init(M%C%SP%PS_ind%data_probe_unsteady,str(M%C%DT%B%probes),'induction_data_unsteady')
                 call init(SL,'Time')
                 call add(SL,'dB0dt_x')
                 call add(SL,'dB0dt_y')
                 call add(SL,'dB0dt_z')
                 call add(SL,'B0_x')
                 call add(SL,'B0_y')
                 call add(SL,'B0_z')
                 call set_var_names(M%C%SP%PS_ind%data_probe_unsteady,SL)
           endif
           write(*,*) '     Induction probes initialized'
         endif

         if (M%C%SP%VS%T%SS%initialize) then
           call init(M%C%SP%PS_nrg%data_probe,str(M%C%DT%T%residual),'probe_divQ',L,.true.,M%C%SP%VS%T%TMP)

           call init(M%C%SP%PS_nrg%data_probe,str(M%C%DT%T%probes),'energy_data')
           call init(SL,'Time')
           call add(SL,'divQ')
           call set_var_names(M%C%SP%PS_nrg%data_probe,SL)
           call delete(SL)
           write(*,*) '     Energy probes initialized'
         endif

         write(*,*) ' ************* FINISHED CONFIGURING PROBES ************* '
       end subroutine

       end module