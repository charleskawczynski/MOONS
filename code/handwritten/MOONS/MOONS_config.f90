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
         logical,parameter :: T = .true.
         logical,parameter :: F = .false.
         write(*,*) ' ************* STARTED CONFIGURING PROBES ************** '
         L = M%C%SP%FCL%restart_all

         if (M%C%SP%VS%U%SS%initialize) then
           call init(M%C%SP%PS_mom%amax_U_x   ,str(M%C%DT%U%local),'amax(u)', L,T,M%C%SP%VS%U%TMP)
           call init(M%C%SP%PS_mom%amax_U_y   ,str(M%C%DT%U%local),'amax(v)', L,T,M%C%SP%VS%U%TMP)
           call init(M%C%SP%PS_mom%amax_U_z   ,str(M%C%DT%U%local),'amax(w)', L,T,M%C%SP%VS%U%TMP)
           call init(M%C%SP%PS_mom%probe_divU ,str(M%C%DT%U%residual),'probe_divU',L,.true. ,M%C%SP%VS%U%TMP)
           call init(M%C%SP%PS_mom%probe_KE   ,str(M%C%DT%U%energy)  ,'KE'        ,L,.false.,M%C%SP%VS%U%TMP)
           call init(M%C%SP%PS_mom%probe_Q    ,str(M%C%DT%U%energy)  ,'probe_Q'   ,L,.true. ,M%C%SP%VS%U%TMP)
           write(*,*) '     Momentum probes initialized'
         endif

         if (M%C%SP%VS%B%SS%initialize) then
           call init(M%C%SP%PS_ind%probe_dB0dt(1),str(M%C%DT%B%local),'amax(dB0dt)_x',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%probe_dB0dt(2),str(M%C%DT%B%local),'amax(dB0dt)_y',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%probe_dB0dt(3),str(M%C%DT%B%local),'amax(dB0dt)_z',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%probe_B0(1)  ,str(M%C%DT%B%local),'amax(B0)_x',   L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%probe_B0(2)  ,str(M%C%DT%B%local),'amax(B0)_y',   L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%probe_B0(3)  ,str(M%C%DT%B%local),'amax(B0)_z',   L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%probe_B1(1)  ,str(M%C%DT%B%local),'amax(B1)_x',   L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%probe_B1(2)  ,str(M%C%DT%B%local),'amax(B1)_y',   L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%probe_B1(3)  ,str(M%C%DT%B%local),'amax(B1)_z',   L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%probe_Btot(1),str(M%C%DT%B%local),'amax(Btot)_x', L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%probe_Btot(2),str(M%C%DT%B%local),'amax(Btot)_y', L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%probe_Btot(3),str(M%C%DT%B%local),'amax(Btot)_z', L,T,M%C%SP%VS%B%TMP)

           call init(M%C%SP%PS_ind%amax_JxB     ,str(M%C%DT%jCrossB%energy),'amax(JxB)',   L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_JxB_x   ,str(M%C%DT%jCrossB%energy),'amax(JxB)_x', L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_JxB_y   ,str(M%C%DT%jCrossB%energy),'amax(JxB)_y', L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_JxB_z   ,str(M%C%DT%jCrossB%energy),'amax(JxB)_z', L,T,M%C%SP%VS%B%TMP)

           call init(M%C%SP%PS_ind%amax_stress_xx,str(M%C%DT%stresses%local),&
            'amax(stress)_xx',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_xy,str(M%C%DT%stresses%local),&
            'amax(stress)_xy',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_xz,str(M%C%DT%stresses%local),&
            'amax(stress)_xz',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_yx,str(M%C%DT%stresses%local),&
            'amax(stress)_yx',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_yy,str(M%C%DT%stresses%local),&
            'amax(stress)_yy',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_yz,str(M%C%DT%stresses%local),&
            'amax(stress)_yz',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_zx,str(M%C%DT%stresses%local),&
            'amax(stress)_zx',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_zy,str(M%C%DT%stresses%local),&
            'amax(stress)_zy',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_zz,str(M%C%DT%stresses%local),&
            'amax(stress)_zz',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_walls_xx,str(M%C%DT%stresses%local),&
            'amax(stress_walls)_xx',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_walls_xy,str(M%C%DT%stresses%local),&
            'amax(stress_walls)_xy',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_walls_xz,str(M%C%DT%stresses%local),&
            'amax(stress_walls)_xz',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_walls_yx,str(M%C%DT%stresses%local),&
            'amax(stress_walls)_yx',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_walls_yy,str(M%C%DT%stresses%local),&
            'amax(stress_walls)_yy',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_walls_yz,str(M%C%DT%stresses%local),&
            'amax(stress_walls)_yz',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_walls_zx,str(M%C%DT%stresses%local),&
            'amax(stress_walls)_zx',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_walls_zy,str(M%C%DT%stresses%local),&
            'amax(stress_walls)_zy',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%amax_stress_walls_zz,str(M%C%DT%stresses%local),&
            'amax(stress_walls)_zz',L,T,M%C%SP%VS%B%TMP)

           call init(M%C%SP%PS_ind%Bx(1)   ,str(M%C%DT%B%energy),'Bx',    L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%By(1)   ,str(M%C%DT%B%energy),'By',    L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%Bz(1)   ,str(M%C%DT%B%energy),'Bz',    L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%Bx(2)   ,str(M%C%DT%B%energy),'B0x',   L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%By(2)   ,str(M%C%DT%B%energy),'B0y',   L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%Bz(2)   ,str(M%C%DT%B%energy),'B0z',   L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%Bx(3)   ,str(M%C%DT%B%energy),'B1x',   L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%By(3)   ,str(M%C%DT%B%energy),'B1y',   L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%Bz(3)   ,str(M%C%DT%B%energy),'B1z',   L,T,M%C%SP%VS%B%TMP)

           call init(M%C%SP%PS_ind%probe_divB     ,str(M%C%DT%B%residual),'transient_divB',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%probe_divJ     ,str(M%C%DT%J%residual),'transient_divJ',L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%JE             ,str(M%C%DT%J%energy),'JE',            L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%JE_fluid       ,str(M%C%DT%J%energy),'JE_fluid',      L,T,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%ME(1)          ,str(M%C%DT%B%energy),'ME',           L,.false.,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%ME_fluid(1)    ,str(M%C%DT%B%energy),'ME_fluid',     L,.false.,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%ME_conductor(1),str(M%C%DT%B%energy),'ME_conductor', L,.false.,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%ME(2)          ,str(M%C%DT%B%energy),'ME0',          L,.false.,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%ME_fluid(2)    ,str(M%C%DT%B%energy),'ME0_fluid',    L,.false.,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%ME_conductor(2),str(M%C%DT%B%energy),'ME0_conductor',L,.false.,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%ME(3)          ,str(M%C%DT%B%energy),'ME1',          L,.false.,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%ME_fluid(3)    ,str(M%C%DT%B%energy),'ME1_fluid',    L,.false.,M%C%SP%VS%B%TMP)
           call init(M%C%SP%PS_ind%ME_conductor(3),str(M%C%DT%B%energy),'ME1_conductor',L,.false.,M%C%SP%VS%B%TMP)
           write(*,*) '     Induction probes initialized'
         endif

         if (M%C%SP%VS%T%SS%initialize) then
           call init(M%C%SP%PS_nrg%probe_divQ,str(M%C%DT%T%residual),'probe_divQ',L,T,M%C%SP%VS%T%TMP)
           write(*,*) '     Energy probes initialized'
         endif

         write(*,*) ' ************* FINISHED CONFIGURING PROBES ************* '
       end subroutine

       end module