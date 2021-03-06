     module sim_params_init_export_mod
     use current_precision_mod
     use constants_mod
     use string_mod
     use dir_tree_mod
     use time_statistics_params_extend_mod
     use export_frequency_params_extend_mod
     use export_frequency_extend_mod
     use export_field_extend_mod
     use export_planes_extend_mod
     use export_lines_extend_mod
     use sim_params_mod
     use sim_params_aux_mod
     implicit none

     private
     public :: sim_params_init_export

     contains

     subroutine sim_params_init_export(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       logical,parameter :: T = .true.
       logical,parameter :: F = .false.

       ! call init(EFP,export_ever,export_first_step,frequency_base,frequency_exp)
       call init(SP%EF%info          ,T,T,1,10,2,0.0_cp,SP%DP%t_final)
       call init(SP%EF%unsteady_0D   ,T,T,1,10,2,0.0_cp,SP%DP%t_final)
       call init(SP%EF%unsteady_1D   ,F,F,1,10,2,0.0_cp,SP%DP%t_final)
       call init(SP%EF%unsteady_2D   ,F,F,1,10,2,0.0_cp,SP%DP%t_final)
       call init(SP%EF%unsteady_3D   ,F,F,1,10,4,0.0_cp,2.0_cp)
       call init(SP%EF%restart_files ,F,F,1,10,2,0.0_cp,SP%DP%t_final)
       call init(SP%EF%final_solution,F,F,1,10,6,0.0_cp,SP%DP%t_final)

       ! call init(export_field,export_ever)
       call init(SP%VS%T%unsteady_field  ,F)
       call init(SP%VS%U%unsteady_field  ,F)
       call init(SP%VS%P%unsteady_field  ,F)
       call init(SP%VS%B%unsteady_field  ,F)
       call init(SP%VS%B0%unsteady_field ,F)
       call init(SP%VS%phi%unsteady_field,F)
       call init(SP%VS%rho%unsteady_field,F)
       ! call init(export_plane,export_ever,dir,plane)
       call init(SP%VS%T%unsteady_planes  ,F,2,1,'1')
       call init(SP%VS%U%unsteady_planes  ,F,3,2,'1')
       call add (SP%VS%U%unsteady_planes  ,F,3,23,'2')
       call init(SP%VS%P%unsteady_planes  ,F,2,1,'1')
       call init(SP%VS%B%unsteady_planes  ,F,2,1,'1')
       call init(SP%VS%B0%unsteady_planes ,F,2,1,'1')
       call init(SP%VS%phi%unsteady_planes,F,2,1,'1')
       call init(SP%VS%rho%unsteady_planes,F,2,1,'1')
       ! call init(export_line,export_ever,dir,line)
       call init(SP%VS%T%unsteady_lines  ,F,1,(/2,34/),'1')
       call init(SP%VS%U%unsteady_lines  ,F,1,(/2,34/),'1')
       call init(SP%VS%P%unsteady_lines  ,F,1,(/2,34/),'1')
       call init(SP%VS%B%unsteady_lines  ,F,1,(/2,34/),'1')
       call init(SP%VS%B0%unsteady_lines ,F,1,(/2,34/),'1')
       call init(SP%VS%phi%unsteady_lines,F,1,(/2,34/),'1')
       call init(SP%VS%rho%unsteady_lines,F,1,(/2,34/),'1')

       ! Statistics
       ! call init(TSP,collect,t_start,t_stop)
       call init(SP%TSP,T,900.0_cp,1000.0_cp)
     end subroutine

     end module