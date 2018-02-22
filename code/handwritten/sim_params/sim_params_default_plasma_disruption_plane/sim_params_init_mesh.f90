     module sim_params_init_mesh_mod
     use current_precision_mod
     use constants_mod
     use string_mod
     use dir_tree_mod
     use path_extend_mod
     use mesh_params_extend_mod
     use segment_extend_mod
     use mesh_quality_params_extend_mod
     use mesh_bank_mod
     use sim_params_mod
     use sim_params_aux_mod
     implicit none

     private
     public :: sim_params_init_mesh

     contains

     subroutine sim_params_init_mesh(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       logical,parameter :: T = .true.
       logical,parameter :: F = .false.
       ! call init(MQP,auto_find_N,max_mesh_stretch_ratio,N_max_points_add)
       call init(SP%MQP,T,2.0_cp,50)

       call define_mesh_SP_plasma_disruption_plane(SP)
       ! call define_mesh_SP_plasma_disruption_plane_insulating(SP)
       ! call define_mesh_SP_plasma_disruption_line(SP)
       ! call small_dataset(SP)
       ! call define_mesh_bandaru(SP)
       ! call define_mesh_full_BC_w_vacuum_symmetric(SP)
       ! call define_mesh_full_BC_n_vacuum_symmetric(SP)
       ! call define_mesh_full_BC_w_vacuum_3D(SP)
       ! call define_mesh_full_BC_n_vacuum_3D(SP)
       ! call define_mesh_SP_MHD_LDC_Sergey_uniform(SP)
     end subroutine

     end module