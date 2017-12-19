     module sim_params_extend_mod
     use sim_params_mod
     use sim_params_aux_mod
     use sim_params_init_mesh_mod
     use sim_params_init_solver_config_mod
     use sim_params_init_flow_config_mod
     use sim_params_init_export_mod
     use sim_params_init_logicals_mod
     use sim_params_init_DPs_mod
     implicit none

     private
     public :: sim_params
     public :: init

     interface init; module procedure init_SP; end interface

     contains

     subroutine init_SP(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       call delete(SP)
       call sim_params_init_logicals(SP)
       call sim_params_init_mesh(SP)
       call sim_params_init_DPs(SP)
       call sim_params_init_export(SP)
       call sim_params_init_solver_config(SP)
       call sim_params_init_flow_config(SP)
       call post_process(SP)
     end subroutine

     end module