       module mesh_generate_mod
       use current_precision_mod
       use mesh_mod
       use mesh_benchmark_geometries_mod
       use dir_tree_mod
       use string_mod
       use path_mod
       use mesh_domain_mod
       use sim_params_mod
       implicit none

       private
       public :: mesh_generate

       contains

       subroutine mesh_generate(m_mom,m_ind,MD_sigma,SP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_domain),intent(inout) :: MD_sigma
         type(sim_params),intent(in) :: SP
         call geometry_BMC(m_mom,m_ind,SP%MQP,MD_sigma,SP%DP,&
         SP%GP%tw,SP%include_vacuum,SP%GP%geometry)
       end subroutine

       end module