       module mesh_PD_geometries_mod
       use current_precision_mod
       use grid_init_mod
       use grid_extend_mod
       use grid_connect_mod
       use coordinate_stretch_parameters_mod
       use dir_tree_mod
       use string_mod
       use path_mod
       use grid_mod
       use mesh_domain_mod
       use mesh_mod
       use mesh_quality_params_mod
       use dimensionless_params_mod
       implicit none

       private
       public :: MHD_3D_NSC_PD

       contains

       subroutine MHD_3D_NSC_PD(m_mom,m_ind,MQP,MD_sigma,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(3) = -5.0_cp
         hmax(3) = 5.0_cp
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         N = (/50,50,100/)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       end module