       module mesh_PD_geometries_mod
       use current_precision_mod
       use grid_init_mod
       use grid_continue_mod
       use grid_connect_mod
       use coordinate_stretch_parameters_mod
       use dir_tree_mod
       use string_mod
       use path_extend_mod
       use grid_mod
       use mesh_domain_mod
       use mesh_extend_mod
       use mesh_quality_params_mod
       use dimensionless_params_mod
       implicit none

       private
       public :: MHD_3D_NSC_PD
       public :: MHD_3D_2_channel_PD

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

       subroutine MHD_3D_2_channel_PD(m_mom,m_ind,MQP,MD_sigma,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i,a,b,N_w
         real(cp) :: tw,duct_width
         call delete(m_mom)
         call delete(m_ind)
         tw = 0.1_cp
         a = 2
         b = 3
         N_w = 16
         hmin(1) = -0.5_cp
         hmax(1) = 0.5_cp
         hmin(a) = -1.0_cp
         hmax(a) =  1.0_cp
         duct_width = 2.0_cp
         N = (/1,70,70/)

         hmin(b) = - 0.5_cp*tw - duct_width
         hmax(b) = - 0.5_cp*tw
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i = 1; call grid_Roberts_B(g1,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g1,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g1,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g1)

         hmin(b) = 0.5_cp*tw
         hmax(b) = 0.5_cp*tw + duct_width
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i = 1; call grid_Roberts_B(g2,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g2,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g2,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g2)

         call ext_app_Roberts_B_IO(g1,tw,N_w,b,MQP)
         call ext_app_Roberts_B_IO(g1,duct_width,N(a),b,MQP)
         ! Walls
         call ext_prep_Roberts_B_IO(g1,tw,N_w,b,MQP)
         call ext_app_Roberts_B_IO(g1,tw,N_w,b,MQP)
         call ext_Roberts_B_IO(g1,tw,N_w,a,MQP)
         call init(m_ind,g1)

         call init_props(m_ind)
         call patch(m_ind)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g1)
         call delete(g2)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       end module