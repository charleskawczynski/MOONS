       module mesh_BC_geometries_symmetric_mod
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
       implicit none

       private
       public :: MHD_3D_LDC_RVBC_Ha20_symmetric
       public :: MHD_3D_LDC_RVBC_Ha100_symmetric
       public :: BC_sim_ind_symmetric

       contains

       subroutine MHD_3D_LDC_RVBC_Ha20_symmetric(m_ind,m_mom,MD_sigma)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         real(cp) :: Ha
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         Ha = 20.0_cp
         hmin = -1.0_cp; hmax = 1.0_cp; beta = HartmannBL(Ha,hmin,hmax)
         hmax(3) = 0.0_cp
         N = (/15,15,10/)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 3; call grid_Roberts_L(g,hmin(i),hmax(i),N(i),beta(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call MHD_3D_LDC_RVBC_Ha20_symmetric_extend(m_ind,m_mom,MD_sigma)
       end subroutine
       subroutine MHD_3D_LDC_RVBC_Ha20_symmetric_extend(m_ind,m_mom,MD_sigma)
         implicit none
         type(mesh),intent(inout) :: m_ind
         type(mesh),intent(in) :: m_mom
         type(mesh_domain),intent(inout) :: MD_sigma
         real(cp) :: tw
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp) :: tf
         real(cp) :: Gamma_v
         integer :: N_w,N_v,N_extra
         call delete(m_ind)
         call init(g,m_mom%B(1)%g)
         Gamma_v = 7.0_cp
         tf = 1.0_cp
         N_v = 5
         N_w = 2
         N_extra = N_w ! since no wall domain above lid
         ! Wall
         call ext_Roberts_B_IO(g,tw,N_w,1)
         call ext_prep_Roberts_B_IO(g,tw,N_w,3)
         call ext_prep_Roberts_B_IO(g,tw,N_w,2)
         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call init_props(m_sigma)
         call patch(m_sigma)
         ! Vacuum
         call ext_Roberts_near_IO(g,Gamma_v - tw - tf,N_v,1) ! x-direction
         call ext_prep_Roberts_R_IO(g,Gamma_v - tw - tf,N_v,3) ! z-direction
         call ext_prep_Roberts_R_IO(g,Gamma_v - tw - tf,N_v,2) ! y-direction
         call ext_app_Roberts_L_IO (g,Gamma_v - tf,N_v+N_extra,2) ! y-direction
         call add(m_ind,g)
         call init_props(m_ind)
         call patch(m_ind)
         call init(MD_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       end module