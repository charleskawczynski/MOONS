       module mesh_BC_geometries_mod
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
       public :: MHD_3D_LDC_BC
       public :: MHD_3D_LDC_BC_symmetric
       public :: MHD_3D_LDC_BC_symmetric_fine_top

       contains

       subroutine MHD_3D_LDC_BC(m_mom,m_ind,MQP,MD_sigma,DP,tw,include_vacuum)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         real(cp),intent(in) :: tw
         logical,intent(in) :: include_vacuum
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i,N_w,N_v,N_extra
         real(cp) :: tf,Gamma_v
         call delete(m_mom); call delete(m_ind)
         hmin = -1.0_cp; hmax = 1.0_cp; beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         N = (/30,30,30/)
         N_v = 12
         N_w = 4
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call init(g,m_mom%B(1)%g)
         Gamma_v = 7.0_cp
         tf = 1.0_cp
         N_extra = N_w ! since no wall domain above lid
         ! Wall
         call ext_Roberts_B_IO(g,tw,N_w,1,MQP)
         call ext_Roberts_B_IO(g,tw,N_w,3,MQP)
         call ext_prep_Roberts_B_IO(g,tw,N_w,2,MQP)
         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call init_props(m_sigma)
         call patch(m_sigma)
         ! Vacuum
         ! Remove the following 4 lines for vacuum-absent case
         if (include_vacuum) then
           call ext_Roberts_near_IO  (g,Gamma_v - tw - tf,N_v   ,1,MQP) ! x-direction
           call ext_Roberts_near_IO  (g,Gamma_v - tw - tf,N_v   ,3,MQP) ! z-direction
           call ext_prep_Roberts_R_IO(g,Gamma_v - tw - tf,N_v   ,2,MQP) ! y-direction
           call ext_app_Roberts_L_IO (g,Gamma_v - tf,N_v+N_extra,2,MQP) ! y-direction
         endif
         call add(m_ind,g)
         call init_props(m_ind)
         call patch(m_ind)
         call init(MD_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       subroutine MHD_3D_LDC_BC_symmetric(m_mom,m_ind,MQP,MD_sigma,DP,tw,include_vacuum)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         real(cp),intent(in) :: tw
         logical,intent(in) :: include_vacuum
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i,N_w,N_v,N_extra
         real(cp) :: tf,Gamma_v
         call delete(m_mom)
         hmin = -1.0_cp; hmax = 1.0_cp; beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         hmax(3) = 0.0_cp
         ! N = (/15,15,10/)
         N = (/32,32,16/)
         N_v = 5
         N_w = 4
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_L(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call delete(m_ind)
         call init(g,m_mom%B(1)%g)
         Gamma_v = 7.0_cp
         tf = 1.0_cp
         N_extra = N_w ! since no wall domain above lid
         ! Wall
         call ext_Roberts_B_IO(g,tw,N_w,1,MQP)
         call ext_prep_Roberts_B_IO(g,tw,N_w,3,MQP)
         call ext_prep_Roberts_B_IO(g,tw,N_w,2,MQP)
         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call init_props(m_sigma)
         call patch(m_sigma)
         ! Vacuum
         if (include_vacuum) then
           call ext_Roberts_near_IO  (g,Gamma_v - tw - tf,N_v   ,1,MQP) ! x-direction
           call ext_prep_Roberts_R_IO(g,Gamma_v - tw - tf,N_v   ,3,MQP) ! z-direction
           call ext_prep_Roberts_R_IO(g,Gamma_v - tw - tf,N_v   ,2,MQP) ! y-direction
           call ext_app_Roberts_L_IO (g,Gamma_v - tf,N_v+N_extra,2,MQP) ! y-direction
         endif
         call add(m_ind,g)
         call init_props(m_ind)
         call patch(m_ind)
         call init(MD_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       subroutine MHD_3D_LDC_BC_symmetric_fine_top(m_mom,m_ind,MQP,MD_sigma,DP,tw,include_vacuum)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         real(cp),intent(in) :: tw
         logical,intent(in) :: include_vacuum
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp),dimension(3) :: hmin,center,hmax,beta,beta_y
         integer,dimension(3) :: N
         integer :: N_top_half,N_bottom_half
         real(cp) :: buffer,buffer_y
         integer :: i,N_w,N_v,N_extra
         real(cp) :: tf,Gamma_v
         call delete(m_mom)
         buffer = 1.0_cp
         buffer_y = 1.0_cp
         hmin = -1.0_cp; hmax = 1.0_cp; center = 0.0_cp
         hmax(3) = 0.0_cp
         beta = Re_Ha_BL(DP%Re*buffer,DP%Ha*buffer,hmin,hmax)
         beta_y = Re_Ha_BL(DP%Re*buffer_y,DP%Ha*buffer_y,hmin,hmax)

         ! N = (/30,30,15/); N_w = 8; N_v = 14 ! Coarse
         ! N = (/40,40,20/); N_w = 10; N_v = 16 ! Moderates
         ! N = (/50,50,25/); N_w = 12; N_v = 18 ! Fine
         N = 30; N(3)=N(1)/2; N_w = 4;  N_v = 8 ! Physics search mesh

         ! New MRS:
         ! N = 30; N(3)=N(1)/2; N_w = 8;  N_v = 14 ! Coarse
         ! N = 50; N(3)=N(1)/2; N_w = 12; N_v = 18 ! Moderates
         ! N = 70; N(3)=N(1)/2; N_w = 16; N_v = 22 ! Fine

         ! High Ha
         ! N = 58; N(3)=N(1)/2; N_w = 14; N_v = 22 ! Fine

         ! Ha=1000
         ! N = 70; N(3)=N(1)/2; N_w = 20; N_v = 24 ! Fine

         ! Ha=500
         ! N = 60; N(3)=N(1)/2; N_w = 16; N_v = 22 ! Fine

         N_extra = N_w ! since no wall domain above lid
         N_top_half = ceiling(N(2)/2.0_cp)+5
         N_bottom_half = ceiling(N(2)/2.0_cp)

         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_L(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)

         i = 2; call grid_Roberts_R(g,center(i),hmax(i),N_top_half,beta_y(i),i,MQP)
         i = 2; call ext_prep_Roberts_C2F_IO(g,center(i)-hmin(i),N_bottom_half,i,MQP)

         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call delete(m_ind)
         call init(g,m_mom%B(1)%g)
         Gamma_v = 7.0_cp
         tf = 1.0_cp
         ! Wall
         call ext_Roberts_B_IO(g,tw,N_w,1,MQP)
         call ext_prep_Roberts_B_IO(g,tw,N_w,3,MQP)
         call ext_prep_Roberts_B_IO(g,tw,N_w,2,MQP)
         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call init_props(m_sigma)
         call patch(m_sigma)
         ! Vacuum
         if (include_vacuum) then
           call ext_Roberts_near_IO  (g,Gamma_v - tw - tf,N_v   ,1,MQP) ! x-direction
           call ext_prep_Roberts_R_IO(g,Gamma_v - tw - tf,N_v   ,3,MQP) ! z-direction
           call ext_prep_Roberts_R_IO(g,Gamma_v - tw - tf,N_v   ,2,MQP) ! y-direction
           call ext_app_Roberts_L_IO (g,Gamma_v - tf,N_v+N_extra,2,MQP) ! y-direction
         endif
         call add(m_ind,g)
         call init_props(m_ind)
         call patch(m_ind)
         call init(MD_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       end module