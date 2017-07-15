       module mesh_benchmark_geometries_mod
       use current_precision_mod
       use grid_init_mod
       use grid_extend_mod
       use grid_connect_mod
       use coordinate_stretch_parameters_mod
       use grid_mod
       use mesh_domain_mod
       use mesh_mod
       use constants_mod
       use mesh_PD_geometries_mod
       use mesh_BC_geometries_mod
       use mesh_complex_BC_geometries_mod
       use mesh_quality_params_mod
       use dimensionless_params_mod
       implicit none

       private
       public :: geometry_BMC

       contains

       subroutine geometry_BMC(m_mom,m_ind,MQP,MD_sigma,DP,tw,include_vacuum,preset_ID)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_domain),intent(inout) :: MD_sigma
         type(mesh_quality_params),intent(in) :: MQP
         type(dimensionless_params),intent(in) :: DP
         real(cp),intent(in) :: tw
         logical,intent(in) :: include_vacuum
         integer,intent(in) :: preset_ID
         select case (preset_ID)
         case (1);  call Hydro_2D_LDC_Ghia(        m_mom,m_ind,MQP,DP)
         case (2);  call Hydro_2D_duct_along_x(    m_mom,m_ind,MQP,DP)
         case (3);  call Hydro_3D_LDC_Guj_stella(  m_mom,m_ind,MQP,DP)
         case (4);  call Hydro_3D_duct_along_x(    m_mom,m_ind,MQP,DP)
         case (5);  call kinetic_MHD_2D_Weiss(     m_mom,m_ind,MD_sigma)
         case (6);  call kinetic_MHD_2D_Parker(    m_mom,m_ind,MD_sigma)
         case (7);  call MHD_2D_Bandaru(           m_mom,m_ind,MQP,MD_sigma,DP)
         case (8);  call MHD_2D_LDC_Shatrov(       m_mom,m_ind,MQP,MD_sigma,DP)
         case (9);  call MHD_3D_LDC_Sergey_uniform(m_mom,m_ind,MD_sigma)
         case (10); call MHD_3D_LDC_Sergey(        m_mom,m_ind,MQP,MD_sigma,DP)
         case (11); call MHD_3D_Shercliff(         m_mom,m_ind,MQP,MD_sigma,DP)
         case (12); call MHD_3D_Hunt(              m_mom,m_ind,MQP,MD_sigma,DP)
         case (13); call MHD_3D_LDC_BC(            m_mom,m_ind,MQP,MD_sigma,DP,tw,include_vacuum)
         case (14); call MHD_3D_LDC_BC_symmetric(  m_mom,m_ind,MQP,MD_sigma,DP,tw,include_vacuum)
         case (15); call MHD_3D_LDC_BC_symmetric_fine_top(m_mom,m_ind,MQP,MD_sigma,DP,tw,include_vacuum)
         case (16); call MHD_3D_NSC_PD(            m_mom,m_ind,MQP,MD_sigma,DP)
         case (17); call MHD_3D_LDC_Salah(         m_mom,m_ind,MQP,MD_sigma,DP)
         case (18); call MHD_3D_LDC_BC_symmetric_fine_top_new(m_mom,m_ind,MQP,MD_sigma,DP,tw,include_vacuum)
         case (19); call user_defined(             m_mom,m_ind,MQP,MD_sigma,DP)
         case (20); call Hydro_3D_LDC_Leriche(     m_mom,m_ind,MQP,DP)
         case (21); call MHD_3D_2_channel_PD(      m_mom,m_ind,MQP,MD_sigma,DP)
         case (22); call MHD_3D_LDC_Shatrov(       m_mom,m_ind,MQP,MD_sigma,DP)
         case (23); call MHD_3D_Duct_Kinet(        m_mom,m_ind,MQP,MD_sigma,DP)
         case (24); call MHD_2D_Duct_Hunt(         m_mom,m_ind,MQP,MD_sigma,DP)
         case (25); call MHD_2D_Duct_Hunt_Walls(   m_mom,m_ind,MQP,MD_sigma,DP)
         case (26); call MHD_3D_Bandaru(           m_mom,m_ind,MQP,MD_sigma,DP)
         case (27); call MHD_3D_LDC_BC_fine_top(   m_mom,m_ind,MQP,MD_sigma,DP,tw,include_vacuum)
         case (28); call MHD_2D_Shercliff(         m_mom,m_ind,MQP,MD_sigma,DP)
         case (29); call MHD_2D_Hunt(              m_mom,m_ind,MQP,MD_sigma,DP)
         case (30); call Hydro_2D_Taylor_Green_Vortex_z(m_mom,m_ind,MQP,DP)
         case default; stop 'Error: bad BMC_geometry in mesh_benchmark_geometries.f90'
         end select
       end subroutine

       ! ********************************************************************
       ! ***************************** HYDRO ********************************
       ! ********************************************************************

       subroutine Hydro_2D_LDC_Ghia(m_mom,m_ind,MQP,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         N = 60; hmin = -0.5_cp; hmax = 0.5_cp
         N(3) = 1
         beta = ReynoldsBL(DP%Re,hmin,hmax)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
       end subroutine

       subroutine Hydro_2D_duct_along_x(m_mom,m_ind,MQP,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = 0.0_cp; hmax(1) = 60.0_cp
         N = (/70,20,1/)
         beta = ReynoldsBL(DP%Re,hmin,hmax)
         i = 1; call grid_Roberts_L(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
       end subroutine

       subroutine Hydro_2D_Taylor_Green_Vortex_z(m_mom,m_ind,MQP,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         hmin = 0.0_cp; hmax = 2.0_cp*PI
         hmin(3) = -0.5_cp; hmax(3) = 0.5_cp
         N = (/45,45,1/)
         beta = ReynoldsBL(DP%Re,hmin,hmax)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
       end subroutine

       subroutine Hydro_3D_LDC_Guj_stella(m_mom,m_ind,MQP,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         N = 60; hmin = -0.5_cp; hmax = 0.5_cp
         beta = ReynoldsBL(DP%Re,hmin,hmax)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
       end subroutine

       subroutine Hydro_3D_LDC_Leriche(m_mom,m_ind,MQP,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         N = 60; hmin = -1.0_cp; hmax = 1.0_cp
         beta = ReynoldsBL(DP%Re,hmin,hmax)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
       end subroutine

       subroutine Hydro_3D_duct_along_x(m_mom,m_ind,MQP,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         hmin = -0.5_cp; hmax = 0.5_cp
         hmin(1) = 0.0_cp; hmax(1) = 60.0_cp
         N = (/100,60,60/)
         beta = ReynoldsBL(DP%Re,hmin,hmax)
         i = 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
       end subroutine

       ! ********************************************************************
       ! ************************** KINEMATIC MHD ***************************
       ! ********************************************************************

       subroutine kinetic_MHD_2D_Weiss(m_mom,m_ind,MD_sigma)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_domain),intent(inout) :: MD_sigma
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         N = (/45,45,1/)
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(3) = 0.5_cp
         hmax(3) = 0.5_cp
         i = 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine kinetic_MHD_2D_Parker(m_mom,m_ind,MD_sigma)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_domain),intent(inout) :: MD_sigma
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         N = (/45,45,1/)
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(3) = 0.5_cp
         hmax(3) = 0.5_cp
         i = 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       ! ********************************************************************
       ! ******************************* 2D MHD *****************************
       ! ********************************************************************

       subroutine MHD_2D_LDC_Shatrov(m_mom,m_ind,MQP,MD_sigma,DP)
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
         N = 60; hmin = -0.5_cp; hmax = 0.5_cp
         N(3) = 1
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine MHD_2D_Duct_Hunt(m_mom,m_ind,MQP,MD_sigma,DP)
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
         N = (/1,40,40/)
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = -0.5_cp; hmax(1) = 0.5_cp
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i = 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call init(m_ind,m_mom)
         call delete(g)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine MHD_2D_Duct_Hunt_Walls(m_mom,m_ind,MQP,MD_sigma,DP)
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
         N = (/1,40,40/)
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = -0.5_cp; hmax(1) = 0.5_cp
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i = 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call init(g,m_mom%B(1)%g)
         N = 5
         i = 2; call ext_Roberts_near_IO(g,DP%c_w(1),N(i),i,MQP)
         i = 3; call ext_Roberts_near_IO(g,DP%c_w(1),N(i),i,MQP)
         call init(m_ind,g)
         call init_props(m_ind)
         call patch(m_ind)
         call delete(g)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine MHD_2D_Bandaru(m_mom,m_ind,MQP,MD_sigma,DP)
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
         N = (/64,1,64/);   hmin = -1.0_cp; hmax = 1.0_cp ! Approximate
         ! N = (/129,1,129/); hmin = -1.0_cp; hmax = 1.0_cp ! DNS
         hmin(2) = -0.5_cp; hmax(2) = 0.5_cp
         hmin(1) = 0.0_cp; hmax(1) = 2.0_cp*PI
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i = 1; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine MHD_2D_Shercliff(m_mom,m_ind,MQP,MD_sigma,DP)
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
         N = (/1,50,50/)   ! Ha = 500
         ! N = (/1,60,60/)   ! Ha = 5000
         ! N = (/1,80,80/)   ! Ha = 10000
         ! N = (/1,100,100/) ! Ha = 15000
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = -0.5_cp; hmax(1) = 0.5_cp
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i = 1; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine MHD_2D_Hunt(m_mom,m_ind,MQP,MD_sigma,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N,N_wall
         real(cp) :: t_wall
         integer :: i
         call delete(m_mom); call delete(m_ind)
         ! N = (/1,50,50/);   N_wall = 10 ! Ha = 500
         N = (/1,80,80/);   N_wall = 10 ! Ha = 5000
         ! N = (/1,80,80/);   N_wall = 10 ! Ha = 10000
         ! N = (/1,100,100/); N_wall = 10 ! Ha = 15000
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = -0.5_cp; hmax(1) = 0.5_cp
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i = 1; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
         call init(g,m_mom%B(1)%g)
         t_wall = 0.01_cp
         i = 3; call ext_Roberts_near_IO(g,t_wall,N_wall(i),i,MQP)
         call init(m_ind,g)
         call init_props(m_ind)
         call patch(m_ind)
         call delete(g)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       ! ********************************************************************
       ! ******************************* 3D MHD *****************************
       ! ********************************************************************

       subroutine MHD_3D_LDC_Shatrov(m_mom,m_ind,MQP,MD_sigma,DP)
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
         N = 60; hmin = -0.5_cp; hmax = 0.5_cp
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
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

       subroutine MHD_3D_Duct_Kinet(m_mom,m_ind,MQP,MD_sigma,DP)
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
         N = (/512,100,100/)
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = 0.0_cp
         hmax(1) = 4.0_cp*PI
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i = 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine MHD_3D_Bandaru(m_mom,m_ind,MQP,MD_sigma,DP)
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
         N = (/256,192,192/)
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = 0.0_cp
         hmax(1) = 4.0_cp*PI
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i = 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine MHD_3D_LDC_Sergey_uniform(m_mom,m_ind,MD_sigma)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_domain),intent(inout) :: MD_sigma
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax
         integer :: i
         integer,dimension(3) :: N
         call delete(m_mom); call delete(m_ind)
         N = 45; hmin = -1.0_cp; hmax =  1.0_cp
         i = 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call init(g,m_mom%B(1)%g)
         N = 11
         i = 1; call ext_uniform_IO(g,N(i),i)
         i = 2; call ext_uniform_IO(g,N(i),i)
         i = 3; call ext_uniform_IO(g,N(i),i)
         call init(m_ind,g)
         call init_props(m_ind)
         call patch(m_ind)
         call delete(g)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine MHD_3D_LDC_Sergey(m_mom,m_ind,MQP,MD_sigma,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer :: i
         integer,dimension(3) :: N
         call delete(m_mom); call delete(m_ind)
         N = 45; hmin = -1.0_cp; hmax =  1.0_cp
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i= 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i= 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i= 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call init(g,m_mom%B(1)%g)
         N = 11
         i = 1; call ext_uniform_IO(g,N(i),i)
         i = 2; call ext_uniform_IO(g,N(i),i)
         i = 3; call ext_uniform_IO(g,N(i),i)
         call init(m_ind,g)
         call init_props(m_ind)
         call patch(m_ind)
         call delete(g)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine MHD_3D_LDC_Salah(m_mom,m_ind,MQP,MD_sigma,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer :: i
         integer,dimension(3) :: N
         call delete(m_mom); call delete(m_ind)
         N = 50; hmin = -0.5_cp; hmax =  0.5_cp
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i= 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i= 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i= 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call init(g,m_mom%B(1)%g)
         call init(m_ind,g)
         call init_props(m_ind)
         call patch(m_ind)
         call delete(g)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine MHD_3D_Shercliff(m_mom,m_ind,MQP,MD_sigma,DP)
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
         N = (/200,50,50/); hmin = -0.5_cp; hmax = 0.5_cp
         hmin(1) = 0.0_cp; hmax(1) = 80.0_cp
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i = 1; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine MHD_3D_Hunt(m_mom,m_ind,MQP,MD_sigma,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom); call delete(m_ind)
         N = (/200,50,50/); hmin = -0.5_cp; hmax = 0.5_cp
         hmin(1) = 0.0_cp; hmax(1) = 80.0_cp
         beta = Re_Ha_BL(DP%Re,DP%Ha,hmin,hmax)
         i = 1; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
         call init(g,m_mom%B(1)%g)
         N = 11
         i = 3; call ext_uniform_IO(g,N(i),i)
         call init(m_ind,g)
         call init_props(m_ind)
         call patch(m_ind)
         call delete(g)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       subroutine user_defined(m_mom,m_ind,MQP,MD_sigma,DP)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Re
         Re = 400.0_cp
         call delete(m_mom)
         N = (/60,60,1/); hmin = -0.5_cp; hmax = 0.5_cp
         beta = ReynoldsBL(DP%Re,hmin,hmax)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
         call init(MD_sigma,m_mom,m_ind)
       end subroutine

       end module