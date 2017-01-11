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
       use mesh_quality_params_mod
       implicit none

       private
       public :: geometry_BMC

       contains

       subroutine geometry_BMC(m_mom,m_ind,MQP,MD_sigma,Re,Ha,tw,include_vacuum,preset_ID)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_domain),intent(inout) :: MD_sigma
         type(mesh_quality_params),intent(in) :: MQP
         real(cp),intent(in) :: Re,Ha,tw
         logical,intent(in) :: include_vacuum
         integer,intent(in) :: preset_ID
         select case (preset_ID)
         case (1);  call Hydro_2D_LDC_Ghia(        m_mom,m_ind,MQP,Re)
         case (2);  call Hydro_2D_duct_along_x(    m_mom,m_ind,MQP,Re)
         case (3);  call Hydro_3D_LDC_Guj_stella(  m_mom,m_ind,MQP,Re)
         case (4);  call Hydro_3D_duct_along_x(    m_mom,m_ind,MQP,Re)
         case (5);  call kinetic_MHD_2D_Weiss(     m_mom,m_ind,MD_sigma)
         case (6);  call kinetic_MHD_2D_Parker(    m_mom,m_ind,MD_sigma)
         case (7);  call MHD_2D_Bandaru(           m_mom,m_ind,MQP,MD_sigma,Re,Ha)
         case (8);  call MHD_3D_LDC_Sergey_uniform(m_mom,m_ind,MD_sigma)
         case (9);  call MHD_3D_LDC_Sergey(        m_mom,m_ind,MQP,MD_sigma,Re,Ha)
         case (10); call MHD_3D_Shercliff(         m_mom,m_ind,MQP,MD_sigma,Re,Ha)
         case (11); call MHD_3D_Hunt(              m_mom,m_ind,MQP,MD_sigma,Re,Ha)
         case (12); call MHD_3D_LDC_BC(            m_mom,m_ind,MQP,MD_sigma,Re,Ha,tw,include_vacuum)
         case (13); call MHD_3D_LDC_BC_symmetric(  m_mom,m_ind,MQP,MD_sigma,Re,Ha,tw,include_vacuum)
         case (14); call MHD_3D_NSC_PD(            m_mom,m_ind,MQP,MD_sigma,Re,Ha)
         case (15); call user_defined(             m_mom,m_ind,MQP,MD_sigma)
         case default; stop 'Error: bad BMC_geometry in mesh_benchmark_geometries.f90'
         end select
       end subroutine

       ! ********************************************************************
       ! ***************************** HYDRO ********************************
       ! ********************************************************************

       subroutine Hydro_2D_LDC_Ghia(m_mom,m_ind,MQP,Re)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         real(cp),intent(in) :: Re
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         N = (/60,60,1/); hmin = -0.5_cp; hmax = 0.5_cp
         beta = ReynoldsBL(Re,hmin,hmax)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
       end subroutine

       subroutine Hydro_2D_duct_along_x(m_mom,m_ind,MQP,Re)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         real(cp),intent(in) :: Re
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = 0.0_cp; hmax(1) = 30.0_cp
         N = (/50,20,1/)
         beta = ReynoldsBL(Re,hmin,hmax)
         i = 1; call grid_Roberts_L(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
       end subroutine

       subroutine Hydro_3D_LDC_Guj_stella(m_mom,m_ind,MQP,Re)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         real(cp),intent(in) :: Re
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         N = 30; hmin = -0.5_cp; hmax = 0.5_cp
         beta = ReynoldsBL(Re,hmin,hmax)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m_mom,g)
         call init_props(m_mom)
         call patch(m_mom)
         call delete(g)
         call init(m_ind,m_mom)
       end subroutine

       subroutine Hydro_3D_duct_along_x(m_mom,m_ind,MQP,Re)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         real(cp),intent(in) :: Re
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         hmin = -0.5_cp; hmax = 0.5_cp
         hmin(1) = 0.0_cp; hmax(1) = 60.0_cp
         N = (/100,60,60/)
         beta = ReynoldsBL(Re,hmin,hmax)
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
       ! ******************************* MHD ********************************
       ! ********************************************************************

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
         i= 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i= 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i= 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
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

       subroutine MHD_3D_LDC_Sergey(m_mom,m_ind,MQP,MD_sigma,Re,Ha)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         real(cp),intent(in) :: Re,Ha
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer :: i
         integer,dimension(3) :: N
         call delete(m_mom); call delete(m_ind)
         N = 45; hmin = -1.0_cp; hmax =  1.0_cp
         beta = Re_Ha_BL(Re,Ha,hmin,hmax)
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

       subroutine MHD_2D_Bandaru(m_mom,m_ind,MQP,MD_sigma,Re,Ha)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         real(cp),intent(in) :: Re,Ha
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         N = (/64,1,64/); hmin = -1.0_cp; hmax = 1.0_cp
         hmin(2) = -0.5_cp; hmax(2) = 0.5_cp
         hmin(1) = 0.0_cp; hmax(1) = 2.0_cp*PI
         beta = Re_Ha_BL(Re,Ha,hmin,hmax)
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

       subroutine mhd_3D_Shercliff(m_mom,m_ind,MQP,MD_sigma,Re,Ha)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         real(cp),intent(in) :: Re,Ha
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom)
         N = (/200,50,1/); hmin = -0.5_cp; hmax = 0.5_cp
         hmin(1) = 0.0_cp; hmax(1) = 80.0_cp
         beta = Re_Ha_BL(Re,Ha,hmin,hmax)
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

       subroutine mhd_3D_Hunt(m_mom,m_ind,MQP,MD_sigma,Re,Ha)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         real(cp),intent(in) :: Re,Ha
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call delete(m_mom); call delete(m_ind)
         N = (/200,50,1/); hmin = -0.5_cp; hmax = 0.5_cp
         hmin(1) = 0.0_cp; hmax(1) = 80.0_cp
         beta = Re_Ha_BL(Re,Ha,hmin,hmax)
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

       subroutine user_defined(m_mom,m_ind,MQP,MD_sigma)
         implicit none
         type(mesh),intent(inout) :: m_mom,m_ind
         type(mesh_quality_params),intent(in) :: MQP
         type(mesh_domain),intent(inout) :: MD_sigma
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Re
         Re = 400.0_cp
         call delete(m_mom)
         N = (/60,60,1/); hmin = -0.5_cp; hmax = 0.5_cp
         beta = ReynoldsBL(Re,hmin,hmax)
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