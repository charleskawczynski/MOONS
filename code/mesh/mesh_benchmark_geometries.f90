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
       implicit none

       private
       public :: hydro_2D_Ghia_Re_400
       public :: hydro_3D_Guj_Stella_Re_400
       public :: hydro_3D_Guj_Stella_Re_1000

       public :: kinematic_2D_mhd_Parker
       public :: kinematic_2D_mhd_Weiss

       public :: mhd_2D_Bandaru
       public :: mhd_3D_BMC_102,mhd_3D_BMC_102_extend
       public :: mhd_3D_BMC_103_mom,mhd_3D_BMC_103_ind
       public :: mhd_3D_Shercliff_Ha500
       public :: mhd_3D_Hunt_Ha500
       public :: mhd_3D_Hunt_Ha500_extend

       contains

       ! ********************************************************************
       ! ***************************** HYDRO ********************************
       ! ********************************************************************

       subroutine hydro_3D_Guj_Stella_Re_400(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Re
         Re = 400.0_cp
         call delete(m)
         N = 30; hmin = -0.5_cp; hmax = 0.5_cp
         beta = ReynoldsBL(Re,hmin,hmax)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine hydro_3D_Guj_Stella_Re_1000(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Re
         Re = 1000.0_cp
         call delete(m)
         N = 60; hmin = -0.5_cp; hmax = 0.5_cp
         beta = ReynoldsBL(Re,hmin,hmax)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine hydro_2D_Ghia_Re_400(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Re
         Re = 400.0_cp
         call delete(m)
         N = 60; hmin = -0.5_cp; hmax = 0.5_cp
         beta = ReynoldsBL(Re,hmin,hmax)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       ! ********************************************************************
       ! ************************** KINEMATIC MHD ***************************
       ! ********************************************************************

       subroutine kinematic_2D_mhd_Weiss(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax
         integer,dimension(3) :: N
         integer :: i
         call delete(m)
         N = (/45,45,1/)
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(3) = 0.5_cp
         hmax(3) = 0.5_cp
         i = 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine kinematic_2D_mhd_Parker(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax
         integer,dimension(3) :: N
         integer :: i
         call delete(m)
         N = (/45,45,1/)
         hmin = -1.0_cp; hmax = 1.0_cp
         hmin(3) = 0.5_cp
         hmax(3) = 0.5_cp
         i = 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       ! ********************************************************************
       ! ******************************* MHD ********************************
       ! ********************************************************************

       subroutine mhd_3D_BMC_102(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax
         integer :: i
         integer,dimension(3) :: N
         call delete(m)
         N = 45; hmin = -1.0_cp; hmax =  1.0_cp
         i= 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i= 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i= 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine
       subroutine mhd_3D_BMC_102_extend(m,m_in)
         implicit none
         type(mesh),intent(inout) :: m
         type(mesh),intent(in) :: m_in
         type(grid) :: g
         integer :: i
         integer,dimension(3) :: N
         call delete(m)
         call init(g,m_in%B(1)%g)
         N = 11
         i = 1; call ext_uniform_IO(g,N(i),i)
         i = 2; call ext_uniform_IO(g,N(i),i)
         i = 3; call ext_uniform_IO(g,N(i),i)
         call init(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine mhd_3D_BMC_103_mom(m,Ha)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: Ha
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer :: i
         integer,dimension(3) :: N
         call delete(m)
         N = 30 ! For full
         ! N = 15 ! For refinement
         hmin = -1.0_cp; hmax =  1.0_cp
         beta = HartmannBL(Ha,hmin,hmax)
         i= 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i= 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i= 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine
       subroutine mhd_3D_BMC_103_ind(m_ind,m_mom,MD_sigma)
         implicit none
         type(mesh),intent(inout) :: m_ind
         type(mesh),intent(in) :: m_mom
         type(mesh_domain),intent(inout) :: MD_sigma
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp),dimension(3) :: L
         integer :: i
         integer,dimension(3) :: N
         call delete(m_ind)
         call init(g,m_mom%B(1)%g)
         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)
         N = 4 ! For full
         ! N = 3 ! For refinement
         L = 0.1_cp
         i = 1; call ext_Roberts_near_IO(g,L(i),N(i),i)
         i = 2; call ext_Roberts_near_IO(g,L(i),N(i),i)
         i = 3; call ext_Roberts_near_IO(g,L(i),N(i),i)
         call init(m_ind,g)
         call initProps(m_ind)
         call patch(m_ind)
         call init(MD_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       subroutine mhd_2D_Bandaru(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Ha
         call delete(m)
         Ha = 26.0_cp
         N = (/64,1,64/); hmin = -1.0_cp; hmax = 1.0_cp
         hmin(2) = -0.5_cp; hmax(2) = 0.5_cp
         hmin(1) = 0.0_cp; hmax(1) = 2.0_cp*PI
         beta = HartmannBL(Ha,hmin,hmax)
         i = 1; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine mhd_3D_Shercliff_Ha500(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Ha
         Ha = 500.0_cp
         call delete(m)
         N = (/200,50,1/); hmin = -0.5_cp; hmax = 0.5_cp
         hmin(1) = 0.0_cp; hmax(1) = 80.0_cp
         beta = HartmannBL(Ha,hmin,hmax)
         i = 1; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 3; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine mhd_3D_Hunt_Ha500(m)
         implicit none
         type(mesh),intent(inout) :: m
         call mhd_3D_Shercliff_Ha500(m)
       end subroutine

       subroutine mhd_3D_Hunt_Ha500_extend(m,m_in)
         implicit none
         type(mesh),intent(inout) :: m
         type(mesh),intent(in) :: m_in
         type(grid) :: g
         integer :: i
         integer,dimension(3) :: N
         call delete(m)
         call init(g,m_in%B(1)%g)
         N = 11
         i = 3; call ext_uniform_IO(g,N(i),i)
         call init(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       end module