       module mesh_simple_geometries_mod
       use current_precision_mod
       use grid_init_mod
       use grid_extend_mod
       use grid_connect_mod
       use coordinate_stretch_parameters_mod
       use grid_mod
       use mesh_domain_mod
       use mesh_mod
       implicit none

       private
       public :: cube_uniform,cube
       public :: square
       public :: extend_cube_uniform,extend_cube
       public :: matrix_export_mesh

       public :: straight_duct_fluid
       public :: Hunt_duct_magnetic
       public :: Shercliff_duct_magnetic
       public :: duct_with_vacuum

       real(cp),parameter :: PI = 3.141592653589793238462643383279502884197169399375105820974_cp

       contains

       subroutine cube_uniform(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax
         integer :: i
         integer,dimension(3) :: N
         call delete(m)
         ! N = (/45,45,45/); hmin = -1.0_cp; hmax = 1.0_cp
         ! N = 45; hmin = -1.0_cp; hmax = 1.0_cp
         N = 32; hmin = -0.5_cp; hmax = 0.5_cp
         ! N = 64; hmin = -0.5_cp; hmax = 0.5_cp
         ! hmin = -0.5_cp; hmax = 0.5_cp
         ! hmin(1) = -1.0_cp; hmax(1) = 1.0_cp
         ! hmin(3) = -0.5_cp; hmax(3) = 0.5_cp

         i= 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i= 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i= 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         ! call init(m,g)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine cube(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Ha,Re
         Ha = 100.0_cp; Re = 400.0_cp
         call delete(m)
         ! N = (/45,45,45/); hmin = -1.0_cp; hmax = 1.0_cp
         N = 16; hmin = -0.5_cp; hmax = 0.5_cp
         beta = ReynoldsBL(Re,hmin,hmax)
         ! beta = HartmannBL(Ha,hmin,hmax)
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine square(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Ha,Re
         Ha = 5.0_cp; Re = 400.0_cp
         call delete(m)
         N = (/45,45,1/); hmin = -0.5_cp; hmax = 0.5_cp
         beta = ReynoldsBL(Re,hmin,hmax)
         ! beta = HartmannBL(Ha,hmin,hmax)

         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i= 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine extend_cube_uniform(m,m_in)
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

       subroutine extend_cube(m,m_in)
         implicit none
         type(mesh),intent(inout) :: m
         type(mesh),intent(in) :: m_in
         type(grid) :: g
         integer :: i
         integer,dimension(3) :: N
         real(cp),dimension(3) :: L
         call delete(m)
         call init(g,m_in%B(1)%g)
         N = 2; L = 1.0_cp
         i = 1; call ext_Roberts_near_IO(g,L(i),N(i),i)
         i = 2; call ext_Roberts_near_IO(g,L(i),N(i),i)
         i = 3; call ext_Roberts_near_IO(g,L(i),N(i),i)
         call init(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine channel_Bandaru(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Ha,Re
         Ha = 26.0_cp; Re = 10.0_cp**(7.0_cp)
         call delete(m)
         N = (/64,1,64/); hmin = -1.0_cp; hmax = 1.0_cp

         hmin(2) = -0.5_cp; hmax(2) = 0.5_cp
         hmin(1) = 0.0_cp; hmax(1) = 2.0_cp*PI
         ! beta = ReynoldsBL(Re,hmin,hmax)
         beta = HartmannBL(Ha,hmin,hmax)

         i = 1; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)

         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine straight_duct_fluid(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         real(cp) :: Ha,Re
         Ha = 60.0_cp; Re = 10.0_cp**(7.0_cp)
         call delete(m)
         N = (/25,40,1/); hmin = -0.5_cp; hmax = 0.5_cp
         hmin(1) = 0.0_cp; hmax(1) = 10.0_cp
         ! beta = ReynoldsBL(Re,hmin,hmax)
         beta = HartmannBL(Ha,hmin,hmax)

         i = 1; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         i = 3; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         ! i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)

         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine duct_with_vacuum(m_ind,m_mom,D_sigma)
         implicit none
         type(mesh),intent(inout) :: m_ind
         type(mesh),intent(in) :: m_mom
         type(mesh_domain),intent(inout) :: D_sigma
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp) :: tw,tf
         real(cp) :: Gamma_f,Gamma_w,Gamma_v
         integer :: N_w,N_v
         call delete(m_ind)
         call init(g,m_mom%B(1)%g)

         Gamma_f = 1.0_cp
         Gamma_w = Gamma_f + tw
         Gamma_v = 7.0_cp
         tf = 1.0_cp

         tw = 0.1_cp
         N_w = 10
         N_v = 15

         ! Wall
         call ext_Roberts_B_IO(g,tw,N_w,2)
         call ext_Roberts_B_IO(g,tw,N_w,3)

         ! Define mesh_domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)

         ! Vacuum
         ! Remove the following 4 lines for vacuum-absent case
         ! call ext_Roberts_near_IO(g,Gamma_v - tw - tf,N_v,2) ! y-direction
         ! call ext_Roberts_near_IO(g,Gamma_v - tw - tf,N_v,3) ! z-direction

         call add(m_ind,g)
         call initProps(m_ind)
         call patch(m_ind)

         call init(D_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine


       subroutine matrix_export_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax
         integer,dimension(3) :: N
         integer :: i
         call delete(m)
         N = (/3,3,3/); hmin = -1.0_cp; hmax = 1.0_cp
         i = 1; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 2; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         i = 3; call grid_uniform(g,hmin(i),hmax(i),N(i),i)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine Hunt_duct_magnetic(m_ind,m_mom,D_sigma)
         implicit none
         type(mesh),intent(inout) :: m_ind
         type(mesh),intent(in) :: m_mom
         type(mesh_domain),intent(inout) :: D_sigma
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp) :: tw
         integer :: N_w
         call delete(m_ind)
         call init(g,m_mom%B(1)%g)
         tw = 0.01_cp
         N_w = 3
         ! Wall
         call ext_Roberts_near_IO(g,tw,N_w,2) ! Comment / uncomment for Shercliff / Hunt flow
         ! Define mesh_domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)
         ! Define mesh_domain for magnetic field mesh_domain
         call add(m_ind,g)
         call initProps(m_ind)
         call patch(m_ind)

         call init(D_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       subroutine Shercliff_duct_magnetic(m_ind,m_mom,D_sigma)
         implicit none
         type(mesh),intent(inout) :: m_ind
         type(mesh),intent(in) :: m_mom
         type(mesh_domain),intent(inout) :: D_sigma
         type(mesh) :: m_sigma
         type(grid) :: g
         integer :: N_w
         call delete(m_ind)
         call init(g,m_mom%B(1)%g)
         N_w = 3
         ! Define mesh_domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)
         ! Define mesh_domain for magnetic field mesh_domain
         call add(m_ind,g)
         call initProps(m_ind)
         call patch(m_ind)

         call init(D_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       end module