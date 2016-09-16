       module mesh_simple_geometries_mod
       use current_precision_mod
       use grid_init_mod
       use grid_extend_mod
       use grid_connect_mod
       use grid_distribution_funcs_mod
       use grid_mod
       use domain_mod
       use mesh_mod
       implicit none

       private
       public :: BC_sim_mom,BC_sim_ind
       public :: cube_uniform,extend_cube_uniform
       public :: cube,extend_cube
       public :: straight_duct_fluid
       public :: Hunt_duct_magnetic
       public :: Shercliff_duct_magnetic

       contains

       subroutine BC_sim_mom(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         N = (/30,30,30/); hmin = -1.0_cp; hmax = 1.0_cp ! For Ha = 20
         ! N = (/32,32,32/); hmin = -1.0_cp; hmax = 1.0_cp ! For Ha = 100
         beta = hartmannBL(20.0_cp,hmin,hmax)
         ! beta = hartmannBL(100.0_cp,hmin,hmax)
         call grid_Roberts_B(g,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_B(g,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_Roberts_B(g,hmin(3),hmax(3),N(3),beta(3),3)
         call add(m,g)

         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine BC_sim_ind(m_ind,m_mom,D_sigma)
         implicit none
         type(mesh),intent(inout) :: m_ind
         type(mesh),intent(in) :: m_mom
         type(domain),intent(inout) :: D_sigma
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp) :: tw,tf
         real(cp) :: Gamma_f,Gamma_w,Gamma_v
         integer :: N_w,N_v,N_extra
         call delete(m_ind)
         call init(g,m_mom%g(1))

         Gamma_f = 1.0_cp
         Gamma_w = Gamma_f + tw
         Gamma_v = 7.0_cp
         tf = 1.0_cp

         ! tw = 0.5_cp
         ! N_w = 8 ! For Ha = 20
         ! N_w = 10 ! For Ha = 100

         tw = 0.05_cp
         N_w = 4 ! For Ha = 20
         ! N_w = 6 ! For Ha = 100

         N_v = 12
         N_extra = 6 ! since no wall domain above lid

         ! Wall
         call ext_Roberts_B_IO(g,tw,N_w,1)
         call ext_Roberts_B_IO(g,tw,N_w,3)
         call ext_prep_Roberts_B_IO(g,tw,N_w,2)

         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)

         ! Vacuum
         ! Remove the following 4 lines for vacuum-absent case
         call ext_Roberts_near_IO(g,Gamma_v - tw - tf,N_v,1) ! x-direction
         call ext_Roberts_near_IO(g,Gamma_v - tw - tf,N_v,3) ! z-direction
         call ext_prep_Roberts_R_IO(g,Gamma_v - tw - tf,N_v,2) ! y-direction
         call ext_app_Roberts_L_IO (g,Gamma_v - tf,N_v+N_extra,2) ! y-direction

         call add(m_ind,g)
         call initProps(m_ind)
         call patch(m_ind)

         call init(D_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       subroutine cube_uniform(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax
         integer,dimension(3) :: N
         call delete(m)
         N = (/45,45,45/)
         hmin = -1.0_cp; hmax = 1.0_cp
         ! hmin(1) = -1.0_cp; hmax(1) = 1.0_cp
         ! hmin(3) = -0.5_cp; hmax(3) = 0.5_cp

         call grid_uniform(g,hmin(1),hmax(1),N(1),1)
         call grid_uniform(g,hmin(2),hmax(2),N(2),2)
         call grid_uniform(g,hmin(3),hmax(3),N(3),3)
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
         real(cp) :: Ha,Re
         Ha = 10.0_cp; Re = 1000.0_cp
         call delete(m)
         N = (/50,50,1/); hmin = -0.5_cp; hmax = 0.5_cp
         beta = reynoldsBL(Re,hmin,hmax)
         beta = hartmannBL(Ha,hmin,hmax)

         ! call grid_uniform(g,hmin(1),hmax(1),N(1),1)
         ! call grid_uniform(g,hmin(2),hmax(2),N(2),2)
         ! call grid_uniform(g,hmin(3),hmax(3),N(3),3)

         call grid_Roberts_B(g,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_B(g,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_uniform(g,hmin(3),hmax(3),N(3),3)
         ! call grid_Roberts_B(g,hmin(3),hmax(3),N(3),beta(3),3)
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
         integer,dimension(3) :: N
         call delete(m)
         call init(g,m_in%g(1))
         N = 11
         call ext_uniform_IO(g,N(1),1)
         call ext_uniform_IO(g,N(2),2)
         call ext_uniform_IO(g,N(3),3)
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
         integer,dimension(3) :: N
         real(cp),dimension(3) :: L
         call delete(m)
         call init(g,m_in%g(1))
         N = 2; L = 1.0_cp
         call ext_Roberts_near_IO(g,L(1),N(1),1)
         call ext_Roberts_near_IO(g,L(2),N(2),2)
         call ext_Roberts_near_IO(g,L(3),N(3),3)
         call init(m,g)
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
         real(cp) :: Ha,Re
         Ha = 500.0_cp; Re = 100.0_cp
         call delete(m)
         N = (/68,45,45/); hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = 0.0_cp; hmax(1) = 60.0_cp
         beta = reynoldsBL(Re,hmin,hmax)
         beta = hartmannBL(Ha,hmin,hmax)

         call grid_uniform(g,hmin(1),hmax(1),N(1),1)
         call grid_Roberts_B(g,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_Roberts_B(g,hmin(3),hmax(3),N(3),beta(3),3)

         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine Hunt_duct_magnetic(m_ind,m_mom,D_sigma)
         implicit none
         type(mesh),intent(inout) :: m_ind
         type(mesh),intent(in) :: m_mom
         type(domain),intent(inout) :: D_sigma
         type(mesh) :: m_sigma
         type(grid) :: g
         real(cp) :: tw
         integer :: N_w
         call delete(m_ind)
         call init(g,m_mom%g(1))
         tw = 0.01_cp
         N_w = 3
         ! Wall
         call ext_Roberts_near_IO(g,tw,N_w,2) ! Comment / uncomment for Shercliff / Hunt flow
         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)
         ! Define domain for magnetic field domain
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
         type(domain),intent(inout) :: D_sigma
         type(mesh) :: m_sigma
         type(grid) :: g
         integer :: N_w
         call delete(m_ind)
         call init(g,m_mom%g(1))
         N_w = 3
         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)
         ! Define domain for magnetic field domain
         call add(m_ind,g)
         call initProps(m_ind)
         call patch(m_ind)

         call init(D_sigma,m_sigma,m_ind)
         call delete(m_sigma)
         call delete(g)
       end subroutine

       end module