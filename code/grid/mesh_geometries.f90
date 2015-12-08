       module geometries_mod
       use grid_init_mod
       use grid_extend_mod
       use grid_connect_mod
       use grid_distribution_funcs_mod
       use grid_mod
       use domain_mod
       use mesh_mod
       implicit none

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: BC_sim_mom,BC_sim_ind
       public :: cube_uniform,extend_cube_uniform
       public :: cube,extend_cube
       public :: ins_elbow
       public :: ins_u_bend
       public :: ins_sudden_Expansion
       public :: ins_sep_channel
       public :: flow_past_square
       ! public :: ins_manifold
       ! public :: cond_elbow
       ! public :: cond_sudden_Expansion
       ! public :: cond_u_bend
       ! public :: cond_manifold


       ! Routines in generateGrid.f90
       !           cavity3D_uniform(g,hmin,hmax,Ni)
       !           cavity3D_nonUniform(g,hmin,hmax,Ni,betai)
       !           cavity3D_uniformBL(g,hmin,hmax,Ni,betai)
       !           extend_uniform(g,g_in,Ntop,Nbot)
       !           extend_nonuniform(g,g_in,ttop,tbot,Ntop,Nbot)
       !           extend_nonuniform_both(g,g_in,ttop,tbot,Ntop,Nbot)
       !           extend_nonuniform_both_safe(g,g_in,ttop,tbot,Ntop,Nbot)


       contains

       ! *********************************************************************
       ! ************************* GENERATE ROUTINES *************************
       ! *********************************************************************

       subroutine BC_sim_mom(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         ! N = (/30,30,15/); hmin = -1.0_cp; hmax = 1.0_cp ! For symmetry
         N = (/30,30,30/); hmin = -1.0_cp; hmax = 1.0_cp ! For full 3D
         ! hmax(3) = 0.0_cp ! Symmetric w.r.t z ! For symmetry
         beta = hartmannBL(20.0_cp,hmin,hmax)
         call grid_Roberts_B(g,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_B(g,hmin(2),hmax(2),N(2),beta(2),2)
         ! call grid_Roberts_L(g,hmin(3),hmax(3),N(3),beta(3),3) ! For symmetry
         call grid_Roberts_B(g,hmin(3),hmax(3),N(3),beta(3),3) ! For full 3D
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
         real(cp) :: tw,tv
         integer :: N
         call delete(m_ind)
         call init(g,m_mom%g(1))
         ! Wall
         N = 8; tw = 0.5_cp
         ! N = 5; tw = 0.05_cp;
         call ext_Roberts_B_IO(g,tw,N,1)
         ! call ext_prep_Roberts_B_IO(g,tw,N,3) ! For symmetry
         call ext_Roberts_B_IO(g,tw,N,3) ! For full 3D
         call ext_prep_Roberts_B_IO(g,tw,N,2)

         ! Define domain for electrical conductivity
         call add(m_sigma,g)
         call initProps(m_sigma)
         call patch(m_sigma)

         ! Vacuum
         tv = 5.0_cp - tw ! Vacuum extends to 6 (hmax = 1 + 5 + tw)
         N = 10
         call ext_Roberts_near_IO(g,tv,N,1)
         call ext_Roberts_near_IO(g,tv,N,3) ! For full 3D
         ! call ext_prep_Roberts_R_IO(g,tv,N,3) ! For symmetry
         call ext_prep_Roberts_R_IO(g,tv,N,2)
         call ext_app_Roberts_L_IO (g,5.0_cp,N+6,2)

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
         N = (/45,45,45/); hmin = -1.0_cp; hmax = 1.0_cp
         call grid_uniform(g,hmin(1),hmax(1),N(1),1)
         call grid_uniform(g,hmin(2),hmax(2),N(2),2)
         call grid_uniform(g,hmin(3),hmax(3),N(3),3)
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
         N = 3; L = 1.0_cp
         call ext_Roberts_near_IO(g,L(1),N(1),1)
         call ext_Roberts_near_IO(g,L(2),N(2),2)
         call ext_Roberts_near_IO(g,L(3),N(3),3)
         call init(m,g)
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
         Ha = 10.0_cp; Re = 100.0_cp
         call delete(m)
         N = (/3,3,3/); hmin = 0.0_cp; hmax = 1.0_cp
         beta = reynoldsBL(Re,hmin,hmax)
         beta = hartmannBL(Ha,hmin,hmax)

         ! call grid_uniform(g,hmin(1),hmax(1),N(1),1)
         ! call grid_uniform(g,hmin(2),hmax(2),N(2),2)
         ! call grid_uniform(g,hmin(3),hmax(3),N(3),3)

         call grid_Roberts_B(g,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_B(g,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_Roberts_B(g,hmin(3),hmax(3),N(3),beta(3),3)
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine ins_elbow(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         N = 40; beta = 1.1_cp
         hmin = 0.0_cp; hmax = 1.0_cp
         call grid_Roberts_R(g1,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_B(g1,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_Roberts_B(g1,hmin(3),hmax(3),N(3),beta(3),3)
         call add(m,g1)

         call con_app_Roberts_B(g2,g1,1.0_cp,N(1),1); call add(m,g2)
         call con_app_Roberts_L(g1,g2,1.0_cp,N(1),2); call add(m,g1)

         call initProps(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
       end subroutine

       subroutine ins_u_bend(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         N = 30
         hmin = 0.0_cp; hmax = 1.0_cp; beta = 1.05_cp
         hmax(1) = 5.0_cp
         call grid_Roberts_R(g1,hmin(1),hmax(1), 40 ,beta(1),1)
         call grid_Roberts_B(g1,hmin(2),hmax(2),N(2),beta(2),2)
         ! call grid_Roberts_B(g1,hmin(3),hmax(3),N(3),beta(3),3)
         call grid_uniform(g1,hmin(3),hmax(3),1,3)
         call add(m,g1)

         call con_app_Roberts_B (g2,g1,1.0_cp,N(1),1); call add(m,g2) ! first corner
         call con_app_Roberts_B (g1,g2,2.0_cp,N(2),2); call add(m,g1) ! Long 
         call con_app_Roberts_B (g2,g1,1.0_cp,N(2),2); call add(m,g2) ! second corner
         call con_prep_Roberts_R(g1,g2,5.0_cp, 40 ,1); call add(m,g1) ! exit

         call initProps(m)
         call patch(m)
         call print(m)
         stop 'Done'
         call delete(g1)
         call delete(g2)
       end subroutine

       subroutine ins_sudden_Expansion(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         N = 40
         hmin = -0.5_cp; hmax = 0.5_cp; beta = 1.1_cp
         hmin(1) = 0.0_cp; hmax(1) = 1.0_cp
         call grid_Roberts_R(g2,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_B(g2,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_Roberts_B(g2,hmin(3),hmax(3),N(3),beta(3),3)
         call add(m,g2)

         call con_app_Roberts_L (g1,g2,5.0_cp,N(1),1); call add(m,g1)
         call con_app_Roberts_B (g2,g1,1.0_cp,N(2),2); call add(m,g2)
         call con_prep_Roberts_B(g2,g1,1.0_cp,N(2),2); call add(m,g2)

         call initProps(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
       end subroutine

       subroutine ins_sep_channel(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2,g3
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         N = 40
         hmin = -0.5_cp; hmax = 0.5_cp; beta = 1.1_cp
         hmin(1) = 0.0_cp; hmax(1) = 1.0_cp
         call grid_Roberts_R(g2,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_B(g2,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_Roberts_B(g2,hmin(3),hmax(3),N(3),beta(3),3)
         call add(m,g2)

         call con_app_Roberts_B (g1,g2,1.0_cp,N(1),1); call add(m,g1) ! entrance
         call con_app_Roberts_L (g2,g1,5.0_cp,N(1),1); call add(m,g2) ! center (long)

         call con_app_Roberts_B (g2,g1,1.0_cp,N(2),2); call add(m,g2) ! left turn
         call con_app_Roberts_B (g3,g2,1.0_cp,N(2),2); call add(m,g3) ! left entrance
         call con_app_Roberts_L (g2,g3,5.0_cp,N(1),1); call add(m,g2) ! left channel

         call con_prep_Roberts_B(g2,g1,1.0_cp,N(2),2); call add(m,g2) ! right turn
         call con_prep_Roberts_B(g3,g2,1.0_cp,N(2),2); call add(m,g3) ! right entrance
         call con_app_Roberts_L (g2,g3,5.0_cp,N(1),1); call add(m,g2) ! right channel

         call initProps(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
         call delete(g3)
       end subroutine

       subroutine flow_past_square(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2,g3
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         N = 30
         hmin = -0.5_cp; hmax = 0.5_cp; beta = 1.1_cp
         hmin(1) = 0.0_cp; hmax(1) = 2.0_cp
         call grid_Roberts_R(g2,hmin(1),hmax(1),N(1),beta(1),1) ! Central entrance
         call grid_Roberts_B(g2,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_Roberts_B(g2,hmin(3),hmax(3),1,beta(3),3)
         call add(m,g2)

         call con_app_Roberts_B (g1,g2,1.0_cp,N(2),2); call add(m,g1) ! left entrance
         call con_prep_Roberts_B(g3,g2,1.0_cp,N(2),2); call add(m,g3) ! right entrance

         call con_app_Roberts_B(g2,g1,1.0_cp,N(1),1); call add(m,g2) ! left side
         call con_app_Roberts_L(g1,g2,10.0_cp,60,1); call add(m,g1) ! left side exit

         call con_app_Roberts_B(g2,g3,1.0_cp,N(1),1); call add(m,g2) ! right side
         call con_app_Roberts_L(g3,g2,10.0_cp,60,1); call add(m,g3) ! right side exit

         call con_app_Roberts_B(g2,g3,1.0_cp,N(2),2); call add(m,g2) ! trailing cube

         call initProps(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
         call delete(g3)
       end subroutine

       end module