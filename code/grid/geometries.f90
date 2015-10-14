       module geometries_mod
       use generateGrid_mod
       use box3D_mod
       use extendGrid_mod
       use connectGrid_mod
       use gridGenTools_mod
       use grid_mod
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

       public :: cube
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

       subroutine cube(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         N = 52; hmin = 0.0_cp; hmax = 1.0_cp
         beta = reynoldsBL(1000.0_cp,hmin,hmax)
         ! beta = 10000.0_cp
         call box3D_Roberts_B(g,hmin(1),hmax(1),N(1),beta(1),1)
         call box3D_Roberts_B(g,hmin(2),hmax(2),N(2),beta(2),2)
         call box3D_Roberts_B(g,hmin(3),hmax(3),N(3),beta(3),3)
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
         N = 40; beta = 1.1_cp
         hmin = 0.0_cp; hmax = 1.0_cp
         call box3D_Roberts_R(g1,hmin(1),hmax(1),N(1),beta(1),1)
         call box3D_Roberts_B(g1,hmin(2),hmax(2),N(2),beta(2),2)
         call box3D_Roberts_B(g1,hmin(3),hmax(3),N(3),beta(3),3)
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
         N = 40
         hmin = 0.0_cp; hmax = 1.0_cp; beta = 1.1_cp
         call box3D_Roberts_B(g1,hmin(1),hmax(1),N(1),beta(1),1)
         call box3D_Roberts_B(g1,hmin(2),hmax(2),N(2),beta(2),2)
         call box3D_Roberts_B(g1,hmin(3),hmax(3),N(3),beta(3),3)
         call add(m,g1)

         call con_app_Roberts_B (g2,g1,1.0_cp,N(1),1); call add(m,g2)
         call con_app_Roberts_B (g1,g2,2.0_cp,N(2),2); call add(m,g1)
         call con_app_Roberts_B (g2,g1,1.0_cp,N(2),2); call add(m,g2)
         call con_prep_Roberts_B(g1,g2,1.0_cp,N(1),1); call add(m,g1)

         call initProps(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
       end subroutine

       subroutine ins_sudden_Expansion(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         N = 40
         hmin = -0.5_cp; hmax = 0.5_cp; beta = 1.1_cp
         hmin(1) = 0.0_cp; hmax(1) = 1.0_cp
         call box3D_Roberts_R(g2,hmin(1),hmax(1),N(1),beta(1),1)
         call box3D_Roberts_B(g2,hmin(2),hmax(2),N(2),beta(2),2)
         call box3D_Roberts_B(g2,hmin(3),hmax(3),N(3),beta(3),3)
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
         N = 40
         hmin = -0.5_cp; hmax = 0.5_cp; beta = 1.1_cp
         hmin(1) = 0.0_cp; hmax(1) = 1.0_cp
         call box3D_Roberts_R(g2,hmin(1),hmax(1),N(1),beta(1),1)
         call box3D_Roberts_B(g2,hmin(2),hmax(2),N(2),beta(2),2)
         call box3D_Roberts_B(g2,hmin(3),hmax(3),N(3),beta(3),3)
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
         N = 30
         hmin = -0.5_cp; hmax = 0.5_cp; beta = 1.1_cp
         hmin(1) = 0.0_cp; hmax(1) = 3.0_cp
         call box3D_Roberts_R(g2,hmin(1),hmax(1),N(1),beta(1),1)
         call box3D_Roberts_B(g2,hmin(2),hmax(2),N(2),beta(2),2)
         call box3D_Roberts_B(g2,hmin(3),hmax(3),1,beta(3),3)
         call add(m,g2)

         call con_app_Roberts_B (g1,g2,1.0_cp,N(2),2); call add(m,g1) ! left entrance
         call con_prep_Roberts_B(g3,g2,1.0_cp,N(2),2); call add(m,g3) ! right entrance

         call con_app_Roberts_B(g2,g1,1.0_cp,N(1),1); call add(m,g2) ! left side
         call con_app_Roberts_L(g1,g2,5.0_cp,N(1),1); call add(m,g1) ! left side exit

         call con_app_Roberts_B(g2,g3,1.0_cp,N(1),1); call add(m,g2) ! right side
         call con_app_Roberts_L(g3,g2,5.0_cp,N(1),1); call add(m,g3) ! right side exit

         call con_app_Roberts_B(g2,g3,1.0_cp,N(2),2); call add(m,g2) ! trailing cube

         call initProps(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
         call delete(g3)
       end subroutine

       end module