       module mesh_complex_geometries_mod
       use current_precision_mod
       use grid_init_mod
       use grid_continue_mod
       use grid_connect_mod
       use coordinate_stretch_parameters_mod
       use grid_mod
       use mesh_domain_mod
       use mesh_extend_mod
       implicit none

       private
       public :: LDC_2D_2domains_horizontal_x
       public :: LDC_2D_2domains_vertical_z
       public :: LDC_2D_2domains_vertical_x
       public :: LDC_2D_2domains_horizontal_z
       public :: LDC_2D_4domains
       public :: LDC_2D_9domains,LDC_2D_9domains_uniform
       public :: duct_2D_2domains,duct_2D_1domain
       public :: ins_elbow
       public :: ins_u_bend
       public :: ins_sudden_Expansion
       public :: ins_sep_channel
       public :: flow_past_square
       public :: ins_sep_channel_Tyler

       contains

       subroutine LDC_2D_2domains_vertical_z(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         real(cp) :: Ha,Re
         Ha = 10.0_cp; Re = 1000.0_cp
         call delete(m)
         N = (/15,30,1/)
         hmin = -0.5_cp; hmax = 0.5_cp
         hmin(1) = -0.5_cp; hmax(1) = 0.0_cp
         beta = ReynoldsBL(Re,hmin,hmax); beta = HartmannBL(Ha,hmin,hmax)

         call grid_uniform(g1,hmin(1),hmax(1),N(1),1)
         call grid_uniform(g1,hmin(2),hmax(2),N(2),2)
         call grid_uniform(g1,hmin(3),hmax(3),N(3),3)
         call add(m,g1)

         call con_app_uniform(g2,g1,N(1),1); call add(m,g2)
         call init_props(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
       end subroutine

       subroutine LDC_2D_2domains_horizontal_z(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         real(cp) :: Ha,Re
         Ha = 10.0_cp; Re = 1000.0_cp
         call delete(m)
         N = (/30,15,1/)
         hmin = -0.5_cp; hmax = 0.5_cp
         hmin(2) = -0.5_cp; hmax(2) = 0.0_cp
         beta = ReynoldsBL(Re,hmin,hmax); beta = HartmannBL(Ha,hmin,hmax)

         call grid_uniform(g1,hmin(1),hmax(1),N(1),1)
         call grid_uniform(g1,hmin(2),hmax(2),N(2),2)
         call grid_uniform(g1,hmin(3),hmax(3),N(3),3)
         call add(m,g1)

         call con_app_uniform(g2,g1,N(2),2); call add(m,g2)
         call init_props(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
       end subroutine

       subroutine LDC_2D_2domains_horizontal_x(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         real(cp) :: Ha,Re
         Ha = 10.0_cp; Re = 1000.0_cp
         call delete(m)
         N = (/1,50,25/)
         hmin = -0.5_cp
         hmax = 0.5_cp
         hmin(3) = -0.5_cp
         hmax(3) = 0.0_cp
         beta = ReynoldsBL(Re,hmin,hmax)
         beta = HartmannBL(Ha,hmin,hmax)

         call grid_uniform(g1,hmin(1),hmax(1),N(1),1)
         call grid_Roberts_B(g1,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_Roberts_L(g1,hmin(3),hmax(3),N(3),beta(3),3)
         call add(m,g1)

         call con_app_Roberts_R(g2,g1,0.5_cp,N(3),3)
         call add(m,g2)
         call init_props(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
       end subroutine

       subroutine LDC_2D_2domains_vertical_x(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         real(cp) :: Ha,Re
         Ha = 10.0_cp; Re = 1000.0_cp
         call delete(m)
         N = (/1,25,50/)
         hmin = -0.5_cp; hmax = 0.5_cp
         hmin(2) = -0.5_cp; hmax(2) = 0.0_cp
         beta = ReynoldsBL(Re,hmin,hmax); beta = HartmannBL(Ha,hmin,hmax)

         call grid_uniform(g1,hmin(1),hmax(1),N(1),1)
         call grid_Roberts_L(g1,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_Roberts_B(g1,hmin(3),hmax(3),N(3),beta(3),3)
         call add(m,g1)

         call con_app_Roberts_R(g2,g1,0.5_cp,N(2),2)
         call add(m,g2)
         call init_props(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
       end subroutine

       subroutine LDC_2D_4domains(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         real(cp) :: Ha,Re
         Ha = 10.0_cp; Re = 1000.0_cp
         call delete(m)
         N = (/25,25,1/)
         hmin = -0.5_cp
         hmax = 0.0_cp
         hmin(3) = -0.5_cp
         hmax(3) = 0.5_cp
         beta = ReynoldsBL(Re,hmin,hmax)
         beta = HartmannBL(Ha,hmin,hmax)

         call grid_Roberts_L(g1,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_L(g1,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_uniform(g1,hmin(3),hmax(3),N(3),3)
         call add(m,g1)

         call con_app_Roberts_R(g2,g1,0.5_cp,N(1),1); call add(m,g2)
         call con_app_Roberts_R(g2,g1,0.5_cp,N(2),2); call add(m,g2)
         call con_app_Roberts_R(g1,g2,0.5_cp,N(2),1); call add(m,g1)

         call init_props(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
       end subroutine

       subroutine LDC_2D_9domains(m)
         ! Setup:
         !         -------------
         !         | 5 | 8 | 9 |
         !         -------------
         !         | 4 | 6 | 7 |
         !         -------------
         !         | 1 | 2 | 3 |
         !         -------------
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2,g3
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         real(cp) :: Ha,Re
         Ha = 10.0_cp; Re = 1000.0_cp
         call delete(m)
         N = (/20,20,1/)
         hmin = -0.5_cp
         hmax = 0.0_cp
         hmin(3) = -0.5_cp
         hmax(3) = 0.5_cp
         beta = ReynoldsBL(Re,hmin,hmax)
         beta = HartmannBL(Ha,hmin,hmax)

         call grid_Roberts_L(g1,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_L(g1,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_uniform(g1,hmin(3),hmax(3),N(3),3)
         call add(m,g1) ! 1

         call con_app_Roberts_B(g2,g1,0.5_cp,N(1),1); call add(m,g2) ! 2
         call con_app_Roberts_R(g3,g2,0.5_cp,N(1),1); call add(m,g3) ! 3
         call con_app_Roberts_B(g2,g1,0.5_cp,N(2),2); call add(m,g2) ! 4
         call con_app_Roberts_R(g3,g2,0.5_cp,N(2),2); call add(m,g3) ! 5

         call con_app_Roberts_B(g1,g2,0.5_cp,N(1),1); call add(m,g1) ! 6
         call con_app_Roberts_R(g3,g1,0.5_cp,N(1),1); call add(m,g3) ! 7
         call con_app_Roberts_R(g3,g1,0.5_cp,N(2),2); call add(m,g3) ! 8
         call con_app_Roberts_R(g2,g3,0.5_cp,N(1),1); call add(m,g2) ! 9

         call init_props(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
         call delete(g3)
       end subroutine

       subroutine LDC_2D_9domains_uniform(m)
         ! Setup:
         !         -------------
         !         | 5 | 8 | 9 |
         !         -------------
         !         | 4 | 6 | 7 |
         !         -------------
         !         | 1 | 2 | 3 |
         !         -------------
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2,g3
         real(cp),dimension(3) :: hmin,hmax
         integer,dimension(3) :: N
         call delete(m)
         N = (/20,20,1/)
         hmin = -0.5_cp
         hmax = -0.5_cp + 1.0_cp/3.0_cp
         hmin(3) = -0.5_cp
         hmax(3) = 0.5_cp

         call grid_uniform(g1,hmin(1),hmax(1),N(1),1)
         call grid_uniform(g1,hmin(2),hmax(2),N(2),2)
         call grid_uniform(g1,hmin(3),hmax(3),N(3),3)
         call add(m,g1) ! 1

         call con_app_uniform(g2,g1,N(1),1); call add(m,g2) ! 2
         call con_app_uniform(g3,g2,N(1),1); call add(m,g3) ! 3
         call con_app_uniform(g2,g1,N(2),2); call add(m,g2) ! 4
         call con_app_uniform(g3,g2,N(2),2); call add(m,g3) ! 5

         call con_app_uniform(g1,g2,N(1),1); call add(m,g1) ! 6
         call con_app_uniform(g3,g1,N(1),1); call add(m,g3) ! 7
         call con_app_uniform(g3,g1,N(2),2); call add(m,g3) ! 8
         call con_app_uniform(g2,g3,N(1),1); call add(m,g2) ! 9

         call init_props(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
         call delete(g3)
       end subroutine

       subroutine duct_2D_2domains(m)
         ! Setup:
         !         -----------------------
         !         |         2           |
         !         -----------------------
         !         |         1           |
         !         -----------------------
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         N = (/50,30,1/)
         hmin = (/0.0_cp,-0.5_cp,-0.5_cp/)
         hmax = (/30.0_cp,0.0_cp,0.5_cp/)
         beta = ReynoldsBL(100.0_cp,hmin,hmax)

         call grid_Roberts_L(g1,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_L(g1,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_uniform(g1,hmin(3),hmax(3),N(3),3)
         call add(m,g1) ! 1

         call con_app_Roberts_R(g2,g1,0.5_cp,N(2),2); call add(m,g2) ! 2

         call init_props(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
       end subroutine

       subroutine duct_2D_1domain(m)
         ! Setup:
         !         -----------------------
         !         |         1           |
         !         -----------------------
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         N = (/50,30,1/)
         hmin = (/0.0_cp,-0.5_cp,-0.5_cp/)
         hmax = (/30.0_cp,0.5_cp,0.5_cp/)
         beta = ReynoldsBL(100.0_cp,hmin,hmax)
         call grid_Roberts_L(g1,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_B(g1,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_uniform(g1,hmin(3),hmax(3),N(3),3)
         call add(m,g1) ! 1
         call init_props(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
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

         call init_props(m)
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

         call init_props(m)
         call patch(m)
         call print(m)
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

         call init_props(m)
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

         call init_props(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
         call delete(g3)
       end subroutine

       subroutine ins_sep_channel_Tyler(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2,g3
         real(cp),dimension(3) :: beta
         integer,dimension(3) :: N
         integer,dimension(3) :: Nw
         real(cp) :: w_chan
         real(cp) :: L_ent,L_exp,L_chan,L_exit
         real(cp) :: height,t_w
         call delete(m)
         N = 25
         Nw = 5
         L_ent = 1.0_cp
         L_exp = 1.0_cp
         L_chan = 2.0_cp
         L_exit = 2.0_cp

         w_chan = 0.6_cp
         t_w = 0.1_cp
         height = 2.0_cp/3.0_cp

         beta = 1.1_cp
         call grid_Roberts_R(g2,0.0_cp,L_ent ,N(1),beta(1),1) ! entrance
         call grid_Roberts_B(g2,0.0_cp,w_chan,N(2),beta(2),2) ! entrance
         call grid_Roberts_B(g2,0.0_cp,height,N(3),beta(3),3) ! entrance
         call add(m,g2)

         call con_app_Roberts_B (g1,g2,L_exp ,N(1),1); call add(m,g1) ! center expansion
         call con_app_Roberts_B (g2,g1,L_chan,N(1),1); call add(m,g2) ! center channel
         call con_app_Roberts_L (g3,g2,L_exit,N(1),1); call add(m,g3) ! center exit

         call con_app_Roberts_B (g2,g3,t_w,Nw(2),2); call add(m,g2) ! exit post left wall
         call con_prep_Roberts_B(g2,g3,t_w,Nw(2),2); call add(m,g2) ! exit post right wall

         call con_app_Roberts_B (g2,g1,t_w   ,Nw(2),2); call add(m,g2) ! left turn
         call con_app_Roberts_B (g3,g2,w_chan,N(2) ,2); call add(m,g3) ! left expansion
         call con_app_Roberts_B (g2,g3,L_chan,N(1) ,1); call add(m,g2) ! left channel
         call con_app_Roberts_L (g3,g2,L_exit,N(1) ,1); call add(m,g3) ! left exit

         call con_prep_Roberts_B(g2,g1,t_w   ,Nw(2),2); call add(m,g2) ! right turn
         call con_prep_Roberts_B(g3,g2,w_chan,N(2) ,2); call add(m,g3) ! right expansion
         call con_app_Roberts_B (g2,g3,L_chan,N(1) ,1); call add(m,g2) ! right channel
         call con_app_Roberts_L (g3,g2,L_exit,N(1) ,1); call add(m,g3) ! right exit

         call init_props(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
         call delete(g3)
       end subroutine

       subroutine flow_past_square(m)
         ! Setup:
         !         ----------------------------------------
         !         | 2 | 4 |               5              |
         !  inlet  ---------------------------------------- outlet
         !   -->   | 1 |   |               8              | -->
         !         ----------------------------------------
         !         | 3 | 6 |               7              |
         !         ----------------------------------------
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2,g3
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         N = (/30,30,1/)
         hmin = -0.5_cp; hmax = 0.5_cp; beta = 1.1_cp
         hmin(1) = 0.0_cp; hmax(1) = 2.0_cp
         beta = ReynoldsBL(100.0_cp,(/-0.5_cp,-0.5_cp,-0.5_cp/),(/0.5_cp,0.5_cp,0.5_cp/))
         call grid_Roberts_R(g2,hmin(1),hmax(1),N(1),beta(1),1) ! Central entrance
         call grid_Roberts_B(g2,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_Roberts_B(g2,hmin(3),hmax(3),1,beta(3),3)
         call add(m,g2)

         call con_app_Roberts_L (g1,g2,24.5_cp,45,2); call add(m,g1) ! left entrance
         call con_prep_Roberts_R(g3,g2,24.5_cp,45,2); call add(m,g3) ! right entrance

         call con_app_Roberts_B(g2,g1,1.0_cp,N(1),1); call add(m,g2) ! left side
         call con_app_Roberts_L(g1,g2,80.0_cp,180,1); call add(m,g1) ! left side exit

         call con_app_Roberts_B(g2,g3,1.0_cp,N(1),1); call add(m,g2) ! right side
         call con_app_Roberts_L(g3,g2,80.0_cp,180,1); call add(m,g3) ! right side exit

         call con_app_Roberts_B(g2,g3,1.0_cp,N(2),2); call add(m,g2) ! trailing cube

         call init_props(m)
         call patch(m)
         call delete(g1)
         call delete(g2)
         call delete(g3)
       end subroutine

       end module