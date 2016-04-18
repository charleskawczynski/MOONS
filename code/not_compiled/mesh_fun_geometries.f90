       module mesh_fun_geometries_mod
       use current_precision_mod
       use grid_init_mod
       use grid_extend_mod
       use grid_connect_mod
       use grid_distribution_funcs_mod
       use grid_mod
       use grid_genHelper_mod
       use domain_mod
       use mesh_mod
       implicit none

       private
       public :: loop_2D
       public :: figure8_2D_cross_flow
       public :: figure8_3D

       contains

       subroutine loop_2D(m)
         ! 
         !      ---------- ---------- ---------- 
         !     |          |          |          |
         !     |    6     |    5     |    4     |
         !     |          |          |          |
         !      ---------- ---------- ---------- 
         !     |          |          |          |
         !     |    7     |  empty   |    3     |
         !     |          |          |          |
         !      ---------- ---------- ---------- 
         !     |          |          |          |
         !     |    8     |    1     |    2     |
         !     |          |          |          |
         !      ---------- ---------- ---------- 
         ! 
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         N = 30
         hmin = 0.0_cp; hmax = 1.0_cp; beta = 1.05_cp
         hmax(1) = 5.0_cp
         call grid_Roberts_B(g1,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_B(g1,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_uniform(g1,hmin(3),hmax(3),1,3)
         call add(m,g1)

         call con_app_Roberts_B (g2,g1,1.0_cp,N(1),1); call add(m,g2) ! first corner
         call con_app_Roberts_B (g1,g2,2.0_cp,N(2),2); call add(m,g1) ! Long 
         call con_app_Roberts_B (g2,g1,1.0_cp,N(2),2); call add(m,g2) ! second corner
         call con_prep_Roberts_R(g1,g2,5.0_cp, 40 ,1); call add(m,g1) ! exit

         call initProps(m)
         call patch(m)
         call print(m)
         call delete(g1)
         call delete(g2)
       end subroutine

       subroutine figure8_2D_cross_flow(m)
         ! 
         ! 
         !                            ---------- ---------- ---------- 
         !                           |          |          |          |
         !                           |    10    |    11    |    12    |
         !                           |          |   -->    |          |
         !                            ---------- ---------- ---------- 
         !                           |          |          |          |
         !                           |    9  ^  |  empty   |    13  | |
         !                           |       |  |          |        v |
         !      ---------- ---------- ---------- ---------- ---------- 
         !     |          |          |          |          |          |
         !     |    2     |    1     |    8     |    15    |    14    |
         !     |          |   <--    |          |    <--   |          |
         !      ---------- ---------- ---------- ---------- ---------- 
         !     |          |          |          |
         !     |    3   | |  empty   |    7  ^  |
         !     |        v |          |       |  |
         !      ---------- ---------- ---------- 
         !     |          |          |          |
         !     |    4     |    5     |    6     |
         !     |          |    -->   |          |
         !      ---------- ---------- ---------- 
         ! 
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         N = 30
         hmin = 0.0_cp; hmax = 1.0_cp; beta = 1.05_cp
         hmax(1) = 5.0_cp
         call grid_Roberts_B(g1,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_B(g1,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_uniform(g1,hmin(3),hmax(3),1,3)
         call add(m,g1)

         call con_app_Roberts_B (g2,g1,1.0_cp,N(1),1); call add(m,g2) ! first corner
         call con_app_Roberts_B (g1,g2,2.0_cp,N(2),2); call add(m,g1) ! Long 
         call con_app_Roberts_B (g2,g1,1.0_cp,N(2),2); call add(m,g2) ! second corner
         call con_prep_Roberts_R(g1,g2,5.0_cp, 40 ,1); call add(m,g1) ! exit

         call initProps(m)
         call patch(m)
         call print(m)
         call delete(g1)
         call delete(g2)
       end subroutine

       subroutine figure8_3D(m)
         ! () = level 1
         ! <> = level 2 (small)
         ! [] = level 3
         ! 
         !                            ---------- ---------- ---------- 
         !                           |          |          |   [15]   |
         !                           |   [13]   |   [14]   |   <16>   |
         !                           |          |   -->    |   (17)   |
         !                            ---------- ---------- ---------- 
         !                           |          |          |          |
         !                           |   [12] ^ |  empty   |   (18) | |
         !                           |        | |          |        v |
         !      ---------- ---------- ---------- ---------- ---------- 
         !     |          |          |          |          |          |
         !     |   (3)    |    (2)   |   (1)    |   (20)   |   (19)   |
         !     |          |   <--    |   [11]   |    <--   |          |
         !      ---------- ---------- ---------- ---------- ---------- 
         !     |          |          |          |
         !     |   (4)  | |  empty   |   [10] ^ |
         !     |        v |          |        | |
         !      ---------- ---------- ---------- 
         !     |   (5)    |          |          |
         !     |   <6>    |   [8]    |   [9]    |
         !     |   [7]    |    -->   |          |
         !      ---------- ---------- ---------- 
         ! 
         implicit none
         type(mesh),intent(inout) :: m
         type(grid) :: g1,g2
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         call delete(m)
         N = 30
         hmin = 0.0_cp; hmax = 1.0_cp; beta = 1.05_cp
         hmax(1) = 5.0_cp
         call grid_Roberts_B(g1,hmin(1),hmax(1),N(1),beta(1),1)
         call grid_Roberts_B(g1,hmin(2),hmax(2),N(2),beta(2),2)
         call grid_uniform(g1,hmin(3),hmax(3),1,3)
         call add(m,g1)

         call con_app_Roberts_B (g2,g1,1.0_cp,N(1),1); call add(m,g2) ! first corner
         call con_app_Roberts_B (g1,g2,2.0_cp,N(2),2); call add(m,g1) ! Long 
         call con_app_Roberts_B (g2,g1,1.0_cp,N(2),2); call add(m,g2) ! second corner
         call con_prep_Roberts_R(g1,g2,5.0_cp, 40 ,1); call add(m,g1) ! exit

         call initProps(m)
         call patch(m)
         call print(m)
         call delete(g1)
         call delete(g2)
       end subroutine

       end module