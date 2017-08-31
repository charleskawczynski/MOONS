       module grid_init_mod
       use current_precision_mod
       use grid_mod
       use grid_extend_mod
       use array_mod
       use array_extend_mod
       use coordinates_mod
       use coordinates_extend_mod
       use mesh_quality_params_mod
       use coordinate_distribution_funcs_mod
       use coordinate_distribution_funcs_iterate_mod
       use coordinate_stretch_param_match_mod
       implicit none

       private

       public :: grid_uniform
       public :: grid_Roberts_L,grid_Roberts_R,grid_Roberts_B
       public :: grid_cluster

       contains

       subroutine process(g,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: dir
         call add_ghost_nodes(g%c(dir))
         call initProps(g)
       end subroutine

       subroutine grid_uniform(g,hmin,hmax,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         real(cp),intent(in) :: hmin,hmax
         call init(g%c(dir),uniform_iterate(hmin,hmax,N))
         call process(g,dir)
       end subroutine

       subroutine grid_Roberts_L(g,hmin,hmax,N,beta,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         real(cp),intent(in) :: hmin,hmax,beta
         type(mesh_quality_params),intent(in) :: MQP
         call init(g%c(dir),robertsLeft_iterate(hmin,hmax,N,beta,MQP))
         call process(g,dir)
       end subroutine

       subroutine grid_Roberts_R(g,hmin,hmax,N,beta,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         real(cp),intent(in) :: hmin,hmax,beta
         type(mesh_quality_params),intent(in) :: MQP
         call init(g%c(dir),robertsRight_iterate(hmin,hmax,N,beta,MQP))
         call process(g,dir)
       end subroutine

       subroutine grid_Roberts_B(g,hmin,hmax,N,beta,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         real(cp),intent(in) :: hmin,hmax,beta
         type(mesh_quality_params),intent(in) :: MQP
         call init(g%c(dir),robertsBoth_iterate(hmin,hmax,N,beta,MQP))
         call process(g,dir)
       end subroutine

       subroutine grid_cluster(g,hmin,hmax,N,yc,tau,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         real(cp),intent(in) :: hmin,hmax,yc,tau
         type(mesh_quality_params),intent(in) :: MQP
         call init(g%c(dir),cluster_iterate(hmin,hmax,N,yc,tau,MQP))
         call process(g,dir)
       end subroutine

       end module