       module cube_general_mod
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
       public :: cube
       public :: square
       public :: cube_general

       contains

       subroutine cube(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer,dimension(3) :: N
         real(cp),dimension(3) :: hmin,hmax,beta
         logical,dimension(3) :: uniform
         real(cp) :: Re,Ha
         N = 45
         uniform = .false.
         hmin = -1.0_cp
         hmax =  1.0_cp
         Ha = 10.0_cp
         Re = 100.0_cp
         beta = Re_Ha_BL(Re,Ha,hmin,hmax) ! based on both Re and Ha

         call cube_general(m,hmin,hmax,N,beta,uniform)
       end subroutine

       subroutine square(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer,dimension(3) :: N
         real(cp),dimension(3) :: hmin,hmax,beta
         logical,dimension(3) :: uniform
         real(cp) :: Re,Ha
         N = 45
         uniform = .false.
         hmin = -1.0_cp
         hmax =  1.0_cp
         Ha = 1.0_cp
         Re = 1.0_cp
         beta = Re_Ha_BL(Re,Ha,hmin,hmax) ! based on both Re and Ha

         N(3) = 1
         call cube_general(m,hmin,hmax,N,beta,uniform)
       end subroutine

       subroutine cube_general(m,hmin,hmax,N,beta,uniform)
         implicit none
         type(mesh),intent(inout) :: m
         integer,dimension(3),intent(in) :: N
         real(cp),dimension(3),intent(in) :: hmin,hmax,beta
         logical,dimension(3),intent(in) :: uniform
         logical,dimension(3) :: temp
         type(grid) :: g
         integer :: i
         call delete(m)
         do i=1,3
           temp(i) = uniform(i).or.(N(i).eq.1)
         enddo
         if (temp(1)) then; i = 1; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         else;              i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         endif
         if (temp(2)) then; i = 2; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         else;              i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         endif
         if (temp(3)) then; i = 3; call grid_uniform(  g,hmin(i),hmax(i),N(i),i)
         else;              i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i)
         endif
         call add(m,g)
         call initProps(m)
         call patch(m)
         call delete(g)
       end subroutine

       end module