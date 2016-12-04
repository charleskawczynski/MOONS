       module grid_connect_mod
       use current_precision_mod
       use grid_mod
       use grid_genHelper_mod
       use coordinate_distribution_funcs_mod
       use coordinate_stretch_param_match_mod
       implicit none

       private

       public :: con_prep_uniform      ! con_prep_uniform(g,g_in,N,dir)
       public :: con_prep_Roberts_L    ! con_prep_Roberts_L(g,g_in,L,N,dir)
       public :: con_prep_Roberts_R    ! con_prep_Roberts_R(g,g_in,L,N,dir)
       public :: con_prep_Roberts_B    ! con_prep_Roberts_B(g,g_in,L,N,dir)
       ! public :: con_prep_cluster      ! Not yet implemented

       public :: con_app_uniform       ! con_app_uniform(g,g_in,N,dir)
       public :: con_app_Roberts_L     ! con_app_Roberts_L(g,g_in,L,N,dir)
       public :: con_app_Roberts_R     ! con_app_Roberts_R(g,g_in,L,N,dir)
       public :: con_app_Roberts_B     ! con_app_Roberts_B(g,g_in,L,N,dir)
       ! public :: con_app_cluster       ! Not yet implemented

       contains

       subroutine process(g,gg,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         call applyGhost(gg,dir)
         call init(g,gg%g%c(dir)%hn%f,dir)
         call delete(gg)
       end subroutine

       ! ******************************************************************** Prepend

       subroutine con_prep_uniform(g,g_in,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: N,dir
         type(gridGenerator) :: gg
         real(cp) :: dh
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in con_prep_uniform in extend_grid.f90'
         call init(g,g_in); call init(gg%g,g)
         dh = gg%g%c(dir)%dhn%f(1)
         call init(gg,(/uniformLeft(g_in%c(dir)%hmin,dh,N)/),dir)
         call process(g,gg,dir)
       end subroutine

       subroutine con_prep_Roberts_L(g,g_in,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         real(cp) :: beta,hmin
         type(gridGenerator) :: gg
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in con_prep_Roberts_L in extend_grid.f90'
         call init(g,g_in); call init(gg%g,g)
         hmin = g_in%c(dir)%hmin
         beta = beta_dh_small(hmin - L,hmin,N,g_in%c(dir)%dhn%f(1))
         call init(gg,(/robertsLeft(hmin-L,hmin,N,beta)/),dir)
         call process(g,gg,dir)
       end subroutine

       subroutine con_prep_Roberts_R(g,g_in,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         real(cp) :: beta,hmin
         type(gridGenerator) :: gg
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in con_prep_Roberts_R in extend_grid.f90'
         call init(g,g_in); call init(gg%g,g)
         hmin = g_in%c(dir)%hmin
         beta = beta_dh_small(hmin - L,hmin,N,g_in%c(dir)%dhn%f(1))
         call init(gg,(/robertsRight(hmin-L,hmin,N,beta)/),dir)
         call process(g,gg,dir)
       end subroutine

       subroutine con_prep_Roberts_B(g,g_in,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         real(cp) :: beta,hmin
         type(gridGenerator) :: gg
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in con_prep_Roberts_B in extend_grid.f90'
         call init(g,g_in); call init(gg%g,g)
         hmin = g_in%c(dir)%hmin
         beta = beta_dh_both(hmin - L,hmin,N,g_in%c(dir)%dhn%f(1))
         call init(gg,(/robertsBoth(hmin-L,hmin,N,beta)/),dir)
         call process(g,gg,dir)
       end subroutine

       ! ******************************************************************** Append

       subroutine con_app_uniform(g,g_in,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: N,dir
         type(gridGenerator) :: gg
         real(cp) :: dh
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in con_app_uniform in extend_grid.f90'
         call init(g,g_in); call init(gg%g,g)
         dh = gg%g%c(dir)%dhn%f(gg%g%c(dir)%sn-1)
         call init(gg,(/uniformRight(g_in%c(dir)%hmax,dh,N)/),dir)
         call print(gg%g)
         call process(g,gg,dir)
       end subroutine

       subroutine con_app_Roberts_L(g,g_in,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         real(cp) :: beta,hmax
         type(gridGenerator) :: gg
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in con_app_Roberts_L in extend_grid.f90'
         call init(g,g_in); call init(gg%g,g)
         hmax = g_in%c(dir)%hmax
         beta = beta_dh_small(hmax,hmax + L,N,g_in%c(dir)%dhn_e)
         call init(gg,(/robertsLeft(hmax,hmax + L,N,beta)/),dir)
         call process(g,gg,dir)
       end subroutine

       subroutine con_app_Roberts_R(g,g_in,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         real(cp) :: beta,hmax
         type(gridGenerator) :: gg
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in con_app_Roberts_R in extend_grid.f90'
         call init(g,g_in); call init(gg%g,g)
         hmax = g_in%c(dir)%hmax
         beta = beta_dh_small(hmax,hmax + L,N,g_in%c(dir)%dhn%f(1))
         call init(gg,(/robertsRight(hmax,hmax + L,N,beta)/),dir)
         call process(g,gg,dir)
       end subroutine

       subroutine con_app_Roberts_B(g,g_in,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         real(cp) :: beta,hmax
         type(gridGenerator) :: gg
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in con_app_Roberts_B in extend_grid.f90'
         call init(g,g_in); call init(gg%g,g)
         hmax = g_in%c(dir)%hmax
         beta = beta_dh_both(hmax,hmax + L,N,g_in%c(dir)%dhn_e)
         call init(gg,(/robertsBoth(hmax,hmax + L,N,beta)/),dir)
         call process(g,gg,dir)
       end subroutine

       end module