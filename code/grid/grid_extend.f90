       module grid_extend_mod
       use grid_mod
       use grid_genHelper_mod
       use grid_distribution_funcs_mod
       use grid_stretchParamMatch_mod
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

       public :: ext_uniform_IO
       public :: ext_Roberts_near_IO
       public :: ext_Roberts_far_IO
       public :: ext_Roberts_B_IO

       public :: ext_prep_uniform,ext_prep_uniform_IO      ! ext_prep_uniform(g,g_in,N,dir)
       public :: ext_prep_Roberts_L,ext_prep_Roberts_L_IO    ! ext_prep_Roberts_L(g,g_in,L,N,dir)
       public :: ext_prep_Roberts_R,ext_prep_Roberts_R_IO    ! ext_prep_Roberts_R(g,g_in,L,N,dir)
       public :: ext_prep_Roberts_B,ext_prep_Roberts_B_IO    ! ext_prep_Roberts_B(g,g_in,L,N,dir)
       ! public :: ext_prep_cluster      ! Not yet implemented

       public :: ext_app_uniform,ext_app_uniform_IO       ! ext_app_uniform(g,g_in,N,dir)
       public :: ext_app_Roberts_L,ext_app_Roberts_L_IO     ! ext_app_Roberts_L(g,g_in,L,N,dir)
       public :: ext_app_Roberts_R,ext_app_Roberts_R_IO     ! ext_app_Roberts_R(g,g_in,L,N,dir)
       public :: ext_app_Roberts_B,ext_app_Roberts_B_IO     ! ext_app_Roberts_B(g,g_in,L,N,dir)
       ! public :: ext_app_cluster       ! Not yet implemented

       contains

       ! ******************************************************************** Prepend + Append

       subroutine ext_uniform_IO(g,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         call ext_prep_uniform_IO(g,N,dir)
         call ext_app_uniform_IO (g,N,dir)
       end subroutine

       subroutine ext_Roberts_near_IO(g,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         call ext_prep_Roberts_R_IO(g,L,N,dir)
         call ext_app_Roberts_L_IO (g,L,N,dir)
       end subroutine

       subroutine ext_Roberts_far_IO(g,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         call ext_prep_Roberts_L_IO(g,L,N,dir)
         call ext_app_Roberts_R_IO (g,L,N,dir)
       end subroutine

       subroutine ext_Roberts_B_IO(g,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         call ext_prep_Roberts_B_IO(g,L,N,dir)
         call ext_app_Roberts_B_IO (g,L,N,dir)
       end subroutine

       ! ******************************************************************** Prepend

       subroutine ext_prep_uniform_IO(g,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         type(grid) :: temp
         call init(temp,g); call ext_prep_uniform(g,temp,N,dir); call delete(temp)
       end subroutine

       subroutine ext_prep_uniform(g,g_in,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: N,dir
         type(gridGenerator) :: gg
         real(cp) :: dh
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in ext_prep_uniform in extend_grid.f90'
         call init(g,g_in); call init(gg%g%c(dir),g_in%c(dir))
         call snip(gg,dir) ! Remove ghost nodes
         dh = gg%g%c(dir)%dhn(1)
         call snip(gg,dir)
         call prep(gg,(/uniformLeft(g_in%c(dir)%hmin,dh,N)/),dir)
         call prepGhost(gg,dir) ! re-apply ghosts
         call init(g,gg%g%c(dir)%hn,dir)
         call initProps(g)
         call delete(gg)
       end subroutine

       subroutine ext_prep_Roberts_L_IO(g,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(grid) :: temp
         call init(temp,g); call ext_prep_Roberts_L(g,temp,L,N,dir); call delete(temp)
       end subroutine

       subroutine ext_prep_Roberts_L(g,g_in,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         real(cp) :: beta,hmin
         type(gridGenerator) :: gg
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in ext_prep_Roberts_L in extend_grid.f90'
         call init(g,g_in); call init(gg%g%c(dir),g_in%c(dir))
         hmin = g_in%c(dir)%hmin
         beta = betaRobertsLeft(hmin - L,hmin,N,g_in%c(dir)%dhn(1))
         call snip(gg,dir); call snip(gg,dir)
         call prep(gg,(/robertsLeft(hmin-L,hmin,N,beta)/),dir)
         call prepGhost(gg,dir)
         call init(g,gg%g%c(dir)%hn,dir)
         call initProps(g)
         call delete(gg)
       end subroutine

       subroutine ext_prep_Roberts_R_IO(g,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(grid) :: temp
         call init(temp,g); call ext_prep_Roberts_R(g,temp,L,N,dir); call delete(temp)
       end subroutine

       subroutine ext_prep_Roberts_R(g,g_in,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         real(cp) :: beta,hmin
         type(gridGenerator) :: gg
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in ext_prep_Roberts_R in extend_grid.f90'
         call init(g,g_in); call init(gg%g%c(dir),g_in%c(dir))
         hmin = g_in%c(dir)%hmin
         beta = betaRobertsRight(hmin - L,hmin,N,g_in%c(dir)%dhn(1))
         call snip(gg,dir); call snip(gg,dir)
         call prep(gg,(/robertsRight(hmin-L,hmin,N,beta)/),dir)
         call prepGhost(gg,dir)
         call init(g,gg%g%c(dir)%hn,dir)
         call initProps(g)
         call delete(gg)
       end subroutine

       subroutine ext_prep_Roberts_B_IO(g,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(grid) :: temp
         call init(temp,g); call ext_prep_Roberts_B(g,temp,L,N,dir); call delete(temp)
       end subroutine

       subroutine ext_prep_Roberts_B(g,g_in,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         real(cp) :: beta,hmin
         type(gridGenerator) :: gg
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in ext_prep_Roberts_B in extend_grid.f90'
         call init(g,g_in); call init(gg%g%c(dir),g_in%c(dir))
         hmin = g_in%c(dir)%hmin
         beta = betaRobertsBoth(hmin - L,hmin,N,g_in%c(dir)%dhn(1))
         call snip(gg,dir); call snip(gg,dir)
         call prep(gg,(/robertsBoth(hmin-L,hmin,N,beta)/),dir)
         call prepGhost(gg,dir)
         call init(g,gg%g%c(dir)%hn,dir)
         call initProps(g)
         call delete(gg)
       end subroutine

       ! ******************************************************************** Append

       subroutine ext_app_uniform_IO(g,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         type(grid) :: temp
         call init(temp,g); call ext_app_uniform(g,temp,N,dir); call delete(temp)
       end subroutine

       subroutine ext_app_uniform(g,g_in,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: N,dir
         type(gridGenerator) :: gg
         real(cp) :: dh
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in ext_app_uniform in extend_grid.f90'
         call init(g,g_in); call init(gg%g%c(dir),g_in%c(dir))
         call pop(gg,dir) ! Remove ghost nodes
         dh = gg%g%c(dir)%dhn(gg%g%c(dir)%sn-1)
         call pop(gg,dir)
         call app(gg,(/uniformRight(g_in%c(dir)%hmax,dh,N)/),dir)
         call appGhost(gg,dir) ! re-apply ghosts
         call init(g,gg%g%c(dir)%hn,dir)
         call initProps(g)
         call delete(gg)
       end subroutine

       subroutine ext_app_Roberts_L_IO(g,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(grid) :: temp
         call init(temp,g); call ext_app_Roberts_L(g,temp,L,N,dir); call delete(temp)
       end subroutine

       subroutine ext_app_Roberts_L(g,g_in,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         real(cp) :: beta,hmax
         type(gridGenerator) :: gg
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in ext_app_Roberts_L in extend_grid.f90'
         call init(g,g_in); call init(gg%g%c(dir),g_in%c(dir))
         hmax = g_in%c(dir)%hmax
         beta = betaRobertsLeft(hmax,hmax + L,N,g_in%c(dir)%dhn(g_in%c(dir)%sn-1))
         call pop(gg,dir); call pop(gg,dir)
         call app(gg,(/robertsLeft(hmax,hmax + L,N,beta)/),dir)
         call appGhost(gg,dir)
         call init(g,gg%g%c(dir)%hn,dir)
         call initProps(g)
         call delete(gg)
       end subroutine

       subroutine ext_app_Roberts_R_IO(g,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(grid) :: temp
         call init(temp,g); call ext_app_Roberts_R(g,temp,L,N,dir); call delete(temp)
       end subroutine

       subroutine ext_app_Roberts_R(g,g_in,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         real(cp) :: beta,hmax
         type(gridGenerator) :: gg
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in ext_app_Roberts_R in extend_grid.f90'
         call init(g,g_in); call init(gg%g%c(dir),g_in%c(dir))
         hmax = g_in%c(dir)%hmax
         beta = betaRobertsRight(hmax,hmax + L,N,g_in%c(dir)%dhn(g_in%c(dir)%sn-1))
         call pop(gg,dir); call pop(gg,dir)
         call app(gg,(/robertsRight(hmax,hmax + L,N,beta)/),dir)
         call appGhost(gg,dir)
         call init(g,gg%g%c(dir)%hn,dir)
         call initProps(g)
         call delete(gg)
       end subroutine

       subroutine ext_app_Roberts_B_IO(g,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(grid) :: temp
         call init(temp,g); call ext_app_Roberts_B(g,temp,L,N,dir); call delete(temp)
       end subroutine

       subroutine ext_app_Roberts_B(g,g_in,L,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         real(cp) :: beta,hmax
         type(gridGenerator) :: gg
         if (.not.(N.gt.0)) stop 'Error: N is not > 0 in ext_app_Roberts_B in extend_grid.f90'
         call init(g,g_in); call init(gg%g%c(dir),g_in%c(dir))
         hmax = g_in%c(dir)%hmax
         beta = betaRobertsBoth(hmax,hmax + L,N,g_in%c(dir)%dhn(g_in%c(dir)%sn-1))
         call pop(gg,dir); call pop(gg,dir)
         call app(gg,(/robertsBoth(hmax,hmax + L,N,beta)/),dir)
         call appGhost(gg,dir)
         call init(g,gg%g%c(dir)%hn,dir)
         call initProps(g)
         call delete(gg)
       end subroutine

       end module