       module grid_continue_mod
       use current_precision_mod
       use array_mod
       use array_extend_mod
       use coordinates_mod
       use coordinates_extend_mod
       use grid_mod
       use grid_extend_mod
       use mesh_quality_params_mod
       use coordinate_distribution_funcs_mod
       use coordinate_distribution_funcs_iterate_mod
       use coordinate_stretch_param_match_mod
       implicit none

       private

       public :: ext_uniform_IO
       public :: ext_Roberts_near_IO
       public :: ext_Roberts_far_IO
       public :: ext_Roberts_B_IO

       public :: ext_prep_uniform_IO      ! ext_prep_uniform(g,N,dir)
       public :: ext_prep_Roberts_L_IO    ! ext_prep_Roberts_L(g,L,N,dir)
       public :: ext_prep_Roberts_R_IO    ! ext_prep_Roberts_R(g,L,N,dir)
       public :: ext_prep_Roberts_B_IO    ! ext_prep_Roberts_B(g,L,N,dir)
       ! public :: ext_prep_cluster      ! Not yet implemented

       public :: ext_app_uniform_IO       ! ext_app_uniform(g,N,dir)
       public :: ext_app_Roberts_L_IO     ! ext_app_Roberts_L(g,L,N,dir)
       public :: ext_app_Roberts_R_IO     ! ext_app_Roberts_R(g,L,N,dir)
       public :: ext_app_Roberts_B_IO     ! ext_app_Roberts_B(g,L,N,dir)
       ! public :: ext_app_cluster       ! Not yet implemented

       public :: ext_app_Roberts_C2F_IO     ! ext_app_Roberts_B(g,L,N,dir)
       public :: ext_prep_Roberts_C2F_IO

       contains

       ! ******************************************************************** Prepend + Append

       subroutine ext_uniform_IO(g,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         call ext_prep_uniform_IO(g,N,dir)
         call ext_app_uniform_IO (g,N,dir)
       end subroutine

       subroutine ext_Roberts_near_IO(g,L,N,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(mesh_quality_params),intent(in) :: MQP
         call ext_prep_Roberts_R_IO(g,L,N,dir,MQP)
         call ext_app_Roberts_L_IO (g,L,N,dir,MQP)
       end subroutine

       subroutine ext_Roberts_far_IO(g,L,N,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(mesh_quality_params),intent(in) :: MQP
         call ext_prep_Roberts_L_IO(g,L,N,dir,MQP)
         call ext_app_Roberts_R_IO (g,L,N,dir,MQP)
       end subroutine

       subroutine ext_Roberts_B_IO(g,L,N,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(mesh_quality_params),intent(in) :: MQP
         call ext_prep_Roberts_B_IO(g,L,N,dir,MQP)
         call ext_app_Roberts_B_IO (g,L,N,dir,MQP)
       end subroutine

       ! ******************************************************************** Prepend

       subroutine process_prepend(g,a,b,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(array),intent(inout) :: a,b
         integer,intent(in) :: dir
         call snip(a); call snip(a)
         call prepend(a,b)
         call init(g%c(dir),a)
         call prepend_ghost(g%c(dir))
         call initProps(g)
         call delete(a); call delete(b)
       end subroutine

       subroutine ext_prep_uniform_IO(g,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         type(array) :: a,b
         real(cp) :: hmin,dh
         call check_N(N,'ext_prep_uniform_IO')
         call init(a,g%c(dir)%hn)
         hmin = g%c(dir)%hmin; dh = g%c(dir)%dhn%f(1)
         call init(b,uniformLeft(hmin,dh,N))
         call process_prepend(g,a,b,dir)
       end subroutine

       subroutine ext_prep_Roberts_L_IO(g,L,N,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(mesh_quality_params),intent(in) :: MQP
         real(cp) :: beta,hmin,dh
         type(array) :: a,b
         integer :: i,N_final
         call check_N(N,'ext_prep_Roberts_L_IO')
         call init(a,g%c(dir)%hn)
         hmin = g%c(dir)%hmin; dh = g%c(dir)%dhn%f(1)
         N_final = N
         do i=1,MQP%N_iter
           beta = beta_dh_small(hmin - L,hmin,N_final,dh)
           call init(b,robertsLeft(hmin-L,hmin,N_final,beta))
           N_final = b%N
           if (needs_more_points(b,MQP)) then; N_final=N_final+1; else; exit; endif
         enddo
         call process_prepend(g,a,b,dir)
       end subroutine

       subroutine ext_prep_Roberts_R_IO(g,L,N,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(mesh_quality_params),intent(in) :: MQP
         real(cp) :: beta,hmin,dh
         type(array) :: a,b
         integer :: i,N_final
         call check_N(N,'ext_prep_Roberts_R_IO')
         call init(a,g%c(dir)%hn)
         hmin = g%c(dir)%hmin; dh = g%c(dir)%dhn%f(1)
         N_final = N
         do i=1,MQP%N_iter
           beta = beta_dh_small(hmin - L,hmin,N_final,dh)
           call init(b,robertsRight(hmin-L,hmin,N_final,beta))
           N_final = b%N
           if (needs_more_points(b,MQP)) then; N_final=N_final+1; else; exit; endif
         enddo
         call process_prepend(g,a,b,dir)
       end subroutine

       subroutine ext_prep_Roberts_B_IO(g,L,N,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(mesh_quality_params),intent(in) :: MQP
         real(cp) :: beta,hmin,dh
         type(array) :: a,b
         integer :: i,N_final
         call check_N(N,'ext_prep_Roberts_B_IO')
         call init(a,g%c(dir)%hn)
         hmin = g%c(dir)%hmin; dh = g%c(dir)%dhn%f(1)
         N_final = N
         do i=1,MQP%N_iter
           beta = beta_dh_both(hmin - L,hmin,N_final,dh)
           call init(b,robertsBoth(hmin-L,hmin,N_final,beta))
           N_final = b%N
           if (needs_more_points(b,MQP)) then; N_final=N_final+1; else; exit; endif
         enddo
         call process_prepend(g,a,b,dir)
       end subroutine

       subroutine ext_prep_Roberts_C2F_IO(g,L,N,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(mesh_quality_params),intent(in) :: MQP
         real(cp) :: beta,hmin,dh
         type(array) :: a,b
         integer :: i,N_final
         call check_N(N,'ext_prep_Roberts_C2F_IO')
         call init(a,g%c(dir)%hn)
         hmin = g%c(dir)%hmin; dh = g%c(dir)%dhn%f(1)
         N_final = N
         do i=1,MQP%N_iter
           beta = beta_dh_big(hmin - L,hmin,N_final,dh)
           call init(b,robertsLeft(hmin-L,hmin,N_final,beta))
           N_final = b%N
           if (needs_more_points(b,MQP)) then; N_final=N_final+1; else; exit; endif
         enddo
         call process_prepend(g,a,b,dir)
       end subroutine

       ! ******************************************************************** Append

       subroutine process_append(g,a,b,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(array),intent(inout) :: a,b
         integer,intent(in) :: dir
         call init(a,g%c(dir)%hn)
         call pop(a); call pop(a)
         call append(a,b)
         call init(g%c(dir),a)
         call append_ghost(g%c(dir))
         call initProps(g)
         call delete(a); call delete(b)
       end subroutine

       subroutine ext_app_uniform_IO(g,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         type(array) :: a,b
         real(cp) :: dh
         call check_N(N,'ext_app_uniform_IO')
         call init(a,g%c(dir)%hn)
         dh = g%c(dir)%dhn_e
         call init(b,uniformRight(g%c(dir)%hmax,dh,N))
         call process_append(g,a,b,dir)
       end subroutine

       subroutine ext_app_Roberts_L_IO(g,L,N,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(mesh_quality_params),intent(in) :: MQP
         real(cp) :: beta,hmax,dh
         type(array) :: a,b
         integer :: i,N_final
         call check_N(N,'ext_app_Roberts_L_IO')
         hmax = g%c(dir)%hmax; dh = g%c(dir)%dhn_e
         N_final = N
         do i=1,MQP%N_iter
           beta = beta_dh_small(hmax,hmax + L,N_final,dh)
           call init(b,robertsLeft(hmax,hmax + L,N_final,beta))
           N_final = b%N
           if (needs_more_points(b,MQP)) then; N_final=N_final+1; else; exit; endif
         enddo
         call process_append(g,a,b,dir)
       end subroutine

       subroutine ext_app_Roberts_R_IO(g,L,N,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(mesh_quality_params),intent(in) :: MQP
         real(cp) :: beta,hmax,dh
         type(array) :: a,b
         integer :: i,N_final
         call check_N(N,'ext_app_Roberts_R_IO')
         hmax = g%c(dir)%hmax; dh = g%c(dir)%dhn_e
         N_final = N
         do i=1,MQP%N_iter
           beta = beta_dh_small(hmax,hmax + L,N_final,dh)
           call init(b,robertsRight(hmax,hmax + L,N_final,beta))
           N_final = b%N
           if (needs_more_points(b,MQP)) then; N_final=N_final+1; else; exit; endif
         enddo
         call process_append(g,a,b,dir)
       end subroutine

       subroutine ext_app_Roberts_B_IO(g,L,N,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(mesh_quality_params),intent(in) :: MQP
         real(cp) :: beta,hmax,dh
         type(array) :: a,b
         integer :: i,N_final
         call check_N(N,'ext_app_Roberts_B_IO')
         hmax = g%c(dir)%hmax; dh = g%c(dir)%dhn_e
         N_final = N
         do i=1,MQP%N_iter
           beta = beta_dh_both(hmax,hmax + L,N_final,dh)
           call init(b,robertsBoth(hmax,hmax + L,N_final,beta))
           N_final = b%N
           if (needs_more_points(b,MQP)) then; N_final=N_final+1; else; exit; endif
         enddo
         call process_append(g,a,b,dir)
       end subroutine

       subroutine ext_app_Roberts_C2F_IO(g,L,N,dir,MQP)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),intent(in) :: L
         integer,intent(in) :: N,dir
         type(mesh_quality_params),intent(in) :: MQP
         real(cp) :: beta,hmax,dh
         type(array) :: a,b
         integer :: i,N_final
         call check_N(N,'ext_app_Roberts_C2F_IO')
         hmax = g%c(dir)%hmax; dh = g%c(dir)%dhn_e
         N_final = N
         do i=1,MQP%N_iter
           beta = beta_dh_big(hmax,hmax + L,N_final,dh)
           call init(b,robertsRight(hmax,hmax + L,N_final,beta))
           N_final = b%N
           if (needs_more_points(b,MQP)) then; N_final=N_final+1; else; exit; endif
         enddo
         call process_append(g,a,b,dir)
       end subroutine

       subroutine check_N(N,caller)
         implicit none
         character(len=*),intent(in) :: caller
         integer,intent(in) :: N
         if (.not.(N.gt.0)) then
           write(*,*) 'Error: N is not > 0 in ',caller,' in extend_grid.f90'
           stop 'Done'
         endif
       end subroutine

       end module