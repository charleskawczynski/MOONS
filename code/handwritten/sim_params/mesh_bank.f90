     module mesh_bank_mod
     use current_precision_mod
     use constants_mod
     use string_mod
     use dir_tree_mod
     use path_extend_mod
     use mesh_params_extend_mod
     use segment_extend_mod
     use mesh_quality_params_extend_mod
     use sim_params_mod
     use sim_params_aux_mod
     implicit none

     private
     public :: define_mesh_SP_plasma_disruption
     public :: define_mesh_SP_plasma_disruption_plane
     public :: define_mesh_SP_plasma_disruption_plane_insulating
     public :: define_mesh_SP_plasma_disruption_line
     public :: define_mesh_SP_plasma_disruption_1D_analytic
     public :: small_dataset
     public :: define_mesh_bandaru
     public :: define_mesh_bandaru_FFT
     public :: define_mesh_MHD_Shercliff_plane
     public :: define_mesh_MHD_Shercliff_plane_FFT
     public :: define_mesh_full_BC_w_vacuum_symmetric
     public :: define_mesh_full_BC_n_vacuum_symmetric
     public :: define_mesh_full_BC_w_vacuum_3D
     public :: define_mesh_full_BC_w_vacuum_3D_high_res
     public :: define_mesh_full_BC_n_vacuum_3D
     public :: define_mesh_SP_MHD_LDC_Sergey_uniform
     public :: define_mesh_hydro_LDC_Ghia

     contains

     subroutine define_mesh_SP_plasma_disruption(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_wall,t_fluid,CR,buffer
       integer :: N,N_w
       CR = 10.0_cp ! Cavity Ratio
       t_fluid = 1.0_cp; t_wall = 0.05_cp; buffer = 1.5_cp;
       N = 100; N_w = 14
       ! N = 30; N_w = 4
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_Roberts_B'   ,N*2 ,-t_fluid*CR,t_fluid*CR,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_B'   ,N   ,-t_fluid   ,t_fluid   ,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B'   ,N   ,-t_fluid   ,t_fluid   ,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call add_ext(SP%MP_sigma,seg_1d(1,'ext_Roberts_B_IO',N_w*2,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(2,'ext_Roberts_B_IO',N_w  ,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(3,'ext_Roberts_B_IO',N_w  ,t_wall,buffer))
       call init(SP%MP_ind,SP%MP_sigma)
     end subroutine

     subroutine define_mesh_SP_plasma_disruption_plane(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_wall,t_fluid,buffer
       integer :: N,N_w
       t_fluid = 1.0_cp; t_wall = 0.05_cp; buffer = 1.5_cp;
       N = 130; N_w = 14
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_uniform'  ,1 ,-0.5_cp ,0.5_cp ,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_B',N ,-t_fluid,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B',N ,-t_fluid,t_fluid,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call add_ext(SP%MP_sigma,seg_1d(2,'ext_Roberts_B_IO',N_w,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(3,'ext_Roberts_B_IO',N_w,t_wall,buffer))
       call init(SP%MP_ind,SP%MP_sigma)
     end subroutine

     subroutine define_mesh_SP_plasma_disruption_plane_insulating(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_wall,t_fluid,buffer
       integer :: N,N_w
       t_fluid = 1.0_cp; t_wall = 0.05_cp; buffer = 1.5_cp;
       N = 150; N_w = 14
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_uniform'  ,1 ,-0.5_cp ,0.5_cp ,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_B',N ,-t_fluid,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B',N ,-t_fluid,t_fluid,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call init(SP%MP_ind,SP%MP_sigma)
     end subroutine

     subroutine define_mesh_SP_plasma_disruption_line(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_wall,t_fluid,buffer
       integer :: N,N_w
       t_fluid = 1.0_cp; t_wall = 0.05_cp; buffer = 1.5_cp;
       N = 200; N_w = 14
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_uniform'  ,1 ,-0.5_cp ,0.5_cp ,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_uniform'  ,1 ,-0.5_cp ,0.5_cp ,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B',N ,-t_fluid,t_fluid,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call add_ext(SP%MP_sigma,seg_1d(3,'ext_Roberts_B_IO',N_w,t_wall,buffer))
       call init(SP%MP_ind,SP%MP_sigma)
     end subroutine

     subroutine define_mesh_SP_plasma_disruption_1D_analytic(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_fluid,buffer
       integer :: N
       t_fluid = 1.0_cp; buffer = 1.5_cp;
       N = 100
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_uniform'     ,1 ,-t_fluid,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_uniform'     ,1 ,-t_fluid,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B'   ,N ,-t_fluid,t_fluid,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call init(SP%MP_ind,SP%MP_sigma)
     end subroutine

     subroutine small_dataset(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_fluid,buffer
       integer :: N
       t_fluid = 1.0_cp; buffer = 1.0_cp; N = 5;
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_Roberts_B'   ,N ,-t_fluid,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_B'   ,N ,-t_fluid,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B'   ,N ,-t_fluid,t_fluid,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call init(SP%MP_ind,SP%MP_sigma)
     end subroutine

     subroutine define_mesh_bandaru(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: buffer
       integer :: N
       buffer = 1.0_cp; N = 130
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_uniform'  ,N,0.0_cp ,2.0_cp*PI,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B',N,-1.0_cp,1.0_cp,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_uniform'  ,1,-0.5_cp,0.5_cp,buffer))
       call init(SP%MP_ind,SP%MP_mom)
       call init(SP%MP_sigma,SP%MP_ind)
     end subroutine

     subroutine define_mesh_bandaru_FFT(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: buffer
       integer :: N
       buffer = 1.0_cp; N = 128
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_uniform',N,0.0_cp ,2.0_cp*PI,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_uniform',N*2,-1.0_cp,1.0_cp,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_uniform',1,-0.5_cp,0.5_cp,buffer))
       call init(SP%MP_ind,SP%MP_mom)
       call init(SP%MP_sigma,SP%MP_ind)
     end subroutine

     subroutine define_mesh_MHD_Shercliff_plane(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: buffer
       integer :: N
       buffer = 1.0_cp; N = 150
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_uniform'  ,1,-0.5_cp,0.5_cp,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_B',N,-1.0_cp,1.0_cp,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B',N,-1.0_cp,1.0_cp,buffer))
       call init(SP%MP_ind,SP%MP_mom)
       call init(SP%MP_sigma,SP%MP_ind)
     end subroutine

     subroutine define_mesh_MHD_Shercliff_plane_FFT(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: buffer
       integer :: N
       buffer = 1.0_cp; N = 128
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_uniform'  ,1,-0.5_cp,0.5_cp,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_uniform'  ,N,-0.5_cp,0.5_cp,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_uniform'  ,N,-0.5_cp,0.5_cp,buffer))
       call init(SP%MP_ind,SP%MP_mom)
       call init(SP%MP_sigma,SP%MP_ind)
     end subroutine

     subroutine define_mesh_full_BC_w_vacuum_symmetric(SP) ! Correctness confirmed 11/27/2017
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_wall,t_fluid,t_vac,buffer
       integer :: N,N_w,N_half
       t_vac = 7.0_cp; t_fluid = 1.0_cp; t_wall = 0.05_cp; buffer = 2.0_cp; N = 80; N_w = 9; N_half = ceiling(N/2.0_cp)
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_Roberts_B'         ,N       ,-t_fluid        ,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_L'         ,N_half  ,-t_fluid        ,0.0_cp ,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_R'         ,N_half+5, 0.0_cp         ,t_fluid,buffer))
       call add_ext( SP%MP_mom,seg_1d(2,'ext_prep_Roberts_C2F_IO',N_half  , 0.0_cp+t_fluid ,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call add_ext(SP%MP_sigma,seg_1d(1,'ext_Roberts_B_IO'      ,N_w-1,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(3,'ext_prep_Roberts_B_IO' ,N_w-1,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(2,'ext_prep_Roberts_B_IO' ,N_w-1,t_wall,buffer))
       call init(SP%MP_ind,SP%MP_sigma)
       call add_ext(SP%MP_ind,seg_1d(1,'ext_Roberts_near_IO'      ,  N_w+1,t_vac - t_fluid - t_wall,buffer))
       call add_ext(SP%MP_ind,seg_1d(3,'ext_prep_Roberts_R_IO'    ,  N_w+2,t_vac - t_fluid - t_wall,buffer))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_prep_Roberts_R_IO'    ,  N_w-1,t_vac - t_fluid - t_wall,buffer))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_app_Roberts_L_IO'     ,2*N_w-2,t_vac - t_fluid         ,buffer))
     end subroutine

     subroutine define_mesh_full_BC_n_vacuum_symmetric(SP) ! Correctness confirmed 11/27/2017
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_wall,t_fluid,t_vac,buffer
       integer :: N,N_w,N_half
       t_vac = 7.0_cp; t_fluid = 1.0_cp; t_wall = 0.05_cp; buffer = 2.0_cp; N = 80; N_w = 9; N_half = ceiling(N/2.0_cp)
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_Roberts_B'         ,N       ,-t_fluid        ,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_L'         ,N_half  ,-t_fluid        ,0.0_cp ,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_R'         ,N_half+5, 0.0_cp         ,t_fluid,buffer))
       call add_ext( SP%MP_mom,seg_1d(2,'ext_prep_Roberts_C2F_IO',N_half  , 0.0_cp+t_fluid ,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call add_ext(SP%MP_sigma,seg_1d(1,'ext_Roberts_B_IO'      ,N_w-1,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(3,'ext_prep_Roberts_B_IO' ,N_w-1,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(2,'ext_prep_Roberts_B_IO' ,N_w-1,t_wall,buffer))
       call init(SP%MP_ind,SP%MP_sigma)
     end subroutine

     subroutine define_mesh_full_BC_w_vacuum_3D(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_wall,t_fluid,t_vac,buffer
       integer :: N,N_w,N_half
       t_vac = 7.0_cp; t_fluid = 1.0_cp; t_wall = 0.05_cp;
       buffer = 2.0_cp; N = 80; N_w = 9; N_half = ceiling(N/2.0_cp)
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_Roberts_B'         ,N       ,-t_fluid        ,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B'         ,N       ,-t_fluid        ,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_R'         ,N_half+5, 0.0_cp         ,t_fluid,buffer))
       call add_ext( SP%MP_mom,seg_1d(2,'ext_prep_Roberts_C2F_IO',N_half  , 0.0_cp+t_fluid ,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call add_ext(SP%MP_sigma,seg_1d(1,'ext_Roberts_B_IO'      ,N_w-1,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(3,'ext_Roberts_B_IO'      ,N_w-1,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(2,'ext_prep_Roberts_B_IO' ,N_w-1,t_wall,buffer))
       call init(SP%MP_ind,SP%MP_sigma)
       call add_ext(SP%MP_ind,seg_1d(1,'ext_Roberts_near_IO'      ,  N_w+1,t_vac - t_fluid - t_wall,buffer))
       call add_ext(SP%MP_ind,seg_1d(3,'ext_Roberts_near_IO'      ,  N_w+1,t_vac - t_fluid - t_wall,buffer))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_prep_Roberts_R_IO'    ,  N_w-1,t_vac - t_fluid - t_wall,buffer))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_app_Roberts_L_IO'     ,2*N_w-2,t_vac - t_fluid         ,buffer))
     end subroutine

     subroutine define_mesh_full_BC_w_vacuum_3D_high_res(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_wall,t_fluid,t_vac,buffer
       integer :: N,N_w,N_half
       t_vac = 7.0_cp; t_fluid = 1.0_cp; t_wall = 0.05_cp;
       buffer = 2.0_cp; N = 100; N_w = 9; N_half = ceiling(N/2.0_cp)
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_Roberts_B'         ,N       ,-t_fluid        ,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B'         ,N       ,-t_fluid        ,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_R'         ,N_half+5, 0.0_cp         ,t_fluid,buffer))
       call add_ext( SP%MP_mom,seg_1d(2,'ext_prep_Roberts_C2F_IO',N_half  , 0.0_cp+t_fluid ,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call add_ext(SP%MP_sigma,seg_1d(1,'ext_Roberts_B_IO'      ,N_w-1,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(3,'ext_Roberts_B_IO'      ,N_w-1,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(2,'ext_prep_Roberts_B_IO' ,N_w-1,t_wall,buffer))
       call init(SP%MP_ind,SP%MP_sigma)
       call add_ext(SP%MP_ind,seg_1d(1,'ext_Roberts_near_IO'      ,  N_w+1,t_vac - t_fluid - t_wall,buffer))
       call add_ext(SP%MP_ind,seg_1d(3,'ext_Roberts_near_IO'      ,  N_w+1,t_vac - t_fluid - t_wall,buffer))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_prep_Roberts_R_IO'    ,  N_w-1,t_vac - t_fluid - t_wall,buffer))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_app_Roberts_L_IO'     ,2*N_w-2,t_vac - t_fluid         ,buffer))
     end subroutine

     subroutine define_mesh_full_BC_n_vacuum_3D(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: t_wall,t_fluid,t_vac,buffer
       integer :: N,N_w,N_half
       t_vac = 7.0_cp; t_fluid = 1.0_cp; t_wall = 0.05_cp;
       buffer = 2.0_cp; N = 80; N_w = 9; N_half = ceiling(N/2.0_cp)
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_Roberts_B'         ,N       ,-t_fluid        ,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_Roberts_B'         ,N       ,-t_fluid        ,t_fluid,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_Roberts_R'         ,N_half+5, 0.0_cp         ,t_fluid,buffer))
       call add_ext( SP%MP_mom,seg_1d(2,'ext_prep_Roberts_C2F_IO',N_half  , 0.0_cp+t_fluid ,buffer))
       call init(SP%MP_sigma,SP%MP_mom)
       call add_ext(SP%MP_sigma,seg_1d(1,'ext_Roberts_B_IO'      ,N_w-1,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(3,'ext_Roberts_B_IO'      ,N_w-1,t_wall,buffer))
       call add_ext(SP%MP_sigma,seg_1d(2,'ext_prep_Roberts_B_IO' ,N_w-1,t_wall,buffer))
       call init(SP%MP_ind,SP%MP_sigma)
     end subroutine

     subroutine define_mesh_SP_MHD_LDC_Sergey_uniform(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_uniform',45,-1.0_cp,1.0_cp,1.0_cp))
       call add_base(SP%MP_mom,seg_1d(2,'grid_uniform',45,-1.0_cp,1.0_cp,1.0_cp))
       call add_base(SP%MP_mom,seg_1d(3,'grid_uniform',45,-1.0_cp,1.0_cp,1.0_cp))
       call init(SP%MP_ind,SP%MP_mom)
       call init(SP%MP_sigma,SP%MP_ind)
       call add_ext(SP%MP_ind,seg_1d(1,'ext_uniform_IO',11))
       call add_ext(SP%MP_ind,seg_1d(2,'ext_uniform_IO',11))
       call add_ext(SP%MP_ind,seg_1d(3,'ext_uniform_IO',11))
     end subroutine

     subroutine define_mesh_hydro_LDC_Ghia(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       real(cp) :: buffer
       integer :: N
       buffer = 1.0_cp; N = 64
       call init(SP%MP_mom,SP%MQP)
       call init(SP%MP_sigma,SP%MQP)
       call init(SP%MP_ind,SP%MQP)
       call add_base(SP%MP_mom,seg_1d(1,'grid_uniform',N,-1.0_cp,1.0_cp,buffer))
       call add_base(SP%MP_mom,seg_1d(2,'grid_uniform',N,-1.0_cp,1.0_cp,buffer))
       call add_base(SP%MP_mom,seg_1d(3,'grid_uniform',1,-0.5_cp,0.5_cp,buffer))
       call init(SP%MP_ind,SP%MP_mom)
       call init(SP%MP_sigma,SP%MP_ind)
     end subroutine

     end module