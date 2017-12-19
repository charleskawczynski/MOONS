     module sim_params_aux_mod
     use current_precision_mod
     use constants_mod
     use IO_tools_mod
     use dir_tree_mod
     use var_mod
     use var_set_extend_mod
     use string_mod
     use path_extend_mod
     use segment_mod
     use dimensionless_params_extend_mod
     use time_marching_params_mod
     use time_marching_params_extend_mod
     use mesh_params_mod
     use mesh_quality_params_mod
     use export_planes_mod
     use export_lines_mod
     use sim_params_mod
     implicit none

     private
     public :: sanity_check
     public :: display_compiler_info
     public :: set_restart
     public :: post_process

     interface sanity_check;          module procedure sanity_check_SP;                  end interface
     interface display_compiler_info; module procedure display_compiler_info_SP;         end interface
     interface display_compiler_info; module procedure display_compiler_info_wrapper_SP; end interface
     interface set_restart;           module procedure set_restart_SP;                   end interface
     interface post_process;          module procedure post_process_SP;                  end interface

     contains

     subroutine post_process_SP(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       call sanity_check(SP)
       if (SP%SCP%couple_time_steps) call couple_time_step(SP%VS,SP%coupled)
       if (SP%MP%mirror) SP%DP%KE_scale = SP%DP%KE_scale*2.0_cp
       if (SP%MP%mirror) SP%DP%ME_scale = SP%DP%ME_scale*2.0_cp
       if (SP%MP%mirror) SP%DP%JE_scale = SP%DP%JE_scale*2.0_cp
     end subroutine

     subroutine set_restart_SP(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       SP%FCL%restart_meshes = .true.
       SP%VS%T%SS%restart    = .true.
       SP%VS%U%SS%restart    = .true.
       SP%VS%P%SS%restart    = .true.
       SP%VS%B%SS%restart    = .true.
       SP%VS%B0%SS%restart   = .true.
       SP%VS%phi%SS%restart  = .true.
       SP%VS%rho%SS%restart  = .true.
     end subroutine

     subroutine sanity_check_SP(SP)
       implicit none
       type(sim_params),intent(in) :: SP
       call sanity_check(SP%VS)
       call check_time_statistics_window(SP)
     end subroutine

     function alpha_in_window(alpha,min_val,max_val) result(L)
       implicit none
       real(cp),intent(in) :: alpha,min_val,max_val
       logical :: L
       L = (alpha.gt.min_val).and.(alpha.lt.max_val)
     end function

     subroutine check_time_statistics_window(SP)
       implicit none
       type(sim_params),intent(in) :: SP
       logical,dimension(4) :: L_temp
       if (SP%TSP%collect) then
         L_temp(1) = alpha_in_window(SP%TSP%O1_stats%t_start,SP%DP%t_start,SP%DP%t_final)
         L_temp(2) = alpha_in_window(SP%TSP%O1_stats%t_stop ,SP%DP%t_start,SP%DP%t_final)
         L_temp(3) = alpha_in_window(SP%TSP%O2_stats%t_start,SP%DP%t_start,SP%DP%t_final)
         L_temp(4) = alpha_in_window(SP%TSP%O2_stats%t_stop ,SP%DP%t_start,SP%DP%t_final)
         if (.not.all(L_temp)) then
           write(*,*) ''
           write(*,*) 'Error: time-statistics window extends beyond'
           write(*,*) 'the simulation time. Ending execution.'
           write(*,*) 'SP%DP%t_start           = ',SP%DP%t_start
           write(*,*) 'SP%DP%t_final           = ',SP%DP%t_final
           write(*,*) 'SP%TSP%O1_stats%t_start = ',SP%TSP%O1_stats%t_start
           write(*,*) 'SP%TSP%O1_stats%t_stop  = ',SP%TSP%O1_stats%t_stop
           write(*,*) 'SP%TSP%O2_stats%t_start = ',SP%TSP%O2_stats%t_start
           write(*,*) 'SP%TSP%O2_stats%t_stop  = ',SP%TSP%O2_stats%t_stop
           stop 'Done in check_time_statistics_window in sim_params_aux.f90'
         endif
       endif
     end subroutine

     subroutine display_compiler_info_SP(un)
       implicit none
       integer,intent(in) :: un
       ! call get_environment_variable(name[, value, length, status, trim_name)
       write(un,*) ' ----------------- COMPILER FLAG INFO -------------- '
#ifdef _PARALLELIZE_GF_
       write(un,*) '_PARALLELIZE_GF_ = .true.'
#else
       write(un,*) '_PARALLELIZE_GF_ = .false.'
#endif
#ifdef _PARALLELIZE_BF_PLANE_
       write(un,*) '_PARALLELIZE_BF_PLANE_ = .true.'
#else
       write(un,*) '_PARALLELIZE_BF_PLANE_ = .false.'
#endif
#ifdef PARALLELIZE_2D_OPS
       write(un,*) 'PARALLELIZE_2D_OPS = .true.'
#else
       write(un,*) 'PARALLELIZE_2D_OPS = .false.'
#endif
#ifdef PARALLELIZE_1D_OPS
       write(un,*) 'PARALLELIZE_1D_OPS = .true.'
#else
       write(un,*) 'PARALLELIZE_1D_OPS = .false.'
#endif
     end subroutine

       subroutine display_compiler_info_wrapper_SP(dir,name)
         implicit none
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display_compiler_info(un)
         close(un)
       end subroutine

     end module