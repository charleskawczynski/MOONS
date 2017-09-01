       module stats_period_extend_mod
       use stats_period_mod
       use current_precision_mod
       use time_marching_params_mod

       implicit none
       private

       public :: init

       public :: update
       public :: set_exported_stats
       public :: add_stat

       interface init;               module procedure init_SP;               end interface
       interface update;             module procedure update_SP;             end interface
       interface set_t_start_actual; module procedure set_t_start_actual_SP; end interface
       interface set_exported_stats; module procedure set_exported_stats_SP; end interface
       interface add_stat;           module procedure add_stat_SP;           end interface

       contains

       subroutine init_SP(SP,t_start,t_stop)
         implicit none
         type(stats_period),intent(inout) :: SP
         real(cp),intent(in) :: t_start,t_stop
         SP%period = t_stop - t_start
         if (SP%period.le.0.0_cp) stop 'Error: period must be > 0 in stats_period.f90'
         SP%t_start                = t_start
         SP%t_start_actual         = t_start
         SP%t_stop                 = t_stop
         SP%exported_stats         = .false.
         SP%export_stats           = .false.
         SP%N_stats_collected      = 0
         SP%compute_stats          = .false.
         SP%define_t_start_actual  = .false.
         SP%t_start_actual_defined = .false.
       end subroutine

       subroutine update_SP(SP,TMP)
         implicit none
         type(stats_period),intent(inout) :: SP
         type(time_marching_params),intent(in) :: TMP
         logical,dimension(4) :: L
         real(cp) :: tol
         tol = TMP%dt*10.0_cp**(-6.0_cp)
         L(1) = TMP%t.gt.SP%t_start+tol
         L(2) = TMP%t.lt.SP%t_stop+tol
         L(3) = .not.SP%exported_stats
         L(4) = TMP%t.gt.SP%t_stop+tol
         SP%compute_stats = all(L(1:2))
         SP%export_stats = all(L(3:4))
         if (.not.SP%t_start_actual_defined) call set_t_start_actual(SP,TMP)
       end subroutine

       subroutine set_t_start_actual_SP(SP,TMP)
         !
         !
         !           |
         !           |          t_start specified
         !           |             |
         !           |             v
         !           |---|---|---|---|---|---|---|---|---|--->t
         !           |           ^
         !           |           |
         !           |      t_start_actual (due to potentially variable dt)
         !
         ! Condition: t_start_actual = t WHEN:
         !                1) t_start>t-dt
         !                1) t_start<t
         !
         implicit none
         type(stats_period),intent(inout) :: SP
         type(time_marching_params),intent(in) :: TMP
         logical,dimension(2) :: L
         real(cp) :: tol
         tol = TMP%dt*10.0_cp**(-6.0_cp)
         L(1) = SP%t_start.gt.TMP%t-TMP%dt-tol
         L(2) = SP%t_start.lt.TMP%t+tol
         SP%define_t_start_actual = all(L).and.(.not.SP%t_start_actual_defined)
         if (SP%define_t_start_actual) then
           SP%t_start_actual = TMP%t
           SP%t_start_actual_defined = .true.
         endif
       end subroutine

       subroutine set_exported_stats_SP(SP)
         implicit none
         type(stats_period),intent(inout) :: SP
         SP%exported_stats = .true.
       end subroutine

       subroutine add_stat_SP(SP)
         implicit none
         type(stats_period),intent(inout) :: SP
         SP%N_stats_collected = SP%N_stats_collected+1
       end subroutine

       end module