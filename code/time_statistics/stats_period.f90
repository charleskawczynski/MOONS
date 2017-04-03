       module stats_period_mod
       use current_precision_mod
       use time_marching_params_mod

       implicit none
       private

       public :: stats_period
       public :: init,delete,display,print,export,import ! Essentials

       public :: update
       public :: set_exported_stats
       public :: add_stat

       type stats_period
         real(cp) :: t_start = 0.0_cp
         real(cp) :: t_stop = 0.0_cp
         real(cp) :: period = 0.0_cp
         integer :: N_stats_collected = 0
         logical :: compute_stats = .false.
         logical :: export_stats = .false.
         logical :: exported_stats = .false.
       end type

       interface init;               module procedure init_SP;               end interface
       interface init;               module procedure init_copy_SP;          end interface
       interface delete;             module procedure delete_SP;             end interface
       interface display;            module procedure display_SP;            end interface
       interface print;              module procedure print_SP;              end interface
       interface export;             module procedure export_SP;             end interface
       interface import;             module procedure import_SP;             end interface

       interface update;             module procedure update_SP;             end interface
       interface set_exported_stats; module procedure set_exported_stats_SP; end interface
       interface add_stat;           module procedure add_stat_SP;           end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_SP(SP,t_start,t_stop)
         implicit none
         type(stats_period),intent(inout) :: SP
         real(cp),intent(in) :: t_start,t_stop
         SP%period = t_stop - t_start
         if (SP%period.le.0.0_cp) stop 'Error: period must be > 0 in stats_period.f90'
         SP%t_start           = t_start
         SP%t_stop            = t_stop
         SP%exported_stats    = .false.
         SP%export_stats      = .false.
         SP%N_stats_collected = 0
         SP%compute_stats     = .false.
       end subroutine

       subroutine init_copy_SP(SP,SP_in)
         implicit none
         type(stats_period),intent(inout) :: SP
         type(stats_period),intent(in) :: SP_in
         SP%t_start           = SP_in%t_start
         SP%t_stop            = SP_in%t_stop
         SP%period            = SP_in%period
         SP%N_stats_collected = SP_in%N_stats_collected
         SP%compute_stats     = SP_in%compute_stats
         SP%export_stats      = SP_in%export_stats
         SP%exported_stats    = SP_in%exported_stats
       end subroutine

       subroutine delete_SP(SP)
         implicit none
         type(stats_period),intent(inout) :: SP
         SP%t_start           = 0.0_cp
         SP%t_stop            = 0.0_cp
         SP%period            = 0.0_cp
         SP%N_stats_collected = 0
         SP%compute_stats     = .false.
         SP%export_stats      = .false.
         SP%exported_stats    = .false.
       end subroutine

       subroutine display_SP(SP,un)
         implicit none
         type(stats_period),intent(in) :: SP
         integer,intent(in) :: un
         write(un,*) 't_start           = ',SP%t_start
         write(un,*) 't_stop            = ',SP%t_stop
         write(un,*) 'period            = ',SP%period
         write(un,*) 'N_stats_collected = ',SP%N_stats_collected
         write(un,*) 'compute_stats     = ',SP%compute_stats
         write(un,*) 'export_stats      = ',SP%export_stats
         write(un,*) 'exported_stats    = ',SP%exported_stats
       end subroutine

       subroutine print_SP(SP)
         implicit none
         type(stats_period),intent(in) :: SP
         call display(SP,6)
       end subroutine

       subroutine export_SP(SP,un)
         implicit none
         type(stats_period),intent(in) :: SP
         integer,intent(in) :: un
         write(un,*) 't_start           = '; write(un,*) SP%t_start
         write(un,*) 't_stop            = '; write(un,*) SP%t_stop
         write(un,*) 'period            = '; write(un,*) SP%period
         write(un,*) 'N_stats_collected = '; write(un,*) SP%N_stats_collected
         write(un,*) 'compute_stats     = '; write(un,*) SP%compute_stats
         write(un,*) 'export_stats      = '; write(un,*) SP%export_stats
         write(un,*) 'exported_stats    = '; write(un,*) SP%exported_stats
       end subroutine

       subroutine import_SP(SP,un)
         implicit none
         type(stats_period),intent(inout) :: SP
         integer,intent(in) :: un
         read(un,*); read(un,*) SP%t_start
         read(un,*); read(un,*) SP%t_stop
         read(un,*); read(un,*) SP%period
         read(un,*); read(un,*) SP%N_stats_collected
         read(un,*); read(un,*) SP%compute_stats
         read(un,*); read(un,*) SP%export_stats
         read(un,*); read(un,*) SP%exported_stats
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine update_SP(SP,TMP)
         implicit none
         type(stats_period),intent(inout) :: SP
         type(time_marching_params),intent(in) :: TMP
         logical,dimension(4) :: L
         L(1) = TMP%t.gt.SP%t_start
         L(2) = TMP%t.lt.SP%t_stop
         L(3) = .not.SP%exported_stats
         L(4) = TMP%t.gt.SP%t_stop
         SP%compute_stats = all(L(1:2))
         SP%export_stats = all(L(3:4))
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