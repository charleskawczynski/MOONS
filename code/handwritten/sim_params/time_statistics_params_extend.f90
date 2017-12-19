       module time_statistics_params_extend_mod
       use time_statistics_params_mod
       use current_precision_mod
       use stats_period_mod
       use stats_period_extend_mod
       use time_marching_params_mod

       implicit none
       private

       public :: time_statistics_params
       public :: init
       public :: update

       interface init;    module procedure init_TSP;      end interface
       interface update;  module procedure update_TSP;    end interface

       contains

       subroutine init_TSP(TSP,collect,t_start,t_stop)
         implicit none
         type(time_statistics_params),intent(inout) :: TSP
         logical,intent(in) :: collect
         real(cp),intent(in) :: t_start,t_stop
         call delete(TSP)
         TSP%collect = collect
         call init(TSP%O1_stats,t_start,t_stop)
         call init(TSP%O2_stats,t_stop,t_stop+TSP%O1_stats%period)
       end subroutine

       subroutine update_TSP(TSP,TMP)
         implicit none
         type(time_statistics_params),intent(inout) :: TSP
         type(time_marching_params),intent(in) :: TMP
         call update(TSP%O1_stats,TMP)
         call update(TSP%O2_stats,TMP)
       end subroutine

       end module