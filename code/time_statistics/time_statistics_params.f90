       module time_statistics_params_mod
       use current_precision_mod
       use stats_period_mod
       use time_marching_params_mod

       implicit none
       private

       public :: time_statistics_params
       public :: init,delete,display,print,export,import ! Essentials

       public :: update

       type time_statistics_params
         logical :: collect = .false.
         type(stats_period) :: O1_stats
         type(stats_period) :: O2_stats
       end type

       interface init;    module procedure init_TSP;      end interface
       interface init;    module procedure init_copy_TSP; end interface
       interface delete;  module procedure delete_TSP;    end interface
       interface display; module procedure display_TSP;   end interface
       interface print;   module procedure print_TSP;     end interface
       interface export;  module procedure export_TSP;    end interface
       interface import;  module procedure import_TSP;    end interface

       interface update;  module procedure update_TSP;    end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

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

       subroutine init_copy_TSP(TSP,TSP_in)
         implicit none
         type(time_statistics_params),intent(inout) :: TSP
         type(time_statistics_params),intent(in) :: TSP_in
         TSP%collect = TSP_in%collect
         call init(TSP%O1_stats,TSP_in%O1_stats)
         call init(TSP%O2_stats,TSP_in%O2_stats)
       end subroutine

       subroutine delete_TSP(TSP)
         implicit none
         type(time_statistics_params),intent(inout) :: TSP
         TSP%collect = .false.
         call delete(TSP%O1_stats)
         call delete(TSP%O2_stats)
       end subroutine

       subroutine display_TSP(TSP,un)
         implicit none
         type(time_statistics_params),intent(in) :: TSP
         integer,intent(in) :: un
         write(un,*) ' collect = ',TSP%collect
         write(un,*) ' ------------ O1_stats ------------ '
         call display(TSP%O1_stats,un)
         write(un,*) ' ------------ O2_stats ------------ '
         call display(TSP%O2_stats,un)
       end subroutine

       subroutine print_TSP(TSP)
         implicit none
         type(time_statistics_params),intent(in) :: TSP
         call display(TSP,6)
       end subroutine

       subroutine export_TSP(TSP,un)
         implicit none
         type(time_statistics_params),intent(in) :: TSP
         integer,intent(in) :: un
         write(un,*) ' collect = '; write(un,*) TSP%collect
         call export(TSP%O1_stats,un)
         call export(TSP%O2_stats,un)
       end subroutine

       subroutine import_TSP(TSP,un)
         implicit none
         type(time_statistics_params),intent(inout) :: TSP
         integer,intent(in) :: un
         read(un,*); read(un,*) TSP%collect
         call import(TSP%O1_stats,un)
         call import(TSP%O2_stats,un)
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine update_TSP(TSP,TMP)
         implicit none
         type(time_statistics_params),intent(inout) :: TSP
         type(time_marching_params),intent(in) :: TMP
         call update(TSP%O1_stats,TMP)
         call update(TSP%O2_stats,TMP)
       end subroutine

       end module