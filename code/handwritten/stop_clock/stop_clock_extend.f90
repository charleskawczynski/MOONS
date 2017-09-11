       module stop_clock_extend_mod
       use stop_clock_mod
       ! Fixes/improvements:
       ! - include output time to file (similar to myError)
       ! - make set/get functions to make components accessable to myEfficiency

       ! - include parallel clock?
       ! - efficiency = runtime/step (optional)
       ! - efficiency = runtime*L2norm (think error vs run time ~ 1/t, we want closest to origin)
       use current_precision_mod
       use IO_tools_mod
       use clock_mod
       use clock_extend_mod
       use string_mod
       use time_marching_params_mod
       use unit_conversion_mod
       use unit_conversion_extend_mod
       implicit none

       private
       public :: init,delete,display,print,export,import
       public :: export_light,print_light
       public :: tic,toc

       interface init;         module procedure init_sc;         end interface
       interface tic;          module procedure tic_sc;          end interface
       interface toc;          module procedure toc_sc;          end interface
       interface print_light;  module procedure print_light_sc;  end interface
       interface export;       module procedure export_sc;       end interface
       interface export_light; module procedure export_light_sc; end interface
       interface export;       module procedure export_sc_dir;   end interface
       interface print;        module procedure print_sc;        end interface
       interface export;       module procedure export_plot_sc;  end interface

       contains

      subroutine init_sc(sc,dir,name)
        implicit none
        type(stop_clock),intent(inout) :: sc
        character(len=*),intent(in) :: dir,name
        type(string) :: vars
        call delete(sc)
        call init(sc%c)
        call init(sc%dir,dir)
        call init(sc%name,name)

        sc%un_plot = new_and_open(dir,name//'_plot')

        call init(vars,'VARIABLES = ')
        call append(vars,'t,')
        call append(vars,'t_elapsed,')
        call append(vars,'t_elapsed(raw),')
        call append(vars,'estimated_total_m,')
        call append(vars,'estimated_total_h,')
        call append(vars,'estimated_total_d')

        write(sc%un_plot,*) 'TITLE = "WALL_CLOCK_TIME_INFO"'
        write(sc%un_plot,*) str(vars)
        write(sc%un_plot,*) 'ZONE DATAPACKING = POINT'
        flush(sc%un_plot)
        call delete(vars)
        call init(sc%uc)
      end subroutine

      subroutine tic_sc(sc)
        implicit none
        type(stop_clock),intent(inout) :: sc
        call tic(sc%c)
      end subroutine

      subroutine toc_sc(sc,TMP)
        implicit none
        type(stop_clock),intent(inout) :: sc
        type(time_marching_params),intent(in) :: TMP
        call toc(sc%c)
        if (sc%c%t_elapsed.lt.0.0_cp) then; sc%frozen_elapsed = .true.
        else;                               sc%t_elapsed = sc%c%t_elapsed
        endif
        sc%t_passed = sc%t_passed + sc%t_elapsed
        sc%seconds_per_step = sc%t_elapsed
        sc%sim_time_per_sec = TMP%dt/sc%seconds_per_step
        sc%estimated_remaining = (TMP%t_final-TMP%t)/sc%sim_time_per_sec
        sc%estimated_total = sc%t_passed + sc%estimated_remaining
        sc%percentage_complete_RB = (sc%estimated_total-sc%estimated_remaining)/sc%estimated_total*100.0_cp
        sc%percentage_complete_SB = TMP%t/TMP%t_final*100.0_cp
      end subroutine

      subroutine export_plot_sc(sc,t)
        implicit none
        type(stop_clock),intent(in) :: sc
        real(cp),intent(in) :: t
        write(sc%un_plot,*) t,sc%t_elapsed,sc%c%t_elapsed,&
                            sc%estimated_total/sc%uc%seconds_per_minute,&
                            sc%estimated_total/sc%uc%seconds_per_hour,&
                            sc%estimated_total/sc%uc%seconds_per_day
        flush(sc%un_plot)
      end subroutine

      subroutine export_sc_dir(sc,TMP)
        implicit none
        type(stop_clock),intent(in) :: sc
        type(time_marching_params),intent(in) :: TMP
        integer :: un
        un = new_and_open(str(sc%dir),str(sc%name))
        call export(sc,TMP,un)
        call close_and_message(un,str(sc%dir),str(sc%name))
      end subroutine

      subroutine print_sc(sc,TMP)
        implicit none
        type(stop_clock),intent(in) :: sc
        type(time_marching_params),intent(in) :: TMP
        call export(sc,TMP,6)
      end subroutine

      subroutine export_sc(sc,TMP,un)
        implicit none
        type(stop_clock),intent(in) :: sc
        type(time_marching_params),intent(in) :: TMP
        integer,intent(in) :: un
        real(cp) :: temp
        character(len=1) :: u
        write(un,*) ''
        write(un,*) ' ****************************** CLOCK INFO ************************* '
        call negative_time_elapsed_reported(sc)

        write(un,*) 'Convective time'
        write(un,*) '     per hour             = ',sc%sim_time_per_sec*sc%uc%seconds_per_hour
        write(un,*) '     per day              = ',sc%sim_time_per_sec*sc%uc%seconds_per_day
        ! Or, as Eldredge did it:
        ! CPU_TIME/(convective unit)
        ! CPU_TIME/(convective unit)/unknown
        ! Where unknowns = (3 momentum + 3 induction)*problem size
        write(un,*) '     now                  = ',TMP%t
        write(un,*) '     final                = ',TMP%t_final
        write(un,*) ''

        write(un,*) 'Wall clock time '
        temp = sc%seconds_per_step; call getTimeWithUnits(temp,u,sc%uc)
        write(un,*) '     per step         (', u,') = ',temp
        temp = sc%estimated_total; call getTimeWithUnits(temp,u,sc%uc)
        write(un,*) '     total            (', u,') = ',temp

        temp = sc%estimated_remaining; call getTimeWithUnits(temp,u,sc%uc)
        write(un,*) '     remaining        (', u,') = ',temp

        temp = sc%t_passed; call getTimeWithUnits(temp,u,sc%uc)
        write(un,*) '     completed        (', u,') = ',temp
        write(un,*) ''
        write(un,*) 'Percentage complete'
        write(un,*) '     Rate  based estimate = ',sc%percentage_complete_RB
        write(un,*) '     State based estimate = ',sc%percentage_complete_SB

        write(un,*) ' ******************************************************************* '
      end subroutine

      subroutine print_light_sc(sc,TMP)
        implicit none
        type(stop_clock),intent(in) :: sc
        type(time_marching_params),intent(in) :: TMP
        call export_light(sc,TMP,6)
      end subroutine

      subroutine export_light_sc(sc,TMP,un)
        implicit none
        type(stop_clock),intent(in) :: sc
        type(time_marching_params),intent(in) :: TMP
        integer,intent(in) :: un
        real(cp) :: temp
        character(len=1) :: u
        call negative_time_elapsed_reported(sc)
        temp = sc%t_passed; call getTimeWithUnits(temp,u,sc%uc)
        write(un,*) ''
        write(un,*) 'Wall clock time *completed* (', u,') = ',temp
        write(un,*) 'Time (convective),n_step = ',TMP%t,TMP%n_step
        write(un,*) 'Convective time'
        write(un,*) '     per hour       = ',sc%uc%seconds_per_hour/sc%seconds_per_step*TMP%dt
        write(un,*) '     per day        = ',sc%uc%seconds_per_day/sc%seconds_per_step*TMP%dt
        write(un,*) '--------------------------------------------------------------'
      end subroutine

      subroutine getTimeWithUnits(t,u,uc)
        implicit none
        real(cp),intent(inout) :: t
        character(len=1),intent(inout) :: u
        type(unit_conversion),intent(in) :: uc
        if (t.lt.uc%seconds_per_minute) then
         u = 's'; t = t
        elseif ((t.ge.uc%seconds_per_minute).and.(t.lt.uc%seconds_per_hour)) then
         u = 'm'; t = t*uc%minute_per_seconds
        elseif ((t.ge.uc%seconds_per_hour).and.(t.lt.uc%seconds_per_day)) then
         u = 'h'; t = t*uc%hour_per_seconds
        elseif ((t.ge.uc%seconds_per_day).and.(t.lt.uc%seconds_per_year)) then
         u = 'd'; t = t*uc%day_per_seconds
        elseif (t.ge.uc%seconds_per_year) then
         u = 'y'; t = t*uc%year_per_seconds
        endif
       end subroutine

      subroutine negative_time_elapsed_reported(sc)
        implicit none
        type(stop_clock),intent(in) :: sc
        if (sc%frozen_elapsed) then
          write(*,*) 'WARNING: negative time estimate in stop_clock.f90'
          write(*,*) 'Using last positive elapsed time for time estimates'
        endif
       end subroutine

       end module