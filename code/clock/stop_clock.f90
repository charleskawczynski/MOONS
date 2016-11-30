       module stop_clock_mod
       ! Fixes/improvements:
       ! - include output time to file (similar to myError)
       ! - make set/get functions to make components accessable to myEfficiency

       ! - include parallel clock?
       ! - efficiency = runtime/step (optional)
       ! - efficiency = runtime*L2norm (think error vs run time ~ 1/t, we want closest to origin)
       use current_precision_mod
       use IO_tools_mod
       use clock_mod
       use string_mod
       use unit_conversion_mod
       implicit none

       private
       public :: stop_clock
       public :: init,delete,export,print
       public :: tic,toc
       public :: reset_Nmax

       type stop_clock
        ! Known Quantities
        type(unit_conversion) :: uc
        type(clock) :: c
        type(string) :: dir,name
        real(cp) :: seconds_per_iter
        real(cp) :: t_passed

        integer(li) :: N,Nmax,NRemaining

        real(cp) :: Nr,Nmaxr
        ! Estimated Quantities
        real(cp) :: estimated_total
        real(cp) :: estimated_remaining
        real(cp) :: percentage_complete,t_elapsed

        integer :: iterPerSec
        integer :: iterPerMin
        integer :: iterPerHour
        integer :: iterPerDay
        logical :: frozen_elapsed = .false. ! For when elapsed is returned negative
        integer :: un_plot
       end type

       interface init;      module procedure init_sc;         end interface
       interface delete;    module procedure delete_sc;       end interface
       interface tic;       module procedure tic_sc;          end interface
       interface toc;       module procedure toc_sc;          end interface
       interface print;     module procedure print_sc;        end interface
       interface export;    module procedure export_sc;       end interface
       interface export;    module procedure export_sc_dir;   end interface
       interface export;    module procedure export_plot_sc;  end interface

       interface reset_Nmax;module procedure reset_Nmax_sc;   end interface

       contains

      subroutine init_sc(sc,Nmax,dir,name)
        implicit none
        type(stop_clock),intent(inout) :: sc
        integer(li),intent(in) :: Nmax
        character(len=*),intent(in) :: dir,name
        type(string) :: vars
        call delete(sc)
        call init(sc%c)
        sc%Nmax = Nmax
        sc%Nmaxr = real(sc%Nmax,cp)
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

      subroutine delete_sc(sc)
        implicit none
        type(stop_clock),intent(inout) :: sc
        call init(sc%c)
        sc%seconds_per_iter = 0.0_cp
        sc%t_passed = 0.0_cp
        sc%N = 0
        sc%Nmax = 0
        sc%NRemaining = 0
        sc%Nr = real(sc%N,cp)
        sc%Nmaxr = real(sc%Nmax,cp)
        sc%frozen_elapsed = .false.

        sc%estimated_total = 0.0_cp
        sc%estimated_remaining = 0.0_cp
        sc%percentage_complete = 0.0_cp

        sc%iterPerSec = 0
        sc%iterPerMin = 0
        sc%iterPerHour = 0
        sc%iterPerDay = 0
        close(sc%un_plot)
        call delete(sc%dir)
        call delete(sc%name)
      end subroutine

      subroutine tic_sc(sc)
        implicit none
        type(stop_clock),intent(inout) :: sc
        call tic(sc%c)
      end subroutine

      subroutine toc_sc(sc)
        implicit none
        type(stop_clock),intent(inout) :: sc
        call toc(sc%c)
        if (sc%c%t_elapsed.lt.0.0_cp) then; sc%frozen_elapsed = .true.
        else;                               sc%t_elapsed = sc%c%t_elapsed
        endif
        sc%t_passed = sc%t_passed + sc%t_elapsed
        sc%N = sc%N + 1
        sc%Nr = real(sc%N,cp)
        sc%seconds_per_iter = sc%t_passed/sc%Nr
        sc%estimated_total = sc%seconds_per_iter*sc%Nmaxr
        sc%estimated_remaining = sc%estimated_total - sc%t_passed
        sc%percentage_complete = sc%Nr/sc%Nmaxr*100.0_cp
        sc%NRemaining = sc%Nmax - sc%N + 1
        sc%iterPerSec = floor(1.0_cp/sc%seconds_per_iter)
        sc%iterPerMin = floor(sc%uc%seconds_per_minute/sc%seconds_per_iter)
        sc%iterPerHour = floor(sc%uc%seconds_per_hour/sc%seconds_per_iter)
        sc%iterPerDay = floor(sc%uc%seconds_per_day/sc%seconds_per_iter)
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

      subroutine export_sc_dir(sc)
        implicit none
        type(stop_clock),intent(in) :: sc
        integer :: un
        un = new_and_open(str(sc%dir),str(sc%name))
        call export_sc(sc,un)
        call close_and_message(un,str(sc%dir),str(sc%name))
      end subroutine

      subroutine print_sc(sc)
        implicit none
        type(stop_clock),intent(in) :: sc
        call export_sc(sc,6)
      end subroutine

      subroutine export_sc(sc,un)
        implicit none
        type(stop_clock),intent(in) :: sc
        integer,intent(in) :: un
        real(cp) :: temp
        character(len=1) :: u
        write(un,*) ''
        write(un,*) '******************* KNOWN WALL CLOCK TIME INFO *********************'
        call negative_time_elapsed_reported(sc)

        write(un,*) 'Iterations (complete) = ',sc%N

        temp = sc%seconds_per_iter; call getTimeWithUnits(temp,u,sc%uc)
        write(un,*) 'Time (seconds/iteration) = ',temp,' (', u,')'

        write(un,*) 'Iterations per (s,m,h,d) = ',sc%iterPerSec,sc%iterPerMin,sc%iterPerHour,sc%iterPerDay

        temp = sc%t_passed; call getTimeWithUnits(temp,u,sc%uc)
        write(un,*) 'Time (Total passed) = ',temp,' (', u,')'

        write(un,*) ''
        write(un,*) '***************** ESTIMATED WALL CLOCK TIME INFO *******************'

        write(un,*) 'Iterations (remaining/max) = ',sc%NRemaining,sc%Nmax

        temp = sc%estimated_total; call getTimeWithUnits(temp,u,sc%uc)
        write(un,*) 'Time (total) = ',temp,' (', u,')'

        temp = sc%estimated_remaining; call getTimeWithUnits(temp,u,sc%uc)
        write(un,*) 'Time (remaining) = ',temp,' (', u,')'

        write(un,*) 'Percentage complete = ',sc%percentage_complete

        write(un,*) ''
        write(un,*) '********************************************************************'
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

      subroutine reset_Nmax_sc(sc,Nmax)
        implicit none
        type(stop_clock),intent(inout) :: sc
        integer(li),intent(in) :: Nmax
        sc%Nmax = Nmax
        sc%Nmaxr = real(Nmax,cp)
       end subroutine

       end module