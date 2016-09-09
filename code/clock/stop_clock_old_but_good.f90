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
       implicit none

       private
       public :: stop_clock
       public :: init,delete
       public :: tic,toc
       public :: print,export

       type stop_clock
        ! Known Quantities
        type(clock) :: c
        real(cp) :: secPerIter
        real(cp) :: t_passed
        integer :: N,Nmax,NRemaining
        real(cp) :: Nr,Nmaxr
        ! Estimated Quantities
        real(cp) :: estimatedTotal
        real(cp) :: estimatedRemaining
        real(cp) :: percentageComplete,t_elapsed

        integer :: iterPerSec
        integer :: iterPerMin
        integer :: iterPerHour
        integer :: iterPerDay
        logical :: frozen_elapsed ! For when elapsed is returned negative
       end type

       interface init;   module procedure init_sc;       end interface
       interface delete; module procedure delete_sc;     end interface
       interface tic;    module procedure tic_sc;        end interface
       interface toc;    module procedure toc_sc;        end interface
       interface print;  module procedure print_sc;      end interface
       interface export; module procedure export_sc;     end interface
       interface export; module procedure export_sc_dir; end interface

       contains

      subroutine init_sc(sc,Nmax)
        implicit none
        type(stop_clock),intent(inout) :: sc
        integer,intent(in) :: Nmax
        call init(sc%c)
        sc%secPerIter = 0.0_cp
        sc%t_passed = 0.0_cp
        sc%N = 0
        sc%NMax = Nmax
        sc%NRemaining = 0
        sc%Nr = real(sc%N,cp)
        sc%NMaxr = real(sc%Nmax,cp)
        sc%frozen_elapsed = .false.

        sc%estimatedTotal = 0.0_cp
        sc%estimatedRemaining = 0.0_cp
        sc%percentageComplete = 0.0_cp

        sc%iterPerSec = 0
        sc%iterPerMin = 0
        sc%iterPerHour = 0
        sc%iterPerDay = 0
      end subroutine

      subroutine delete_sc(sc)
        implicit none
        type(stop_clock),intent(inout) :: sc
        call init(sc%c)
        sc%secPerIter = 0.0_cp
        sc%t_passed = 0.0_cp
        sc%N = 0
        sc%NMax = 0
        sc%NRemaining = 0
        sc%Nr = real(sc%N,cp)
        sc%NMaxr = real(sc%Nmax,cp)
        sc%frozen_elapsed = .false.

        sc%estimatedTotal = 0.0_cp
        sc%estimatedRemaining = 0.0_cp
        sc%percentageComplete = 0.0_cp

        sc%iterPerSec = 0
        sc%iterPerMin = 0
        sc%iterPerHour = 0
        sc%iterPerDay = 0
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
        sc%secPerIter = sc%t_passed/sc%Nr
        sc%estimatedTotal = sc%secPerIter*sc%NMaxr
        sc%estimatedRemaining = sc%estimatedTotal - sc%t_passed
        sc%percentageComplete = sc%Nr/sc%NMaxr*100.0_cp
        sc%NRemaining = sc%NMax - sc%N + 1
        sc%iterPerSec = floor(1.0_cp/sc%secPerIter)
        sc%iterPerMin = floor(60.0_cp/sc%secPerIter)
        sc%iterPerHour = floor(3600.0_cp/sc%secPerIter)
        sc%iterPerDay = floor(86400.0_cp/sc%secPerIter) ! 3600*24 = 86400
      end subroutine

      subroutine export_sc_dir(sc,dir)
        implicit none
        type(stop_clock),intent(in) :: sc
        character(len=*),intent(in) :: dir
        integer :: NewU

        NewU = new_and_open(dir,'WALL_CLOCK_TIME_INFO')
        call export_sc(sc,newU)
        call close_and_message(newU,dir,'WALL_CLOCK_TIME_INFO')
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

        temp = sc%secPerIter; call getTimeWithUnits(temp,u)
        write(un,*) 'Time (seconds/iteration) = ',temp,' (', u,')'

        write(un,*) 'Iterations per (s,m,h,d) = ',sc%iterPerSec,sc%iterPerMin,sc%iterPerHour,sc%iterPerDay

        temp = sc%t_passed; call getTimeWithUnits(temp,u)
        write(un,*) 'Time (Total passed) = ',temp,' (', u,')'

        write(un,*) ''
        write(un,*) '***************** ESTIMATED WALL CLOCK TIME INFO *******************'

        write(un,*) 'Iterations (remaining/max) = ',sc%NRemaining,sc%NMax

        temp = sc%estimatedTotal; call getTimeWithUnits(temp,u)
        write(un,*) 'Time (total) = ',temp,' (', u,')'

        temp = sc%estimatedRemaining; call getTimeWithUnits(temp,u)
        write(un,*) 'Time (remaining) = ',temp,' (', u,')'

        write(un,*) 'Percentage complete = ',sc%percentageComplete

        write(un,*) ''
        write(un,*) '********************************************************************'
      end subroutine

      subroutine getTimeWithUnits(t,u)
        implicit none
        real(cp),intent(inout) :: t
        character(len=1),intent(inout) :: u
        if (t.lt.60.0_cp) then
         u = 's'; t = t
        elseif ((t.ge.60.0_cp).and.(t.lt.3600.0_cp)) then
         u = 'm'; t = t/60.0_cp
        elseif ((t.ge.3600.0_cp).and.(t.lt.86400.0_cp)) then
         u = 'h'; t = t/3600.0_cp ! 60*60 = 3600
        elseif ((t.ge.86400.0_cp).and.(t.lt.31557600.0_cp)) then
         u = 'd'; t = t/86400.0_cp ! 60*60*24 = 86400
        elseif (t.ge.31557600.0_cp) then
         u = 'y'; t = t/31557600.0_cp ! 60*60*24*365.24 = 31557600
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