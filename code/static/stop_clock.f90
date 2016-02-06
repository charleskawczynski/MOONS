       module stop_clock_mod
       ! Fixes/improvements:
       ! - include output time to file (similar to myError)
       ! - make set/get functions to make components accessable to myEfficiency

       ! - include parallel clock?
       ! - efficiency = runtime/step (optional)
       ! - efficiency = runtime*L2norm (think error vs run time ~ 1/t, we want closest to origin)

       use IO_tools_mod
       use clock_mod
       implicit none

       private

       public :: stop_clock
       public :: init,delete
       public :: tic,toc
       public :: print,export

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif


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
        real(cp) :: percentageComplete

        integer :: iterPerSec
        integer :: iterPerMin
        integer :: iterPerHour
        integer :: iterPerDay
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
        sc%t_passed = sc%t_passed + sc%c%t_elapsed
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

        NewU = newAndOpen(dir,'WALL_CLOCK_TIME_INFO')
        call export_sc(sc,newU)
        close(NewU)
        call closeAndMessage(newU,'WALL_CLOCK_TIME_INFO',dir)
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
        logical :: problem
        problem = .false.
        write(un,*) ''
        write(un,*) '******************* KNOWN WALL CLOCK TIME INFO *********************'

        write(un,*) 'Iterations (complete) = ',sc%N

        temp = sc%secPerIter; call getTimeWithUnits(temp,u)
        write(un,*) 'Time (seconds/iteration) = ',temp,' (', u,')'
        call debug_stop_clock(sc,temp,problem)

        write(un,*) 'Iterations per (s,m,h,d) = ',sc%iterPerSec,sc%iterPerMin,sc%iterPerHour,sc%iterPerDay

        temp = sc%t_passed; call getTimeWithUnits(temp,u)
        write(un,*) 'Time (Total passed) = ',temp,' (', u,')'
        call debug_stop_clock(sc,temp,problem)

        write(un,*) ''
        write(un,*) '***************** ESTIMATED WALL CLOCK TIME INFO *******************'

        write(un,*) 'Iterations (remaining/max) = ',sc%NRemaining,sc%NMax

        temp = sc%estimatedTotal; call getTimeWithUnits(temp,u)
        write(un,*) 'Time (total) = ',temp,' (', u,')'
        call debug_stop_clock(sc,temp,problem)

        temp = sc%estimatedRemaining; call getTimeWithUnits(temp,u)
        write(un,*) 'Time (remaining) = ',temp,' (', u,')'
        call debug_stop_clock(sc,temp,problem)

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

      subroutine debug_stop_clock(sc,t,problem)
        implicit none
        type(stop_clock),intent(in) :: sc
        real(cp),intent(in) :: t
        logical,intent(inout) :: problem
        if (.not.problem) then ! check to see if there is a problem
          if (t.lt.0.0_cp) then
            write(*,*) 'WARNING: negative time estimate in stop_clock.f90'
            write(*,*) 'debug info:'
            write(*,*) 't_elapsed = ',sc%c%t_elapsed
            write(*,*) 't_passed = ',sc%t_passed
            write(*,*) 'N = ',sc%N
            write(*,*) 'Nr = ',sc%Nr
            write(*,*) 'secPerIter = ',sc%secPerIter
            write(*,*) 'estimatedTotal = ',sc%estimatedTotal
            write(*,*) 'estimatedRemaining = ',sc%estimatedRemaining
            write(*,*) 'percentageComplete = ',sc%percentageComplete
            write(*,*) 'NRemaining = ',sc%NRemaining
            write(*,*) 'iterPerSec = ',sc%iterPerSec
            problem = .true.
          endif
        endif
       end subroutine

       end module

