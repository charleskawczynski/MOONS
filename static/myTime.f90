       module myTime_mod
       ! Fixes/improvements:
       ! - include output time to file (similar to myError)
       ! - make set/get functions to make components accessable to myEfficiency

       ! - include parallel clock?
       ! - efficiency = runtime/step (optional)
       ! - efficiency = runtime*L2norm (think error vs run time ~ 1/t, we want closest to origin)


       use constants_mod
       use myIO_mod
       use simParams_mod
       use solverSettings_mod
       implicit none

       private

       public :: myTime
       public :: initialize

       public :: startTime,stopTime
       public :: getTotRunTime
       public :: getRunTime
       public :: estimateRemaining

       public :: printTime
       public :: printTimeWithUnits
       public :: writeTime

       public :: computationInProgress
       public :: computationComplete

       type myTime
        private
        ! Known Quantities
        real(dpn) :: t_start_sim
        real(dpn) :: t_finish_sim
        real(dpn) :: runTime_sim
        real(dpn) :: t_start
        real(dpn) :: t_finish
        real(dpn) :: runTime
        real(dpn) :: runTimeCumulative
        integer :: N
        integer :: i_start
        integer :: i_finish
        integer :: i_start_sim
        integer :: i_finish_sim
        integer :: countRate
        real(dpn) :: runTimeAve
        integer :: iterPerSec
        integer :: iterPerDay
        ! Estimated Quantities
        integer :: NMax
        integer :: NRemaining
        real(dpn) :: estimatedRemaining
        real(dpn) :: estimatedTotal
        real(dpn) :: percentageComplete
       end type

       interface initialize
         module procedure initializeTime
       end interface

       interface stopTime
         module procedure stopTimeSS
         module procedure stopTimeNoSS
       end interface

       contains

      subroutine initializeTime(this)
        implicit none
        type(myTime),intent(inout) :: this
        this%t_start = 0.0
        this%t_finish = 0.0
        this%runTime = 0.0
        this%runTimeCumulative = 0.0
        this%runTimeAve = 0.0
        this%percentageComplete = 0.0
        this%estimatedRemaining = 0.0
        this%estimatedTotal = 0.0
        this%iterPerSec = 0
        this%iterPerDay = 0
        this%N = 0
        this%NRemaining = 0
        this%NMax = 0
        this%countRate = 10
        this%i_start = 0
        this%i_finish = 0
        this%i_start_sim = 0
        this%i_finish_sim = 0
      end subroutine

      subroutine startTime(this)
        implicit none
        type(myTime),intent(inout) :: this
        call system_clock(this%i_start,this%countRate)
        this%t_start = dble(this%i_start)
      end subroutine

      subroutine stopTimeSS(this,ss)
        implicit none
        type(myTime),intent(inout) :: this
        type(solverSettings),intent(in) :: ss
        call system_clock(this%i_finish,this%countRate)
        this%t_finish = dble(this%i_finish)
        ! this%N = this%N + 1
        this%N = getIteration(ss)
        this%runTime = (this%t_finish - this%t_start)/dble(this%countRate)
        this%runTimeCumulative = this%runTimeCumulative + this%runTime
        
        ! This is not technically correct, but instead of writing this data
        ! to a file and recording it again, just take the average as the 
        ! local time to finish one MHD loop:
        if (restartU.and.restartB) then
          this%runTimeAve = (this%t_finish - this%t_start)/dble(this%countRate)
        else
          this%runTimeAve = this%runTimeCumulative/dble(this%N)
        endif
        this%iterPerSec = floor(1.0/this%runTimeAve)
        this%iterPerDay = floor(1.0/this%runTimeAve*3600.0*24.0)
      end subroutine

      subroutine stopTimeNoSS(this)
        implicit none
        type(myTime),intent(inout) :: this
        call system_clock(this%i_finish,this%countRate)
        this%t_finish = dble(this%i_finish)
        ! this%N = this%N + 1
        this%runTime = (this%t_finish - this%t_start)/dble(this%countRate)
        this%runTimeCumulative = this%runTimeCumulative + this%runTime
        this%runTimeAve = (this%t_finish - this%t_start)/dble(this%countRate)
      end subroutine

      subroutine estimateRemaining(this,ss)
        implicit none
        type(myTime),intent(inout) :: this
        type(solverSettings),intent(in) :: ss
        if (getMaxIterationsTF(ss)) then
          this%NMax = getMaxIterations(ss)
          this%estimatedRemaining = LIPx(dble(0.0),dble(this%N-1),&
          this%runTimeAve,dble(this%N),dble(getMaxIterations(ss)))
          this%estimatedTotal = this%runTimeAve*this%NMax
          this%percentageComplete = dble(this%N)/dble(this%NMax)*100.0
        elseif (getMaxSimulationTimeTF(ss)) then
          ! Not yet tested
          this%estimatedRemaining = LIPx(dble(0.0),dble(getSimulationTime(ss)),&
          this%runTimeAve,dble(this%N),dble(getMaxSimulationTime(ss)))
        elseif (getMinToleranceTF(ss)) then
          ! Not yet tested
          this%estimatedRemaining = LIPx(dble(0.0),dble(getTolerance(ss)),&
          this%runTimeAve,dble(this%N),dble(getMinTolerance(ss)))
        elseif (getMaxCPUTimeTF(ss)) then
          ! Not yet tested
          this%estimatedRemaining = LIPx(dble(0.0),dble(getCPUTime(ss)),&
          this%runTimeAve,dble(this%N),dble(getMaxCPUTime(ss)))
        endif
        this%NRemaining = this%NMax - this%N
        if (.not.getMaxIterationsTF(ss)) then
          this%NMax = ceiling(this%N/this%percentageComplete*100.0)
        endif
      end subroutine

      function LIPx(x1,y1,x2,y2,y3) result(x3)
        implicit none
        real(dpn),intent(in) :: x1,y1,x2,y2,y3
        real(dpn) :: x3
        x3 = x1 + (y3-y1)/(y2-y1)*(x2-x1)
      end function

      ! function LIPy(x1,y1,x2,y2,x3) result(y3)
      !   implicit none
      !   real(dpn),intent(in) :: x1,y1,x2,y2,x3
      !   real(dpn) :: y3
      !   y3 = y1 + (x3-x1)/(x2-x1)*(y2-y1)
      ! end function

      subroutine writeTime(this,dir,name)
        implicit none
        type(myTime),intent(in) :: this
        character(len=*),intent(in) :: dir
        character(len=*),intent(in) :: name
        integer :: NewU

        NewU = newAndOpen(dir,'WALL_CLOCK_TIME_INFO')
        call writeTimeToFileOrScreen(this,newU,name)
        close(NewU)
        call closeAndMessage(newU,'WALL_CLOCK_TIME_INFO',dir)
      end subroutine

      function getTotRunTime(this) result(runTime)
        implicit none
        type(myTime),intent(in) :: this
        real(dpn) :: runTime
        runTime = this%runTime
      end function

      function getRunTime(this) result(runTimeCumulative)
        implicit none
        type(myTime),intent(in) :: this
        real(dpn) :: runTimeCumulative
        runTimeCumulative = this%runTimeCumulative
      end function

      subroutine printTime(this,name)
        implicit none
        type(myTime),intent(in) :: this
        character(len=*),intent(in) :: name
        call writeTimeToFileOrScreen(this,6,name)
      end subroutine

      subroutine writeTimeToFileOrScreen(this,newU,name)
        implicit none
        type(myTime),intent(in) :: this
        integer,intent(in) :: NewU
        character(len=*),intent(in) :: name
        real(dpn) :: temp
        character :: u
        write(newU,*) ''
        write(newU,*) '********* WALL CLOCK TIME INFO FOR '// trim(adjustl(name)) //' *******'
        write(newU,*) '------------------- KNOWN QUANTITIES ---------------------'
        write(newU,*) ''

        write(newU,*) 'Number of iterations complete = ',this%N

        temp = this%runTimeAve; call getTimeWithUnits(temp,u)
        write(newU,*) 'Average wall clock time per iteration = ',temp,' (', u,')'

        write(newU,*) 'Iterations per second = ',this%iterPerSec
        write(newU,*) 'Iterations per day = ',this%iterPerDay

        temp = this%runTimeCumulative; call getTimeWithUnits(temp,u)
        write(newU,*) 'Total wall clock time passed = ',temp,' (', u,')'

        write(newU,*) ''
        write(newU,*) '---------------- ESTIMATED QUANTITIES ---------------------'

        write(newU,*) 'Maximum number of iterations = ',this%NMax

        write(newU,*) 'Number of iterations remaining = ',this%NRemaining

        temp = this%estimatedTotal; call getTimeWithUnits(temp,u)
        write(newU,*) 'Total wall clock time = ',temp,' (', u,')'

        temp = this%estimatedRemaining; call getTimeWithUnits(temp,u)
        write(newU,*) 'Estimated wall clock time remaining = ',temp,' (', u,')'

        write(newU,*) 'Percentage of process complete = ',this%percentageComplete

        write(newU,*) ''
        write(newU,*) '***********************************************************'
      end subroutine

      subroutine computationInProgress(this,TF)
        implicit none
        type(myTime),intent(inout) :: this
        logical,intent(in),optional :: TF
        if (.not.present(TF)) then
        write(*,*) '******************* COMPUTATIONS IN PROGRESS ', &
                   '*******************'
        endif
        call system_clock(this%i_start_sim,this%countRate)
        this%t_start_sim = dble(this%i_start_sim)
      end subroutine

      subroutine computationComplete(this,TF)
        implicit none
        type(myTime),intent(inout) :: this
        logical,intent(in),optional :: TF
        character :: u
        real(dpn) :: temp
        call system_clock(this%i_finish_sim,this%countRate)
        this%t_finish_sim = this%i_finish_sim
        this%runTime_sim = (this%t_finish_sim - this%t_start_sim)/dble(this%countRate)
        temp = this%runTime_sim
        call getTimeWithUnits(temp,u)
        if (.not.present(TF)) then
          write(*,*) '******************* COMPUTATIONS COMPLETE ', &
                     '*******************'
          write(*,*) ''
          write(*,*) 'Total Simulation wall clock time:',temp,' (', u,')'
        endif
      end subroutine

      subroutine printTimeWithUnits(t,name)
        implicit none
        real(dpn),intent(in) :: t
        character(len=*),intent(in) :: name
        real(dpn) :: temp
        character :: u
        temp = t
        call getTimeWithUnits(temp,u)
        write(*,*) adjustl(trim(name))//' = ',temp,' (', u,')'
       end subroutine

      subroutine getTimeWithUnits(t,u)
        implicit none
        real(dpn),intent(inout) :: t
        character,intent(out) :: u
         if ((t.ge.60.0).and.(t.lt.3600.0)) then
          t = t/60.0; u = 'm'
         elseif ((t.ge.3600.0).and.(t.lt.3600.0*24.0)) then
          t = t/3600.0; u = 'h'
         elseif (t.ge.3600.0*24.0) then
          t = t/(3600.0*24.0); u = 'd'
         else; u = 's'
         endif
       end subroutine

       end module

