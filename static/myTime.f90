       module myTime_mod
       ! Fixes/improvements:
       ! - include output time to file (similar to myError)
       ! - make set/get functions to make components accessable to myEfficiency

       ! - include parallel clock?
       ! - efficiency = runtime/step (optional)
       ! - efficiency = runtime*L2norm (think error vs run time ~ 1/t, we want closest to origin)


       use IO_tools_mod
       use simParams_mod
       use solverSettings_mod
       implicit none

       private

       public :: myTime
       public :: init,print

       public :: startTime,stopTime
       public :: getTotRunTime
       public :: getRunTime
       public :: estimateRemaining

       public :: printTimeWithUnits
       public :: writeTime

       public :: computationInProgress
       public :: computationComplete

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif


       type myTime
        private
        ! Known Quantities
        real(cp) :: t_start_sim
        real(cp) :: t_finish_sim
        real(cp) :: runTime_sim
        real(cp) :: t_start
        real(cp) :: t_finish
        real(cp) :: runTime
        real(cp) :: runTimeCumulative
        integer :: N
        integer :: i_start
        integer :: i_finish
        integer :: i_start_sim
        integer :: i_finish_sim
        integer :: countRate
        real(cp) :: runTimeAve
        integer :: iterPerSec
        integer :: iterPerHour
        integer :: iterPerDay
        ! Estimated Quantities
        integer :: NMax
        integer :: NRemaining
        real(cp) :: estimatedRemaining
        real(cp) :: estimatedTotal
        real(cp) :: percentageComplete
       end type

       interface init;              module procedure initTime;               end interface
       interface stopTime;          module procedure stopTimeSS;             end interface
       interface stopTime;          module procedure stopTimeNoSS;           end interface
       interface estimateRemaining; module procedure estimateRemainingNoSS;  end interface
       interface estimateRemaining; module procedure estimateRemainingSS;    end interface
       interface print;             module procedure printTime;              end interface

       contains

      subroutine initTime(this)
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
        this%iterPerHour = 0
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
        this%t_start = real(this%i_start,cp)
      end subroutine

      subroutine stopTimeNoSS(this)
        implicit none
        type(myTime),intent(inout) :: this
        call system_clock(this%i_finish,this%countRate)
        this%t_finish = real(this%i_finish,cp)
        this%N = this%N + 1
        this%runTime = (this%t_finish - this%t_start)/real(this%countRate,cp)
        this%runTimeCumulative = this%runTimeCumulative + this%runTime
        this%runTimeAve = (this%t_finish - this%t_start)/real(this%countRate,cp)
      end subroutine

      subroutine stopTimeSS(this,ss)
        implicit none
        type(myTime),intent(inout) :: this
        type(solverSettings),intent(in) :: ss
        call system_clock(this%i_finish,this%countRate)
        this%t_finish = real(this%i_finish,cp)
        ! this%N = this%N + 1
        this%N = getIteration(ss)
        this%runTime = (this%t_finish - this%t_start)/real(this%countRate,cp)
        this%runTimeCumulative = this%runTimeCumulative + this%runTime
        
        ! This is not technically correct, but instead of writing this data
        ! to a file and recording it again, just take the average as the 
        ! local time to finish one MHD loop:
        if (restartU.and.restartB) then
          this%runTimeAve = (this%t_finish - this%t_start)/real(this%countRate,cp)
        else
          this%runTimeAve = this%runTimeCumulative/real(this%N,cp)
        endif
        this%iterPerSec = floor(1.0/this%runTimeAve)
        this%iterPerHour = floor(1.0/this%runTimeAve*3600.0)
        this%iterPerDay = floor(1.0/this%runTimeAve*3600.0*24.0)
      end subroutine

      subroutine estimateRemainingNoSS(this,Nmax)
        implicit none
        type(myTime),intent(inout) :: this
        integer,intent(in) :: Nmax
        this%NMax = Nmax
        this%estimatedRemaining = LIPx(real(0.0,cp),real(this%N-1,cp),&
        this%runTimeAve,real(this%N,cp),real(this%NMax,cp))
        this%estimatedTotal = this%runTimeAve*this%NMax
        this%percentageComplete = real(this%N,cp)/real(this%NMax,cp)*100.0
        this%NRemaining = this%NMax - this%N
      end subroutine

      subroutine estimateRemainingSS(this,ss)
        implicit none
        type(myTime),intent(inout) :: this
        type(solverSettings),intent(in) :: ss
        if (getMaxIterationsTF(ss)) then
          this%NMax = getMaxIterations(ss)
          this%estimatedRemaining = LIPx(real(0.0,cp),real(this%N-1,cp),&
          this%runTimeAve,real(this%N,cp),real(this%NMax,cp))
          this%estimatedTotal = this%runTimeAve*this%NMax
          this%percentageComplete = real(this%N,cp)/real(this%NMax,cp)*100.0
        elseif (getMaxSimulationTimeTF(ss)) then
          ! Not yet tested
          this%estimatedRemaining = LIPx(real(0.0,cp),real(getSimulationTime(ss),cp),&
          this%runTimeAve,real(this%N,cp),real(getMaxSimulationTime(ss),cp))
        elseif (getMinToleranceTF(ss)) then
          ! Not yet tested
          this%estimatedRemaining = LIPx(real(0.0,cp),real(getTolerance(ss),cp),&
          this%runTimeAve,real(this%N,cp),real(getMinTolerance(ss),cp))
        elseif (getMaxCPUTimeTF(ss)) then
          ! Not yet tested
          this%estimatedRemaining = LIPx(real(0.0,cp),real(getCPUTime(ss),cp),&
          this%runTimeAve,real(this%N,cp),real(getMaxCPUTime(ss),cp))
        endif
        this%NRemaining = this%NMax - this%N
        if (.not.getMaxIterationsTF(ss)) then
          this%NMax = ceiling(this%N/this%percentageComplete*100.0)
        endif
      end subroutine

      function LIPx(x1,y1,x2,y2,y3) result(x3)
        implicit none
        real(cp),intent(in) :: x1,y1,x2,y2,y3
        real(cp) :: x3
        x3 = x1 + (y3-y1)/(y2-y1)*(x2-x1)
      end function

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
        real(cp) :: runTime
        runTime = this%runTime
      end function

      function getRunTime(this) result(runTimeCumulative)
        implicit none
        type(myTime),intent(in) :: this
        real(cp) :: runTimeCumulative
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
        real(cp) :: temp
        character :: u
        write(newU,*) ''
        write(newU,*) '********* WALL CLOCK TIME INFO FOR '// trim(adjustl(name)) //' *******'
        write(newU,*) '------------------- KNOWN QUANTITIES ---------------------'
        write(newU,*) ''

        write(newU,*) 'Iterations (complete) = ',this%N

        temp = this%runTimeAve; call getTimeWithUnits(temp,u)
        write(newU,*) 'Time (average/iteration) = ',temp,' (', u,')'

        write(newU,*) 'Iterations per (s,h,d) = ',this%iterPerSec,this%iterPerHour,this%iterPerDay

        temp = this%runTimeCumulative; call getTimeWithUnits(temp,u)
        write(newU,*) 'Time (Total passed) = ',temp,' (', u,')'

        write(newU,*) ''
        write(newU,*) '---------------- ESTIMATED QUANTITIES ---------------------'

        write(newU,*) 'Iterations (remaining/max) = ',this%NRemaining,this%NMax

        temp = this%estimatedTotal; call getTimeWithUnits(temp,u)
        write(newU,*) 'Time (total) = ',temp,' (', u,')'

        temp = this%estimatedRemaining; call getTimeWithUnits(temp,u)
        write(newU,*) 'Time (remaining) = ',temp,' (', u,')'

        write(newU,*) 'Percentage complete = ',this%percentageComplete

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
        this%t_start_sim = real(this%i_start_sim,cp)
      end subroutine

      subroutine computationComplete(this,TF)
        implicit none
        type(myTime),intent(inout) :: this
        logical,intent(in),optional :: TF
        character :: u
        real(cp) :: temp
        call system_clock(this%i_finish_sim,this%countRate)
        this%t_finish_sim = this%i_finish_sim
        this%runTime_sim = (this%t_finish_sim - this%t_start_sim)/real(this%countRate,cp)
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
        real(cp),intent(in) :: t
        character(len=*),intent(in) :: name
        real(cp) :: temp
        character :: u
        temp = t
        call getTimeWithUnits(temp,u)
        write(*,*) adjustl(trim(name))//' = ',temp,' (', u,')'
       end subroutine

      subroutine getTimeWithUnits(t,u)
        implicit none
        real(cp),intent(inout) :: t
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

