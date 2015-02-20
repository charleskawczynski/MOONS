      module solverSettings_mod

      use constants_mod
      use simParams_mod
      use myExceptions_mod

      implicit none

      private

      public :: solverSettings
      public :: init
      public :: setName,getName

      public :: setIteration ! (Current value)
      public :: getIteration ! (Current value)
      public :: setMaxIterations
      public :: getMaxIterations
      public :: getMaxIterationsTF

      public :: setSimulationTime ! (Current value)
      public :: getSimulationTime ! (Current value)
      public :: setMaxSimulationTime
      public :: getMaxSimulationTime
      public :: getMaxSimulationTimeTF

      public :: setCPUTime ! (Current value)
      public :: getCPUTime ! (Current value)
      public :: setMaxCPUTime
      public :: getMaxCPUTime
      public :: getMaxCPUTimeTF

      public :: setTolerance ! (Current value)
      public :: getTolerance ! (Current value)
      public :: setMinTolerance
      public :: getMinTolerance
      public :: getMinToleranceTF

      public :: setSubtractMean
      public :: getSubtractMean
      public :: setMixedConditions

      public :: checkCondition
      public :: getExportRawSolution
      public :: getExportTransient
      public :: getExportSolution
      public :: getExportErrors
      public :: getPrintParams
      public :: solverSettingsSet

      integer,parameter :: nameLength = 20

      type solverSettings
        private

        integer :: iteration
        integer :: maxIterations
        logical :: maxIterationsTF

        real(dpn) :: simulationTime
        real(dpn) :: maxSimulationTime
        logical :: maxSimulationTimeTF

        real(dpn) :: CPUTime
        real(dpn) :: maxCPUTime
        logical :: maxCPUTimeTF

        real(dpn) :: tolerance
        real(dpn) :: minTolerance
        logical :: minToleranceTF

        real(dpn) :: current
        real(dpn) :: targetVal

        logical :: minVal
        logical :: isset = .false.
        logical :: displayOutput
        logical :: subtractMean = .false.
        logical :: mixedConditions = .false.
        character(len=nameLength) :: name
      end type

      interface init
        module procedure initializeSolverSettings
      end interface

      contains

      subroutine initializeSolverSettings(this)
        implicit none
        type(solverSettings),intent(inout) :: this
        call setAllFalse(this)
        this%iteration = 0
        this%maxIterations = 0
        this%simulationTime = 0.0
        this%maxSimulationTime = 0.0
        this%CPUTime = 0.0
        this%maxCPUTime = 0.0
        this%tolerance = 0.0
        this%minTolerance = 0.0
        this%targetVal = 0.0
        this%current = 0.0
      end subroutine

      subroutine setName(this,name)
        implicit none
        type(solverSettings),intent(inout) :: this
        character(len=nameLength),intent(in) :: name
        this%name = name
      end subroutine

      function getName(this) result(name)
        implicit none
        type(solverSettings),intent(in) :: this
        character(len=nameLength) :: name
        name = this%name
      end function

!-----------------------------------------------------------

      subroutine setIteration(this,iteration)
        implicit none
        type(solverSettings),intent(inout) :: this
        integer,intent(in) :: iteration
        this%iteration = iteration
        if (this%maxIterationsTF) then
          this%current = dble(iteration)
        endif
      end subroutine

      function getIteration(this) result(iteration)
        implicit none
        type(solverSettings),intent(in) :: this
        integer :: iteration
        iteration = this%iteration
      end function

      function getMaxIterations(this) result(maxIterations)
        implicit none
        type(solverSettings),intent(in) :: this
        integer :: maxIterations
        maxIterations = this%maxIterations
      end function

      subroutine setMaxIterations(this,maxIterations)
        implicit none
        type(solverSettings),intent(inout) :: this
        integer,intent(in) :: maxIterations
        this%maxIterations = maxIterations
        this%targetVal = dble(maxIterations)
        this%minVal = .false.
        this%isset = .true.
        this%maxIterationsTF = .true.
      end subroutine

      function getMaxIterationsTF(this) result(maxIterationsTF)
        implicit none
        type(solverSettings),intent(in) :: this
        logical :: maxIterationsTF
        maxIterationsTF = this%maxIterationsTF
      end function

!-----------------------------------------------------------

      subroutine setSimulationTime(this,simulationTime)
        implicit none
        type(solverSettings),intent(inout) :: this
        real(dpn),intent(in) :: simulationTime
        this%simulationTime = simulationTime
        if (this%maxSimulationTimeTF) then
          this%current = dble(simulationTime)
        endif
      end subroutine

      function getSimulationTime(this) result(simulationTime)
        implicit none
        type(solverSettings),intent(in) :: this
        real(dpn) :: simulationTime
        simulationTime = this%simulationTime
      end function

      function getMaxSimulationTime(this) result(maxSimulationTime)
        implicit none
        type(solverSettings),intent(in) :: this
        real(dpn) :: maxSimulationTime
        maxSimulationTime = this%maxSimulationTime
      end function

      subroutine setMaxSimulationTime(this,maxSimulationTime)
        implicit none
        type(solverSettings),intent(inout) :: this
        real(dpn),intent(in) :: maxSimulationTime
        this%maxSimulationTime = maxSimulationTime
        this%targetVal = dble(maxSimulationTime)
        this%maxSimulationTimeTF = .true.
        this%minVal = .false.
        this%isset = .true.
      end subroutine

      function getMaxSimulationTimeTF(this) result(maxSimulationTimeTF)
        implicit none
        type(solverSettings),intent(in) :: this
        logical :: maxSimulationTimeTF
        maxSimulationTimeTF = this%maxSimulationTimeTF
      end function

!-----------------------------------------------------------

      subroutine setCPUTime(this,CPUTime)
        implicit none
        type(solverSettings),intent(inout) :: this
        real(dpn),intent(in) :: CPUTime
        this%CPUTime = CPUTime
        if (this%maxCPUTimeTF) then
          this%current = dble(CPUTime)
        endif
      end subroutine

      function getCPUTime(this) result(CPUTime)
        implicit none
        type(solverSettings),intent(in) :: this
        real(dpn) :: CPUTime
        CPUTime = this%CPUTime
      end function

      function getMaxCPUTime(this) result(maxCPUTime)
        implicit none
        type(solverSettings),intent(in) :: this
        real(dpn) :: maxCPUTime
        maxCPUTime = this%maxCPUTime
      end function

      subroutine setMaxCPUTime(this,maxCPUTime)
        implicit none
        type(solverSettings),intent(inout) :: this
        real(dpn),intent(in) :: maxCPUTime
        this%maxCPUTime = maxCPUTime
        this%targetVal = dble(maxCPUTime)
        this%maxCPUTimeTF = .true.
        this%minVal = .false.
        this%isset = .true.
      end subroutine

      function getMaxCPUTimeTF(this) result(maxCPUTimeTF)
        implicit none
        type(solverSettings),intent(in) :: this
        logical :: maxCPUTimeTF
        maxCPUTimeTF = this%maxCPUTimeTF
      end function

!-----------------------------------------------------------

      subroutine setTolerance(this,tolerance)
        implicit none
        type(solverSettings),intent(inout) :: this
        real(dpn),intent(in) :: tolerance
        this%tolerance = tolerance
        if (this%minToleranceTF) then
          this%current = dble(tolerance)
        endif
      end subroutine

      function getTolerance(this) result(tolerance)
        implicit none
        type(solverSettings),intent(in) :: this
        real(dpn) :: tolerance
        tolerance = this%tolerance
      end function

      function getMinTolerance(this) result(minTolerance)
        implicit none
        type(solverSettings),intent(in) :: this
        real(dpn) :: minTolerance
        minTolerance = this%minTolerance
      end function

      subroutine setMinTolerance(this,minTolerance)
        implicit none
        type(solverSettings),intent(inout) :: this
        real(dpn),intent(in) :: minTolerance
        this%minTolerance = minTolerance
        this%targetVal = dble(minTolerance)
        this%minToleranceTF = .true.
        this%minVal = .true.
        this%isset = .true.
      end subroutine

      function getMinToleranceTF(this) result(minToleranceTF)
        implicit none
        type(solverSettings),intent(in) :: this
        logical :: minToleranceTF
        minToleranceTF = this%minToleranceTF
      end function

!-----------------------------------------------------------

      subroutine setSubtractMean(this)
        implicit none
        type(solverSettings),intent(inout) :: this
        this%subtractMean = .true.
      end subroutine

      function getSubtractMean(this) result(subtractMean)
        implicit none
        type(solverSettings),intent(in) :: this
        logical :: subtractMean
        subtractMean = this%subtractMean
      end function

!-----------------------------------------------------------

      subroutine setMixedConditions(this)
        implicit none
        type(solverSettings),intent(inout) :: this
        this%mixedConditions = .true.
      end subroutine

!-----------------------------------------------------------

      subroutine checkCondition(this,continueLoop)
        implicit none
        type(solverSettings),intent(in) :: this
        logical,intent(out) :: continueLoop

        if (this%minVal) then
          continueLoop = .not.(this%current.le.this%targetVal)
        else
          continueLoop = .not.(this%current.ge.this%targetVal)
        endif
        if (this%mixedConditions) then
          ! Mixed conditions:
          if (this%minToleranceTF.and.this%maxIterationsTF) then
              continueLoop = .not.((this%iteration.ge.this%maxIterations).or.&
                                   (this%tolerance.le.this%minTolerance))
          endif
        endif
      end subroutine

      subroutine solverSettingsSet(this)
        implicit none
        type(solverSettings),intent(in) :: this
        if (.not.this%isset) then
          write(*,*) 'No solver settings were provided. Terminating execution.'
          stop
        endif
      end subroutine

      function getExportRawSolution(this) result(TF)
        implicit none
        type(solverSettings),intent(in) :: this
        logical :: TF
        TF = (mod(this%iteration,nskip_exportRaw).eq.1).and.(this%iteration.ne.1)
      end function

      function getExportSolution(this) result(TF)
        implicit none
        type(solverSettings),intent(in) :: this
        logical :: TF
        TF = (mod(this%iteration,nskip_export).eq.1).and.(this%iteration.ne.1)
      end function

      function getExportTransient(this) result(TF)
        implicit none
        type(solverSettings),intent(in) :: this
        logical :: TF
        TF = (mod(this%iteration,nskip_exportTransient).eq.1)
      end function

      function getExportErrors(this) result(TF)
        implicit none
        type(solverSettings),intent(in) :: this
        logical :: TF
        TF = (mod(this%iteration,nskip_exportErrors).eq.1)
      end function

      function getPrintParams(this) result(TF)
        implicit none
        type(solverSettings),intent(in) :: this
        logical :: TF
        TF = (mod(this%iteration,nskip_print).eq.1)
      end function

      subroutine setAllFalse(this)
        implicit none
        type(solverSettings),intent(inout) :: this
        this%maxIterationsTF = .false.
        this%minToleranceTF = .false.
        this%maxSimulationTimeTF = .false.
        this%maxCPUTimeTF = .false.
        this%isset = .false.
        this%displayOutput = .false.
        this%subtractMean = .false.
      end subroutine

      end module