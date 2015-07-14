      module PSE_mod
      ! call PseudoTimeSolver(u,f,u_bcs,g,ss,gridType,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), grid (g)
      ! and solver settings (ss) using a Pseudo Time Stepping Method
      ! where
      !    u_t = u_xx + u_yy + u_zz - f = 0
      ! 
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     g            = contains grid information (dhc,dhn)
      !     ss           = solver settings (specifies max iterations, tolerance etc.)
      !     gridType     = (1,2) = (cell-based,node-based)
      !     displayTF    = print residuals to screen (T,F)
      ! 
      ! Flags: (_PARALLELIZE_PSE_,
      !         _EXPORT_PSE_CONVERGENCE_)

      use grid_mod
      use BCs_mod
      use applyBCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod

      use solverSettings_mod
#ifdef _EXPORT_PSE_CONVERGENCE_
      use IO_tools_mod
#endif
      implicit none

      private
      public :: PseudoTimeSolver,solve
      public :: setTimeStep
      private :: init,delete

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type PseudoTimeSolver
        character(len=5) :: name
        type(SF) :: u,lapu,f,res ! f zeros mean
        real(cp) :: dt
        integer,dimension(3) :: s
      end type
      
      interface setTimeStep; module procedure setTimeStepPSE;end interface
      interface init;        module procedure initPSE;       end interface
      interface delete;      module procedure deletePSE;     end interface
      interface solve;       module procedure solvePSE;      end interface

      contains

      subroutine initPSE(PSE,s)
        implicit none
        type(PseudoTimeSolver),intent(inout) :: PSE
        integer,dimension(3),intent(in) :: s
        PSE%s = s
        call init(PSE%lapu,s)
        call init(PSE%f,s)
        call init(PSE%u,s)
        call init(PSE%res,s)
        PSE%name = 'PSE'
      end subroutine

      subroutine deletePSE(PSE)
        implicit none
        type(PseudoTimeSolver),intent(inout) :: PSE
        call delete(PSE%lapu)
        call delete(PSE%f)
        call delete(PSE%u)
        call delete(PSE%res)
      end subroutine

      subroutine setTimeStepPSE(PSE,dt)
        implicit none
        type(PseudoTimeSolver),intent(inout) :: PSE
        real(cp),intent(in) :: dt
        PSE%dt = dt
      end subroutine

      subroutine solvePSE(PSE,u,f,u_bcs,g,ss,norm,displayTF)
        implicit none
        type(PseudoTimeSolver),intent(inout) :: PSE
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        ! Locals
        integer :: ijk
        logical :: TF,continueLoop
        integer :: maxIterations
#ifdef _EXPORT_PSE_CONVERGENCE_
        integer :: NU
#endif
        
        call init(PSE,shape(f))

        call solverSettingsSet(ss)
        ijk = 0

        ! Boundaries
        call applyAllBCs(u_bcs,u,g) ! Necessary with ghost nodes

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif
        continueLoop = .true.

        call assign(PSE%f,f)
        call assign(PSE%u,u)

#ifdef _EXPORT_PSE_CONVERGENCE_
        NU = newAndOpen('out\','norm_PSE')
#endif

        do while (continueLoop.and.TF)
          ijk = ijk + 1

          call lap(PSE%lapu,PSE%u%phi,g)
          call subtract(PSE%res,PSE%lapu,PSE%f)
          call multiply(PSE%res,PSE%dt)
          call add(PSE%u,PSE%res)

          call applyAllBCs(u_bcs,PSE%u%phi,g)

          if (getMinToleranceTF(ss)) then
            call lap(PSE%lapu%phi,PSE%u%phi,g)
            call subtract(PSE%res,PSE%lapu,PSE%f)
            call zeroGhostPoints(PSE%res)
            call compute(norm,real(0.0,cp),PSE%res%phi)
            call setTolerance(ss,getR2(norm))
          endif

#ifdef _EXPORT_PSE_CONVERGENCE_
            call lap(PSE%lapu%phi,PSE%u%phi,g)
            call subtract(PSE%res,PSE%lapu,PSE%f)
            call zeroGhostPoints(PSE%res)
            call compute(norm,real(0.0,cp),PSE%res%phi)
            write(NU,*) getL1(norm),getL2(norm),getLinf(norm)
#endif

          call setIteration(ss,ijk)

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          if (.not.continueLoop) exit
          ! ************************************************************************************
        enddo

#ifdef _EXPORT_PSE_CONVERGENCE_
        close(NU)
#endif
        u = PSE%u%phi
        
        ! Subtract mean (for Pressure Poisson)
        ! This step is not necessary if mean(f) = 0 and all BCs are Neumann.
        if (allNeumann(u_bcs)) then
          u = u - sum(u)/(max(1,size(u)))
        endif

        ! Okay for PSE alone when comparing with u_exact, but not okay for MG
        ! if (.not.allNeumann(u_bcs)) then
        !   u = u - sum(u)/(max(1,size(u)))
        ! endif

        if (displayTF) then
          write(*,*) '(Final,max) '//PSE%name//' iteration = ',ijk,maxIterations

          call lap(PSE%lapu%phi,u,g)
          call subtract(PSE%res,PSE%lapu,PSE%f)
          call zeroGhostPoints(PSE%res%phi)
          call compute(norm,real(0.0,cp),PSE%res%phi)
          call print(norm,PSE%name//' Residuals for '//trim(adjustl(getName(ss))))
        endif

        call delete(PSE)
      end subroutine

      end module