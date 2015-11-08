      module PSE_mod
      ! call PSESolver(PSE,u,f,u_bcs,m,ss,norm,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), mesh (m)
      ! and solver settings (ss) using the iterative Successive Over 
      ! Realxation (PSE) method
      ! 
      ! Note that the variant of Gauss-Seidel/PSE called
      ! "red-black" Gauss-Seidel is used.
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     m            = contains mesh information (dhc,dhn)
      !     ss           = solver settings (specifies max iterations, tolerance etc.)
      !     norm         = Ln norms of residual
      !     displayTF    = print residuals to screen (T,F)
      ! 
      ! Flags: (_PARALLELIZE_PSE_,
      !         _EXPORT_PSE_CONVERGENCE_)

      use grid_mod
      use mesh_mod
      use applyBCs_mod
      use BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod

      use solverSettings_mod
#ifdef _EXPORT_PSE_CONVERGENCE_
      use IO_tools_mod
#endif
      implicit none

      private
      public :: PSESolver,solve
      public :: init,delete

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type PSESolver
        character(len=5) :: name
        type(SF) :: lapu,res ! laplacian, residual
      end type
      
      interface init;        module procedure initPSE;       end interface
      interface delete;      module procedure deletePSE;     end interface
      interface solve;       module procedure solvePSE;      end interface

      contains

      subroutine initPSE(PSE,u,m)
        implicit none
        type(PSESolver),intent(inout) :: PSE
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        call init(PSE%lapu,u)
        call init(PSE%res,u)
      end subroutine

      subroutine deletePSE(PSE)
        implicit none
        type(PSESolver),intent(inout) :: PSE
        call delete(PSE%lapu)
        call delete(PSE%res)
      end subroutine


      subroutine solvePSE(PSE,u,f,m,ss,norm,displayTF)
        implicit none
        type(PSESolver),intent(inout) :: PSE
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
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
        
        ! call init(PSE,shape(f),m)

        call solverSettingsSet(ss)
        ijk = 0

        ! Boundaries
        call applyBCs(u,m) ! Necessary with ghost nodes

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif
        continueLoop = .true.

#ifdef _EXPORT_PSE_CONVERGENCE_
        NU = newAndOpen('out\','norm_PSE')
#endif

        do while (continueLoop.and.TF)
          ijk = ijk + 1

          call lap(PSE%lapu,u,m)
          call subtract(PSE%res,PSE%lapu,f)
          call multiply(PSE%res,0.0000001_cp)
          call add(u,PSE%res)

          call applyBCs(u,m)

          if (getMinToleranceTF(ss)) then
            call lap(PSE%lapu,u,m)
            call subtract(PSE%res,PSE%lapu,f)
            call zeroGhostPoints(PSE%res)
            call compute(norm,PSE%res,m)
            call setTolerance(ss,norm%L2)
          endif

#ifdef _EXPORT_PSE_CONVERGENCE_
            call lap(PSE%lapu,u,m)
            call subtract(PSE%res,PSE%lapu,f)
            call zeroGhostPoints(PSE%res)
            call compute(norm,PSE%res,m)
            write(NU,*) norm%L1,norm%L2,norm%Linf
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
        
        ! Subtract mean (for Pressure Poisson)
        ! Okay for PSE alone when comparing with u_exact, but not okay for MG
        ! This step is not necessary if mean(f) = 0 and all BCs are Neumann.
        if (getAllNeumann(u%RF(1)%b)) then
          call subtract(u,mean(u))
        endif

        if (displayTF) then
          write(*,*) '(Final,max) '//PSE%name//' iteration = ',ijk,maxIterations

          call lap(PSE%lapu,u,m)
          call subtract(PSE%res,PSE%lapu,f)
          call zeroGhostPoints(PSE%res)
          call compute(norm,PSE%res,m)
          call print(norm,PSE%name//' Residuals for '//trim(adjustl(getName(ss))))
        endif

      end subroutine


      end module