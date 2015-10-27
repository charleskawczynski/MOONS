      module CG_mod
      ! call CGSolver(CG,u,f,u_bcs,m,ss,norm,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), mesh (m)
      ! and solver settings (ss) using the Conjugate Gradient (CG) method
      ! 
      ! Note that the variant of Gauss-Seidel/CG called
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
      ! Pre-processor directives: (_PARALLELIZE_CG_,_EXPORT_CG_CONVERGENCE_)

      use mesh_mod
      use applyBCs_mod
      use BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod

      use solverSettings_mod
#ifdef _EXPORT_CG_CONVERGENCE_
      use IO_tools_mod
#endif
      implicit none

      private
      public :: CGSolver,solve
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

      type CGSolver
        type(SF) :: Au,res,q,temp ! laplacian, residual
        real(cp) :: delta,alpha
      end type
      
      interface init;        module procedure initCG;       end interface
      interface delete;      module procedure deleteCG;     end interface
      interface solve;       module procedure solveCG;      end interface

      contains

      subroutine initCG(CG,u)
        implicit none
        type(CGSolver),intent(inout) :: CG
        type(SF),intent(in) :: u
        call init(CG%Au,u)
        call init(CG%res,u)
        call init(CG%q,u)
        call init(CG%temp,u)
      end subroutine

      subroutine deleteCG(CG)
        implicit none
        type(CGSolver),intent(inout) :: CG
        call delete(CG%Au)
        call delete(CG%res)
        call delete(CG%q)
        call delete(CG%temp)
      end subroutine

      subroutine compute_Au(Au,u,m)
        implicit none
        type(SF),intent(inout) :: Au
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        call 
      end subroutine

      subroutine compute_Au(Au,u,sigma)
        implicit none
        type(SF),intent(inout) :: Au
        type(SF),intent(in) :: u,sigma
        
      end subroutine

      subroutine solveCG(CG,u,f,m,ss,norm,displayTF)
        implicit none
        type(CGSolver),intent(inout) :: CG
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
#ifdef _EXPORT_CG_CONVERGENCE_
        integer :: NU
#endif

        call solverSettingsSet(ss)
        ijk = 0

        ! Boundaries
        call applyAllBCs(u,m) ! Necessary with ghost nodes

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif
        continueLoop = .true.

#ifdef _EXPORT_CG_CONVERGENCE_
        NU = newAndOpen('out\','norm_CG')
#endif

        ! Compute Au
        call compute_Au(CG%Au,u,m)
        ! res = f - Au
        call subtract(CG%res,f,CG%Au)
        ! delta = r^T*r
        call dotProduct(CG%delta,CG%res,CG%res,CG%temp)

        do while (continueLoop.and.TF)
          ijk = ijk + 1

          ! q = Au
          call compute_Au(CG%q,CG%res,m)

          ! alpha = delta/r^T q
          call dotProduct(CG%alpha,CG%res,CG%q,CG%temp)
          CG%alpha = CG%delta/CG%alpha
          call zeroGhostPoints(CG%res)

          ! u = u + alpha*res
          call multiply(CG%res,CG%alpha)
          call add(u,CG%res)

          if (mod(ijk,50).eq.0) then
            ! res = f - Au
            call compute_Au(CG%Au,u,m)
            call subtract(CG%res,f,CG%Au)
          else
            ! res = res - alpha*q
            call multiply(CG%q,CG%alpha)
            call subtract(CG%res,CG%q)
          endif

          ! delta = r^T*r
          call dotProduct(CG%delta,CG%res,CG%res,CG%temp)

          call applyAllBCs(u,m)

#ifdef _EXPORT_CG_CONVERGENCE_
            call compute_Au(CG%Au,u,m)
            call subtract(CG%temp,CG%Au,f)
            call zeroGhostPoints(CG%temp)
            call compute(norm,CG%temp,m)
            write(NU,*) norm%L1,norm%L2,norm%Linf
#endif

          call setIteration(ss,ijk)

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          if (.not.continueLoop) exit
          ! ************************************************************************************
        enddo

#ifdef _EXPORT_CG_CONVERGENCE_
        close(NU)
#endif
        
        ! Subtract mean (for Pressure Poisson)
        ! Okay for CG alone when comparing with u_exact, but not okay for MG
        ! This step is not necessary if mean(f) = 0 and all BCs are Neumann.
        if (getAllNeumann(u)) then
          call subtract(u,mean(u))
        endif

        if (displayTF) then
          write(*,*) '(Final,max) CG iteration = ',ijk,maxIterations

          call compute_Au(CG%Au,u,m)
          call subtract(CG%res,CG%Au,f)
          call zeroGhostPoints(CG%res)
          call compute(norm,CG%res,m)
          call print(norm,'CG Residuals for '//trim(adjustl(getName(ss))))
        endif

      end subroutine

      subroutine dotProduct(dot,A,B,temp)
        implicit none
        real(cp),intent(inout) :: dot
        type(SF),intent(in) :: A,B
        type(SF),intent(inout) :: temp
        call multiply(temp,A,B)
        dot = sum(temp)
      end subroutine

      end module