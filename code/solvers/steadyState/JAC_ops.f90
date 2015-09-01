      module JAC_mod
      ! call JACSolver(JAC,u,f,u_bcs,g,ss,norm,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), grid (g)
      ! and solver settings (ss) using the iterative Successive Over 
      ! Realxation (JAC) method
      ! 
      ! Note that the variant of Gauss-Seidel/JAC called
      ! "red-black" Gauss-Seidel is used.
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     g            = contains grid information (dhc,dhn)
      !     ss           = solver settings (specifies max iterations, tolerance etc.)
      !     norm         = Ln norms of residual
      !     displayTF    = print residuals to screen (T,F)
      ! 
      ! Flags: (_PARALLELIZE_JAC_,
      !         _EXPORT_JAC_CONVERGENCE_)

      use grid_mod
      use applyBCs_mod
      use BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use triDiag_mod
      use SF_mod
      use del_mod
      use VF_mod

      use solverSettings_mod
#ifdef _EXPORT_JAC_CONVERGENCE_
      use IO_tools_mod
#endif
      implicit none

      private
      public :: JACSolver,solve
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

      type JACSolver
        type(grid) :: g ! grid
        type(SF) :: lapu,res,r ! laplacian, residual, coefficient
        type(triDiag),dimension(3) :: T,L,D,U ! L,D,U for JAC
      end type
      
      interface init;        module procedure initJAC;       end interface
      interface delete;      module procedure deleteJAC;     end interface
      interface solve;       module procedure solveJAC;      end interface

      contains

      subroutine initJAC(JAC,u,g)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        type(SF),intent(in) :: u
        type(grid),intent(in) :: g
        integer :: i
        JAC%g = g
        call init(JAC%lapu,u)
        call init(JAC%res,u)

        ! Init Laplacian stencil:
        if (u%RF(1)%is_CC) then
          do i=1,3; call init(JAC%T(i),g%c(i)%lapCC); enddo
        elseif (u%RF(1)%is_Node) then
          do i=1,3; call init(JAC%T(i),g%c(i)%lapN); enddo
        elseif (u%RF(1)%is_Face) then
          ! select case (faceDir(u))
          ! case (1); call init(JAC%T(1),g%c(1)%lapN)
          !           call init(JAC%T(2),g%c(1)%lapCC)
          !           call init(JAC%T(3),g%c(1)%lapCC)
          ! case (2); call init(JAC%T(1),g%c(2)%lapCC)
          !           call init(JAC%T(2),g%c(2)%lapN)
          !           call init(JAC%T(3),g%c(2)%lapCC)
          ! case (3); call init(JAC%T(1),g%c(3)%lapCC)
          !           call init(JAC%T(2),g%c(3)%lapCC)
          !           call init(JAC%T(3),g%c(3)%lapN)
          ! end select
        elseif (u%RF(1)%is_Edge) then
          ! select case (edgeDir(u))
          ! case (1); call init(JAC%T(1),g%c(1)%lapCC)
          !           call init(JAC%T(2),g%c(1)%lapN)
          !           call init(JAC%T(3),g%c(1)%lapN)
          ! case (2); call init(JAC%T(1),g%c(2)%lapN)
          !           call init(JAC%T(2),g%c(2)%lapCC)
          !           call init(JAC%T(3),g%c(2)%lapN)
          ! case (3); call init(JAC%T(1),g%c(3)%lapN)
          !           call init(JAC%T(2),g%c(3)%lapN)
          !           call init(JAC%T(3),g%c(3)%lapCC)
          ! end select
        else
           stop 'Error: data type not detected in JAC_ops.f90'
        endif

        do i=1,3
          call init(JAC%L(i),JAC%T(i))
          call init(JAC%D(i),JAC%T(i))
          call init(JAC%U(i),JAC%T(i))
          JAC%L(i)%D = 0.0_cp; JAC%L(i)%U = 0.0_cp
          JAC%D(i)%L = 0.0_cp; JAC%D(i)%U = 0.0_cp
          JAC%U(i)%L = 0.0_cp; JAC%U(i)%D = 0.0_cp
          JAC%D(i)%D = 1.0_cp/JAC%D(i)%D ! Invert D (element by element)
        enddo

      end subroutine

      ! subroutine makeTransient(JAC,dt)
      !   implicit none
      !   type(JACSolver),intent(inout) :: JAC
      !   real(cp),intent(in) :: dt
      !   type(triDiag) :: temp
      !   integer :: i
      !   call init(temp,JAC%T(1))
      !   JAC%T%L = JAC%T%L
      ! end subroutine

      subroutine deleteJAC(JAC)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        call delete(JAC%g)
        call delete(JAC%lapu)
        call delete(JAC%res)
        call delete(JAC%T)
        call delete(JAC%L)
        call delete(JAC%D)
        call delete(JAC%U)
      end subroutine

      subroutine solveJAC(JAC,u,f,g,ss,norm,displayTF)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        type(del) :: d
        ! Locals
        integer :: ijk
        logical :: TF,continueLoop
        integer :: maxIterations
#ifdef _EXPORT_JAC_CONVERGENCE_
        integer :: NU
#endif
        
        ! call init(JAC,shape(f),g)

        call solverSettingsSet(ss)
        ijk = 0

        ! Boundaries
        call applyAllBCs(u,g) ! Necessary with ghost nodes

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif
        continueLoop = .true.

#ifdef _EXPORT_JAC_CONVERGENCE_
        NU = newAndOpen('out\','norm_JAC')
#endif

        do while (continueLoop.and.TF)
          ijk = ijk + 1

          ! (L + U)^-1
          call d%assign_T(JAC%lapu,u,g,JAC%L(1),1,1)
          call d%add_T   (JAC%lapu,u,g,JAC%L(2),2,1)
          call d%add_T (JAC%lapu,u,g,JAC%L(3),3,1)

          call d%add_T(JAC%lapu,u,g,JAC%U(1),1,1)
          call d%add_T(JAC%lapu,u,g,JAC%U(2),2,1)
          call d%add_T(JAC%lapu,u,g,JAC%U(3),3,1)

          ! {f - (L + U)^-1}
          call subtract(JAC%res,f,JAC%lapu)

          ! D^-1 {f - (L + U)^-1}
          call d%assign_T(u,JAC%res,g,JAC%D(1),1,1)
          call d%add_T   (u,JAC%res,g,JAC%D(2),2,1)
          call d%add_T   (u,JAC%res,g,JAC%D(3),3,1)

          call applyAllBCs(u,g)

          if (getMinToleranceTF(ss)) then
            call lap(JAC%lapu,u,g)
            call subtract(JAC%res,JAC%lapu,f)
            call zeroGhostPoints(JAC%res)
            call compute(norm,JAC%res,g)
            call setTolerance(ss,norm%L2)
          endif

#ifdef _EXPORT_JAC_CONVERGENCE_
            call lap(JAC%lapu,u,g)
            call subtract(JAC%res,JAC%lapu,f)
            call zeroGhostPoints(JAC%res)
            call compute(norm,JAC%res,g)
            write(NU,*) norm%L1,norm%L2,norm%Linf
#endif

          call setIteration(ss,ijk)

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          if (.not.continueLoop) exit
          ! ************************************************************************************
        enddo

#ifdef _EXPORT_JAC_CONVERGENCE_
        close(NU)
#endif
        
        ! Subtract mean (for Pressure Poisson)
        ! Okay for JAC alone when comparing with u_exact, but not okay for MG
        ! This step is not necessary if mean(f) = 0 and all BCs are Neumann.
        if (getAllNeumann(u%RF(1)%b)) then
          call subtract(u,mean(u))
          ! u = u - sum(u)/(max(1,size(u)))
        endif

        if (displayTF) then
          write(*,*) '(Final,max) JAC iteration = ',ijk,maxIterations

          call lap(JAC%lapu,u,g)
          call subtract(JAC%res,JAC%lapu,f)
          call zeroGhostPoints(JAC%res)
          call compute(norm,JAC%res,g)
          call print(norm,'Jacobi residuals for '//trim(adjustl(getName(ss))))
        endif

        ! call delete(JAC)
      end subroutine

      end module