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
      use ops_del_mod
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
        type(SF) :: Au,res,Dinv ! laplacian, residual, coefficient
        type(triDiag),dimension(3) :: T,D,LU ! tridiagonals
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
        call init(JAC%Au,u)
        call init(JAC%res,u)
        call init(JAC%Dinv,u)

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
          call init(JAC%LU(i),JAC%T(i))
          call init(JAC%D(i),JAC%T(i))
          JAC%LU(i)%D = 0.0_cp
          JAC%D(i)%L = 0.0_cp; JAC%D(i)%U = 0.0_cp
          JAC%D(i)%D = JAC%D(i)%D ! Invert D (element by element)
        enddo
        call defineDinv(JAC)
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
        call delete(JAC%Au)
        call delete(JAC%res)
        call delete(JAC%T)
        call delete(JAC%LU)
        call delete(JAC%D)
        call delete(JAC%Dinv)
      end subroutine

      subroutine defineDinv(JAC)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        type(SF) :: temp
        type(del) :: d
        call init(temp,JAC%Dinv)
        call assign(temp,1.0_cp)
        call d%assign_T(JAC%Dinv,temp,JAC%g,JAC%D(1),1,1)
        call d%add_T   (JAC%Dinv,temp,JAC%g,JAC%D(2),2,1)
        call d%add_T   (JAC%Dinv,temp,JAC%g,JAC%D(3),3,1)
        call delete(temp)
        call divide(1.0_cp,JAC%Dinv)
      end subroutine

      subroutine compute_Au(Au,u,g)
        implicit none
        type(SF),intent(inout) :: Au
        type(SF),intent(inout) :: u
        type(grid),intent(in) :: g
        call lap(Au,u,g)
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
          call d%assign_T(JAC%Au,u,g,JAC%LU(1),1,1) ! LU_x
          call d%add_T   (JAC%Au,u,g,JAC%LU(2),2,1) ! LU_y
          call d%add_T   (JAC%Au,u,g,JAC%LU(3),3,1) ! LU_z

          call subtract(JAC%res,f,JAC%Au)
          call multiply(u,JAC%Dinv,JAC%res)

          call applyAllBCs(u,g)

          if (getMinToleranceTF(ss)) then
            call compute_Au(JAC%Au,u,g)
            call subtract(JAC%res,JAC%Au,f)
            call zeroGhostPoints(JAC%res)
            call compute(norm,JAC%res,g)
            call setTolerance(ss,norm%L2)
          endif

#ifdef _EXPORT_JAC_CONVERGENCE_
            call compute_Au(JAC%Au,u,g)
            call subtract(JAC%res,JAC%Au,f)
            call zeroGhostPoints(JAC%res)
            call compute(norm,JAC%res,g)
            write(NU,*) ijk,norm%L1,norm%L2,norm%Linf
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
        endif

        if (displayTF) then
          write(*,*) '(Final,max) JAC iteration = ',ijk,maxIterations

          call compute_Au(JAC%Au,u,g)
          call subtract(JAC%res,JAC%Au,f)
          call zeroGhostPoints(JAC%res)
          call compute(norm,JAC%res,g)
          call print(norm,'Jacobi residuals for '//trim(adjustl(getName(ss))))
        endif

      end subroutine

      end module