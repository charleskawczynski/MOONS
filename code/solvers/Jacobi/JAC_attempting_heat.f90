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
      use IO_SF_mod
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
      public :: compute_Au
      public :: makeTransient

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
        type(triDiag),dimension(3) :: T1,T2,D1,D2,U1,U2 ! tridiagonals
        type(VF) :: temp ! intermediate fields, same size as sigma
        real(cp) :: dt
      end type
      
      interface init;        module procedure initJAC;       end interface
      interface delete;      module procedure deleteJAC;     end interface
      interface solve;       module procedure solveJAC;      end interface

      contains

      subroutine initJAC(JAC,u,g,sigma)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        type(SF),intent(in) :: u
        type(VF),intent(in) :: sigma
        type(grid),intent(in) :: g
        integer :: i
        JAC%g = g
        call init(JAC%Au,u)
        call init(JAC%res,u)
        call init(JAC%Dinv,u)
        call init(JAC%temp,sigma)

        ! Init Laplacian stencil:
        if (u%is_CC) then
          do i=1,3; call init(JAC%T1(i),g%c(i)%stagCC2N); enddo
          do i=1,3; call init(JAC%T2(i),g%c(i)%stagN2CC); enddo
        elseif (u%is_Node) then
          do i=1,3; call init(JAC%T1(i),g%c(i)%stagN2CC); enddo
          do i=1,3; call init(JAC%T2(i),g%c(i)%stagCC2N); enddo
        elseif (u%is_Face) then
        elseif (u%is_Edge) then
        else; stop 'Error: data type not detected in JAC_ops.f90'
        endif

        do i=1,3
          call init(JAC%D1(i),JAC%T1(i)); call init(JAC%D2(i),JAC%T2(i))
          call init(JAC%U1(i),JAC%T1(i)); call init(JAC%U2(i),JAC%T2(i))
          JAC%D1(i)%U = 0.0_cp; JAC%D2(i)%U = 0.0_cp
          JAC%U1(i)%D = 0.0_cp; JAC%U2(i)%D = 0.0_cp
        enddo
        call defineDinv(JAC,sigma)
      end subroutine

      subroutine makeTransient(JAC,dt)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        real(cp),intent(in) :: dt
        JAC%dt = dt
        ! call divide(1.0_cp,JAC%Dinv)
        ! call subtract(JAC%Dinv,1.0_cp/dt)
        ! call divide(1.0_cp,JAC%Dinv)
      end subroutine

      subroutine deleteJAC(JAC)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        call delete(JAC%g)
        call delete(JAC%Au)
        call delete(JAC%res)
        call delete(JAC%T1)
        call delete(JAC%T2)
        call delete(JAC%D1)
        call delete(JAC%D2)
        call delete(JAC%U1)
        call delete(JAC%U2)
        call delete(JAC%Dinv)
        call delete(JAC%temp)
      end subroutine

      subroutine defineDinv(JAC,sigma)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        type(VF),intent(in) :: sigma
        type(SF) :: temp
        type(del) :: d
        call init(temp,JAC%Dinv)
        call assign(temp,1.0_cp)
        call d%assign_T(JAC%temp%x,temp    ,JAC%g,JAC%D1(1),1,1)
        call multiply(JAC%temp%x,sigma%x)
        call d%assign_T(JAC%Dinv,JAC%temp%x,JAC%g,JAC%U2(1),1,1)
        call d%assign_T(JAC%temp%x,temp    ,JAC%g,JAC%U1(1),1,1)
        call multiply(JAC%temp%x,sigma%x)
        call d%add_T   (JAC%Dinv,JAC%temp%x,JAC%g,JAC%D2(1),1,1)

        call d%assign_T(JAC%temp%y,temp    ,JAC%g,JAC%D1(2),2,1)
        call multiply(JAC%temp%y,sigma%y)
        call d%add_T   (JAC%Dinv,JAC%temp%y,JAC%g,JAC%U2(2),2,1)
        call d%assign_T(JAC%temp%y,temp    ,JAC%g,JAC%U1(2),2,1)
        call multiply(JAC%temp%y,sigma%y)
        call d%add_T   (JAC%Dinv,JAC%temp%y,JAC%g,JAC%D2(2),2,1)

        call d%assign_T(JAC%temp%z,temp    ,JAC%g,JAC%D1(3),3,1)
        call multiply(JAC%temp%z,sigma%z)
        call d%add_T   (JAC%Dinv,JAC%temp%z,JAC%g,JAC%U2(3),3,1)
        call d%assign_T(JAC%temp%z,temp    ,JAC%g,JAC%U1(3),3,1)
        call multiply(JAC%temp%z,sigma%z)
        call d%add_T   (JAC%Dinv,JAC%temp%z,JAC%g,JAC%D2(3),3,1)

        call delete(temp)
        call subtract(JAC%Dinv,1.0_cp/JAC%dt)
        call divide(1.0_cp,JAC%Dinv)
        call zeroGhostPoints(JAC%Dinv)
      end subroutine

      subroutine compute_Au(Au,u,sigma,g,sig_temp,u_temp,dt)
        implicit none
        type(SF),intent(inout) :: Au
        type(SF),intent(in) :: u
        type(SF),intent(inout) :: u_temp
        type(grid),intent(in) :: g
        type(VF),intent(in) :: sigma
        type(VF),intent(inout) :: sig_temp
        real(cp),intent(in) :: dt
        ! ∇•(σ∇u)
        call grad(sig_temp,u,g)
        call multiply(sig_temp,sigma)
        call div(Au,sig_temp,g)
        ! ∇•(σ∇u) - u/dt
        call assignMinus(u_temp,u)
        call divide(u_temp,dt)
        call subtract(Au,u_temp)
      end subroutine

      subroutine solveJAC(JAC,u,f,sigma,g,ss,norm,displayTF,NU,iter)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(VF),intent(in) :: sigma
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
        integer,intent(in) :: NU,iter
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

! #ifdef _EXPORT_JAC_CONVERGENCE_
!         if (firstTime) then
!           NU = newAndOpen('out\','norm_JAC')
!         else
!           NU = openToAppend('out\','norm_JAC')
!         endif
! #endif

        do while (continueLoop.and.TF)
          ijk = ijk + 1

          call d%assign_T(JAC%temp%x,u     ,g,JAC%D1(1),1,1)
          call multiply(JAC%temp%x,sigma%x)
          call d%assign_T(JAC%Au,JAC%temp%x,g,JAC%D2(1),1,1)
          call d%assign_T(JAC%temp%x,u     ,g,JAC%U1(1),1,1)
          call multiply(JAC%temp%x,sigma%x)
          call d%add_T(JAC%Au,JAC%temp%x   ,g,JAC%U2(1),1,1)

          call d%assign_T(JAC%temp%y,u     ,g,JAC%D1(2),2,1)
          call multiply(JAC%temp%y,sigma%y)
          call d%add_T   (JAC%Au,JAC%temp%y,g,JAC%D2(2),2,1)
          call d%assign_T(JAC%temp%y,u     ,g,JAC%U1(2),2,1)
          call multiply(JAC%temp%y,sigma%y)
          call d%add_T(JAC%Au,JAC%temp%y   ,g,JAC%U2(2),2,1)

          call d%assign_T(JAC%temp%z,u     ,g,JAC%D1(3),3,1)
          call multiply(JAC%temp%z,sigma%z)
          call d%add_T   (JAC%Au,JAC%temp%z,g,JAC%D2(3),3,1)
          call d%assign_T(JAC%temp%z,u     ,g,JAC%U1(3),3,1)
          call multiply(JAC%temp%z,sigma%z)
          call d%add_T(JAC%Au,JAC%temp%z   ,g,JAC%U2(3),3,1)

          call subtract(JAC%res,f,JAC%Au)
          call multiply(u,JAC%Dinv,JAC%res)

          call applyAllBCs(u,g)

          if (getMinToleranceTF(ss)) then
            call compute_Au(JAC%Au,u,sigma,g,JAC%temp,JAC%res,JAC%dt)
            call subtract(JAC%res,JAC%Au,f)
            call zeroGhostPoints(JAC%res)
            call compute(norm,JAC%res,g)
            call setTolerance(ss,norm%L2)
          endif

#ifdef _EXPORT_JAC_CONVERGENCE_
            call compute_Au(JAC%Au,u,sigma,g,JAC%temp,JAC%res,JAC%dt)
            call subtract(JAC%res,JAC%Au,f)
            call zeroGhostPoints(JAC%res)
            call compute(norm,JAC%res,g)
            write(NU,*) (1 + (iter-1)*100),norm%L1,norm%L2,norm%Linf
#endif

          call setIteration(ss,ijk)

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          if (.not.continueLoop) exit
          ! ************************************************************************************
        enddo

! #ifdef _EXPORT_JAC_CONVERGENCE_
!         if (lastTime) close(NU)
! #endif
        call export_1C_SF(g,JAC%res,'out\','R_i'//'JAC'//'_'//int2str((1 + (iter-1)*100)),0)
        
        ! Subtract mean (for Pressure Poisson)
        ! Okay for JAC alone when comparing with u_exact, but not okay for MG
        ! This step is not necessary if mean(f) = 0 and all BCs are Neumann.
        if (getAllNeumann(u%RF(1)%b)) then
          call subtract(u,mean(u))
        endif

        if (displayTF) then
          write(*,*) '(Final,max) JAC iteration = ',ijk,maxIterations

          call compute_Au(JAC%Au,u,sigma,g,JAC%temp,JAC%res,JAC%dt)
          call subtract(JAC%res,JAC%Au,f)
          call zeroGhostPoints(JAC%res)
          call compute(norm,JAC%res,g)
          call print(norm,'Jacobi residuals for '//trim(adjustl(getName(ss))))
        endif

      end subroutine

      end module