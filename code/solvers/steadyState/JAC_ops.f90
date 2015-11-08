      module JAC_mod
      ! call JACSolver(JAC,u,f,u_bcs,m,ss,norm,displayTF)
      ! solves the equation:
      !     ! Au = f
      ! for a given f, boundary conditions for u (u_bcs), mesh (m)
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
      !     m            = contains mesh information (dhc,dhn)
      !     ss           = solver settings (specifies max iterations, tolerance etc.)
      !     norm         = Ln norms of residual
      !     displayTF    = print residuals to screen (T,F)
      ! 
      ! Flags: (_PARALLELIZE_JAC_,
      !         _EXPORT_JAC_CONVERGENCE_)

      use grid_mod
      use mesh_mod
      use applyBCs_mod
      use applyStitches_mod
      use BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use triDiag_mod
      use SF_mod
      use IO_SF_mod
      use ops_del_mod
      use ops_split_mod
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
        type(mesh) :: m ! mesh
        type(SF) :: Au,res,Dinv ! laplacian, residual, coefficient
        type(VF) :: temp ! intermediate fields, same size as sigma
        type(VF) :: sigma ! for sigma = uniform
      end type
      
      interface init;        module procedure initJAC;       end interface
      interface delete;      module procedure deleteJAC;     end interface
      interface solve;       module procedure solveJAC;      end interface

      contains

      subroutine initJAC(JAC,u,m)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        call init(JAC%m,m)
        call init(JAC%Au,u)
        call init(JAC%res,u)
        call init(JAC%Dinv,u)
        call init_Face(JAC%sigma,m)
        call init(JAC%temp,JAC%sigma)

        ! Init Diagonal inverse:
        call defineDinv(JAC)
      end subroutine

      subroutine deleteJAC(JAC)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        call delete(JAC%m)
        call delete(JAC%Au)
        call delete(JAC%res)
        call delete(JAC%Dinv)
        call delete(JAC%temp)
        call delete(JAC%sigma)
      end subroutine

      subroutine defineDinv(JAC)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        type(SF) :: temp
        type(split) :: splt
        call init(temp,JAC%Dinv)
        call assign(JAC%sigma,1.0_cp)
        call assign(temp,1.0_cp)
        call splt%assign_D(JAC%Dinv,temp,JAC%sigma%x,JAC%m,1,1)
           call splt%add_D(JAC%Dinv,temp,JAC%sigma%y,JAC%m,2,1)
           call splt%add_D(JAC%Dinv,temp,JAC%sigma%z,JAC%m,3,1)

        call export_3D_1C(JAC%m,JAC%Dinv,'out/LDC/Ufield/','D_inv',0)
        call divide(1.0_cp,JAC%Dinv)
        call zeroGhostPoints(JAC%Dinv)
      end subroutine

      subroutine compute_Au(Au,u,m,sig_temp)
        implicit none
        type(SF),intent(inout) :: Au
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: sig_temp
        ! ∇•(σ∇u)
        call grad(sig_temp,u,m)
        call div(Au,sig_temp,m)
      end subroutine

      subroutine solveJAC(JAC,u,f,m,ss,norm,displayTF)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        type(solverSettings),intent(inout) :: ss
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        type(split) :: splt
        ! Locals
        integer :: ijk
        logical :: TF,continueLoop
        integer :: maxIterations
#ifdef _EXPORT_JAC_CONVERGENCE_
        integer,intent(in) :: NU
#endif
        
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

! #ifdef _EXPORT_JAC_CONVERGENCE_
!         if (firstTime) then
!           NU = newAndOpen('out\','norm_JAC')
!         else
!           NU = openToAppend('out\','norm_JAC')
!         endif
! #endif

        do while (continueLoop.and.TF)
          ijk = ijk + 1

          call splt%assign_LU(JAC%Au,u,JAC%sigma%x,m,1,1)
             call splt%add_LU(JAC%Au,u,JAC%sigma%y,m,2,1)
             call splt%add_LU(JAC%Au,u,JAC%sigma%z,m,3,1)

          call subtract(JAC%res,f,JAC%Au)
          call multiply(u,JAC%Dinv,JAC%res)

          ! call applyStitches(u,m)
          call applyBCs(u,m)

          if (getMinToleranceTF(ss)) then
            call compute_Au(JAC%Au,u,m,JAC%temp)
            call subtract(JAC%res,JAC%Au,f)
            call zeroGhostPoints(JAC%res)
            call compute(norm,JAC%res,m)
            call setTolerance(ss,norm%L2)
          endif

#ifdef _EXPORT_JAC_CONVERGENCE_
            call compute_Au(JAC%Au,u,m,JAC%temp)
            call subtract(JAC%res,JAC%Au,f)
            call zeroGhostPoints(JAC%res)
            call compute(norm,JAC%res,m)
            write(NU,*) norm%L1,norm%L2,norm%Linf
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
        
        ! Subtract mean (for Pressure Poisson)
        ! Okay for JAC alone when comparing with u_exact, but not okay for MG
        ! This step is not necessary if mean(f) = 0 and all BCs are Neumann.
        if (getAllNeumann(u%RF(1)%b)) then
        ! if (u%all_neumann) then
          call subtract(u,mean(u))
        endif

        if (displayTF) then
          write(*,*) '(Final,max) JAC iteration = ',ijk,maxIterations

          call compute_Au(JAC%Au,u,m,JAC%temp)
          call subtract(JAC%res,JAC%Au,f)
          call zeroGhostPoints(JAC%res)
          call compute(norm,JAC%res,m)
          call print(norm,'Jacobi residuals for '//trim(adjustl(getName(ss))))
        endif

      end subroutine

      end module