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
      use SF_mod
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
        if (is_CC(u,g)) then
          do i=1,3; call init(JAC%T(i),g%lapCC); enddo
        elseif (is_Node(u,g)) then
          do i=1,3; call init(JAC%T(i),g%lapN); enddo
        elseif (is_Face(u,g)) then
          select case (faceDir(u))
          case (1); call init(JAC%T(1),g%lapN);  call init(JAC%T(2),g%lapCC); call init(JAC%T(3),g%lapCC)
          case (2); call init(JAC%T(1),g%lapCC); call init(JAC%T(2),g%lapN);  call init(JAC%T(3),g%lapCC)
          case (3); call init(JAC%T(1),g%lapCC); call init(JAC%T(2),g%lapCC); call init(JAC%T(3),g%lapN)
          end select
        elseif (is_Edge(u,g)) then
          select case (edgeDir(u))
          case (1); call init(JAC%T(1),g%lapCC); call init(JAC%T(2),g%lapN);  call init(JAC%T(3),g%lapN)
          case (2); call init(JAC%T(1),g%lapN);  call init(JAC%T(2),g%lapCC); call init(JAC%T(3),g%lapN)
          case (3); call init(JAC%T(1),g%lapN);  call init(JAC%T(2),g%lapN);  call init(JAC%T(3),g%lapCC)
          end select
        endif

        call init(JAC%L,JAC%T)
        call init(JAC%D,JAC%T)
        call init(JAC%U,JAC%T)
        JAC%L%D = 0.0_cp; JAC%L%U = 0.0_cp
        JAC%D%L = 0.0_cp; JAC%D%U = 0.0_cp
        JAC%U%L = 0.0_cp; JAC%U%D = 0.0_cp

        ! JAC%D%D = 1.0_cp/JAC%D%D

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

          ! THE ORDER OF THESE ROUTINE CALLS IS IMPORTANT. DO NOT CHANGE.

#ifdef _PARALLELIZE_JAC_
          !$OMP PARALLEL

#endif

          call innerLoop(u,f,g) ! Even in odd plane

!           call innerLoop(u,f,g) ! Even in even plane
!           call innerLoop(u,f,g) ! Even in even plane
!           call innerLoop(u,f,g) ! Even in even plane


! #ifdef _PARALLELIZE_JAC_
!           !$OMP END PARALLEL
!           !$OMP PARALLEL

! #endif

!           call innerLoop(u,f,g) ! Odd in odd plane

!           call innerLoop(u,f,g) ! Odd in even plane
!           call innerLoop(u,f,g) ! Odd in even plane
!           call innerLoop(u,f,g) ! Odd in even plane

! #ifdef _PARALLELIZE_JAC_
!           !$OMP END PARALLEL

! #endif

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
          write(*,*) 'JAC parameter = ',JAC%omega
          write(*,*) '(Final,max) '//JAC%name//' iteration = ',ijk,maxIterations

          call lap(JAC%lapu,u,g)
          call subtract(JAC%res,JAC%lapu,f)
          call zeroGhostPoints(JAC%res)
          call compute(norm,JAC%res,g)
          call print(norm,JAC%name//' Residuals for '//trim(adjustl(getName(ss))))
        endif

        ! call delete(JAC)
      end subroutine

      subroutine innerLoop(JAC,u,f,g,odd)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        integer,dimension(3),intent(in) :: odd
        integer :: i
        do i=1,u%s
          call redBlack(JAC,u%RF(i)%f,f%RF(i)%f,r%RF(i)%f,g,u%RF(i)%s,odd)
        enddo
      end subroutine

      subroutine redBlack(JAC,u,f,g,s,odd)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,dimension(3),intent(in) :: odd

        call d%assign(JAC%lapu,u,g,JAC%T(1)%U,1,1)
           call d%add(JAC%lapu,u,g,JAC%T(2)%U,2,1)
           call d%add(JAC%lapu,u,g,JAC%T(3)%U,3,1)

#ifdef _PARALLELIZE_JAC_
        !$OMP DO

#endif

        do k=2+odd(3),s(3)-1,2
          do j=2+odd(2),s(2)-1,2
            do i=2+odd(1),s(1)-1,2

              u(i,j,k) = u(i,j,k)*(1.0_cp-omega) + &
                 omega*( u(i-1,j,k)/(dxp(i-1) * dxd(i-1+gt(1))) + &
                         u(i+1,j,k)/(dxp( i ) * dxd(i-1+gt(1))) + &
                         u(i,j-1,k)/(dyp(j-1) * dyd(j-1+gt(2))) + &
                         u(i,j+1,k)/(dyp( j ) * dyd(j-1+gt(2))) + &
                         u(i,j,k-1)/(dzp(k-1) * dzd(k-1+gt(3))) + &
                         u(i,j,k+1)/(dzp( k ) * dzd(k-1+gt(3))) &
                       - f(i,j,k) )/r(i,j,k)

            enddo
          enddo
        enddo

#ifdef _PARALLELIZE_JAC_
        !$OMP END DO

#endif

      end subroutine


      end module