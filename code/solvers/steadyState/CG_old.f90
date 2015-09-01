      module CG_mod
      ! call CG(u,f,u_bcs,g,ss,gridType,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), grid (g)
      ! and solver settings (ss) using the Conjugate Gradient (CG) Method
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
      ! Flags: (_PARALLELIZE_CG_,
      !         _EXPORT_CG_CONVERGENCE_)

      use grid_mod
      use BCs_mod
      use tridiag_mod
      use applyBCs_mod
      use norms_mod
      use scalarField_mod
      use ops_discrete_mod
      use ops_aux_mod

      use solverSettings_mod
#ifdef _EXPORT_CG_CONVERGENCE_
      use IO_tools_mod
#endif
      implicit none

      private
      public :: CGSolver,solve
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
      real(cp),parameter :: PI = 3.14159265358979

      type CGSolver
        character(len=5) :: name
        type(grid) :: p,d,g ! Primary / Dual grids
        type(scalarField) :: lapu,f,res,q,temp ! f zeros mean
        real(cp) :: dt,delta,alpha
        integer :: solutionForm ! (0,1,2) = (Undefined ,Heat Equation, Poisson Equation)
        integer,dimension(3) :: gt,s
        integer :: gridType ! (1,2,3) = (CC,N,Face)
      end type
      
      interface init;        module procedure initCG;             end interface
      interface delete;      module procedure deleteCG;           end interface
      interface solve;       module procedure solveCG;            end interface

      contains

      subroutine initCG(CG,s,g)
        implicit none
        type(CGSolver),intent(inout) :: CG
        integer,dimension(3),intent(in) :: s
        type(grid),intent(in) :: g
        integer :: Nx,Ny,Nz,i

        CG%solutionForm = 0
        
        CG%s = s

        do i=1,3
          if (CG%s(i).eq.g%c(i)%sc) then
            call init(CG%p,g%c(i)%hc,i,2) ! grid made from cc --> p%dhn is dhc
            call init(CG%d,g%c(i)%hn,i,2) ! grid made from n --> d%dhn is dhn
            CG%gt(i) = 1
          elseif(CG%s(i).eq.g%c(i)%sn) then
            call init(CG%p,g%c(i)%hn,i,2) ! grid made from n --> p%dhn is dhn
            call init(CG%d,g%c(i)%hc,i,2) ! grid made from cc --> d%dhn is dhc
            CG%gt(i) = 0
          else
            write(*,*) 's = ',CG%s
            write(*,*) 'sc = ',(/g%c(1)%sc,g%c(2)%sc,g%c(3)%sc/)
            write(*,*) 'sn = ',(/g%c(1)%sn,g%c(2)%sn,g%c(3)%sn/)
            stop 'Error: grid type was not determined in CG.f90'
          endif
        enddo

        if (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then ! Node data
        CG%gridType = 1

        elseif (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then ! CC data
        CG%gridType = 2

        ! Face data
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sc/))) then
        CG%gridType = 3
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
        CG%gridType = 3
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
        CG%gridType = 3

        ! Edge Data
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sn/))) then
          stop 'Error: edge data not yet supported in CG.f90'
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
          stop 'Error: edge data not yet supported in CG.f90'
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
          stop 'Error: edge data not yet supported in CG.f90'
        else
          write(*,*) 's = ',s
          write(*,*) 'g%sn = ',(/(g%c(i)%sn, i=1,3)/)
          write(*,*) 'g%sc = ',(/(g%c(i)%sc, i=1,3)/)
          stop 'Error: in grid size compared to input field in CG.f90.'
        endif


        call allocateField(CG%lapu,CG%s)
        call allocateField(CG%f,CG%s)
        call allocateField(CG%res,CG%s)
        call allocateField(CG%q,CG%s)
        call allocateField(CG%temp,CG%s)
      end subroutine

      subroutine deleteCG(CG)
        implicit none
        type(CGSolver),intent(inout) :: CG
        call delete(CG%p)
        call delete(CG%d)

        call delete(CG%lapu)
        call delete(CG%f)
        call delete(CG%res)
        call delete(CG%q)
        call delete(CG%temp)

        ! write(*,*) 'CG object deleted'
      end subroutine

      subroutine solveCG(CG,u,f,u_bcs,g,ss,norm,displayTF)
        implicit none
        type(CGSolver),intent(inout) :: CG
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
#ifdef _EXPORT_CG_CONVERGENCE_
        integer :: NU
#endif
        
        call init(CG,shape(f),g)

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

        CG%f%phi = f ! CANNOT REMOVE MEAN FOR NEUMANN, RESULTS IN BAD RESIDUALS FOR CG

#ifdef _EXPORT_CG_CONVERGENCE_
        NU = newAndOpen('out\','norms_CG')
#endif

        ! lapU = lap(u)
        call lap(CG%lapu%phi,u,g)
        ! res = f - lap(u)
        call subtract(CG%res,CG%f,CG%lapu)
        ! delta = r^T*r
        call dotProduct(CG%delta,CG%res,CG%res,CG%temp)

        do while (continueLoop.and.TF)
          ijk = ijk + 1

          ! q = lap(res)
          call lap(CG%q%phi,CG%res%phi,g)

          ! alpha = delta/r^T q
          call dotProduct(CG%alpha,CG%res,CG%q,CG%temp)
          CG%alpha = CG%delta/CG%alpha
          call zeroGhostPoints(CG%res%phi)

          ! u = u + alpha*res
          u = u + CG%alpha*CG%res%phi

          if (mod(ijk,50).eq.0) then
            ! res = f - lap(u)
            call lap(CG%lapu%phi,u,g)
            call subtract(CG%res,CG%f,CG%lapu)
          else
            ! res = res - alpha*q
            call subtract(CG%res,CG%alpha*CG%q)
          endif

          ! delta = r^T*r
          call dotProduct(CG%delta,CG%res,CG%res,CG%temp)

          call applyAllBCs(u_bcs,u,g)

          if (getMinToleranceTF(ss)) then
            call lap(CG%lapu,u,g)
            call subtract(CG%res,CG%lapu,CG%f)
            call zeroGhostPoints(CG%res%phi)
            call compute(norm,CG%res%phi)
            call setTolerance(ss,getR2(norm))
          endif

#ifdef _EXPORT_CG_CONVERGENCE_
            call lap(CG%lapu,u,g)
            call subtract(CG%res,CG%lapu,CG%f)
            call zeroGhostPoints(CG%res%phi)
            call compute(norm,CG%res%phi)
            write(NU,*) getL1(norm),getL2(norm),getLinf(norm)
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
        ! This step is not necessary if mean(f) = 0 and all BCs are Neumann.
        if (allNeumann(u_bcs)) then
          u = u - sum(u)/(max(1,size(u)))
        endif

        ! Okay for CG alone when comparing with u_exact, but not okay for MG
        ! if (.not.allNeumann(u_bcs)) then
        !   u = u - sum(u)/(max(1,size(u)))
        ! endif

        if (displayTF) then
          write(*,*) '(Final,max) '//CG%name//' iteration = ',ijk,maxIterations

          call lap(CG%lapu,u,g)
          CG%res = CG%lapu - CG%f
          call zeroGhostPoints(CG%res%phi)
          call compute(norm,CG%res%phi)
          call print(norm,CG%name//' Residuals for '//trim(adjustl(getName(ss))))
        endif

        call delete(CG)
      end subroutine

      subroutine dotProduct(dot,A,B,temp)
        implicit none
        real(cp),intent(inout) :: dot
        type(scalarField),intent(in) :: A,B
        type(scalarField),intent(inout) :: temp
        call multiply(temp,A,B)
        dot = sum(temp%phi)
      end subroutine

      subroutine CG_loop(u,f,s,dxp,dyp,dzp,dxd,dyd,dzd,dt,gt,odd)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        integer,dimension(3) :: s,odd
        real(cp),dimension(:),intent(in) :: dxp,dyp,dzp,dxd,dyd,dzd
        real(cp),intent(in) :: dt
        integer,dimension(3),intent(in) :: gt
        integer :: i,j,k
        real(cp) :: r

#ifdef _PARALLELIZE_CG_
        !$OMP DO PRIVATE(r)

#endif

        do k=2+odd(3),s(3)-1,2
          do j=2+odd(2),s(2)-1,2
            do i=2+odd(1),s(1)-1,2

              r = real(1.0,cp)/dxd(i-1+gt(1))*(real(1.0,cp)/dxp(i) + real(1.0,cp)/dxp(i-1)) + & 
                  real(1.0,cp)/dyd(j-1+gt(2))*(real(1.0,cp)/dyp(j) + real(1.0,cp)/dyp(j-1)) + & 
                  real(1.0,cp)/dzd(k-1+gt(3))*(real(1.0,cp)/dzp(k) + real(1.0,cp)/dzp(k-1))

              u(i,j,k) = u(i,j,k)*(real(1.0,cp)-dt) + &
                 dt*( u(i-1,j,k)/(dxp(i-1) * dxd(i-1+gt(1))) + &
                         u(i+1,j,k)/(dxp( i ) * dxd(i-1+gt(1))) + &
                         u(i,j-1,k)/(dyp(j-1) * dyd(j-1+gt(2))) + &
                         u(i,j+1,k)/(dyp( j ) * dyd(j-1+gt(2))) + &
                         u(i,j,k-1)/(dzp(k-1) * dzd(k-1+gt(3))) + &
                         u(i,j,k+1)/(dzp( k ) * dzd(k-1+gt(3))) &
                       - f(i,j,k) )/r

            enddo
          enddo
        enddo

#ifdef _PARALLELIZE_CG_
        !$OMP END DO

#endif
      end subroutine

      end module