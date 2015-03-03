      module mySOR_mod
      ! call mySOR(u,f,u_bcs,g,ss,gridType,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), grid (g)
      ! and solver settings (ss) using the iterative Successive Over 
      ! Realxation (SOR) method
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
      ! Fixes/improvements:
      ! - 'If ANY conditions are Dirichlet, then the Neumann conditions
      !   can be taken outside of the iteration loop becuase the matrix 
      !   is no longer singular.' --Gautam
      ! 
      ! - Clean up a bit, it seems some code is written twice

      ! To parallelize, use pre-processor directive:
      !                  _PARALLELIZE_SOR_

      use grid_mod
      use BCs_mod
      use applyBCs_mod
      use myError_mod
      use vectorOps_mod
      use solverSettings_mod
      implicit none

      private
      public :: mySOR,solve
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

      logical, parameter :: useGaussSeidel = .true.

      type mySOR
        character(len=3) :: name
        integer :: gridType
        real(cp),dimension(:),allocatable :: dxp,dyp,dzp
        real(cp),dimension(:),allocatable :: dxd,dyd,dzd
        real(cp),dimension(:,:,:),allocatable :: lapu
        real(cp) :: omega
        integer,dimension(3) :: s
      end type

      interface mySOR;       module procedure SOR;           end interface
      interface init;        module procedure initSOR;       end interface
      interface delete;      module procedure deleteSOR;     end interface
      interface solve;       module procedure solveSOR;      end interface

      contains

      function SOR() result(S)
        implicit none
        type(mySOR) :: S
        S%name = 'SOR'
      end function

      subroutine initSOR(SOR,f,g,gridType)
        implicit none
        type(mySOR),intent(inout) :: SOR
        real(cp),dimension(:,:,:),intent(in) :: f
        integer,intent(in) :: gridType
        type(grid),intent(in) :: g
        integer :: Nx,Ny,Nz

        SOR%gridType = gridType
        SOR%s = shape(f)

        select case (gridType)
        case (1) ! Cell based (for pressure)
          Nx = SOR%s(1)-2; Ny = SOR%s(2)-2; Nz = SOR%s(3)-2   ! number of cells (excluding ghost)
          allocate(SOR%dxp(Nx+1),SOR%dyp(Ny+1),SOR%dzp(Nz+1)) ! Primary grid
          allocate(SOR%dxd(Nx),SOR%dyd(Ny),SOR%dzd(Nz))       ! Dual grid
          SOR%dxp = g%c(1)%dhc
          SOR%dyp = g%c(2)%dhc
          SOR%dzp = g%c(3)%dhc
          SOR%dxd = g%c(1)%dhn
          SOR%dyd = g%c(2)%dhn
          SOR%dzd = g%c(3)%dhn
        case (2) ! Node based (for magnetic field)
          Nx = SOR%s(1)-1; Ny = SOR%s(2)-1; Nz = SOR%s(3)-1   ! number of cells (excluding ghost)
          allocate(SOR%dxd(Nx+1),SOR%dyd(Ny+1),SOR%dzd(Nz+1)) ! Primary grid
          allocate(SOR%dxp(Nx),SOR%dyp(Ny),SOR%dzp(Nz))       ! Dual grid
          SOR%dxd = g%c(1)%dhc
          SOR%dyd = g%c(2)%dhc
          SOR%dzd = g%c(3)%dhc
          SOR%dxp = g%c(1)%dhn
          SOR%dyp = g%c(2)%dhn
          SOR%dzp = g%c(3)%dhn
        case default
          write(*,*) 'gridType in SOR must be 1 or 2. Terminating';stop
        end select
        allocate(SOR%lapu(SOR%s(1),SOR%s(2),SOR%s(3)))

        if (useGaussSeidel) then
          SOR%omega = real(1.0,cp)
          SOR%name = 'GS '
        else
          SOR%omega = real(2.0,cp)/(real(1.0,cp) + sqrt(real(1.0,cp) - & 
           ((cos(PI/real(Nx+1,cp)) + cos(PI/real(Ny+1,cp)) + &
             cos(PI/real(Nz+1,cp)))/real(3.0,cp))**real(2.0,cp)))
          SOR%name = 'SOR'
        endif
      end subroutine

      subroutine deleteSOR(SOR)
        implicit none
        type(mySOR),intent(inout) :: SOR
        deallocate(SOR%dxp)
        deallocate(SOR%dyp)
        deallocate(SOR%dzp)
        deallocate(SOR%dxd)
        deallocate(SOR%dyd)
        deallocate(SOR%dzd)
        deallocate(SOR%lapu)

        ! write(*,*) 'SOR object deleted'
      end subroutine


      subroutine solveSOR(SOR,u,f,u_bcs,g,ss,err,gridType,displayTF)
        implicit none
        type(mySOR),intent(inout) :: SOR
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: err
        integer,intent(in) :: gridType
        logical,intent(in) :: displayTF
        ! Locals
        integer,dimension(3) :: s
        integer :: i,j,k,ijk
        real(cp) :: r
        logical :: TF,continueLoop,TF_allDirichlet
        integer :: maxIterations

        call init(SOR,f,g,gridType)

        s = shape(f)

        TF_allDirichlet = getAllDirichlet(u_bcs)
        call solverSettingsSet(ss)
        ijk = 0

        ! Boundaries
        call applyAllBCs(u_bcs,u,g)

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif
        continueLoop = .true.

        do while (continueLoop.and.TF)
          ijk = ijk + 1
#if _PARALLELIZE_SOR_
          !$OMP PARALLEL PRIVATE(r)
          !$OMP DO
          do k=2,s(3)-1,2
            do j=2,s(2)-1
              do i=2,s(1)-1
                r = real(1.0,cp)/SOR%dxd(i-1)*(real(1.0,cp)/SOR%dxp(i) + real(1.0,cp)/SOR%dxp(i-1)) + & 
                    real(1.0,cp)/SOR%dyd(j-1)*(real(1.0,cp)/SOR%dyp(j) + real(1.0,cp)/SOR%dyp(j-1)) + & 
                    real(1.0,cp)/SOR%dzd(k-1)*(real(1.0,cp)/SOR%dzp(k) + real(1.0,cp)/SOR%dzp(k-1))

                u(i,j,k) = u(i,j,k)*(real(1.0,cp)-SOR%omega) + &

               SOR%omega*( u(i-1,j,k)/(SOR%dxp(i-1) * SOR%dxd(i-1)) + &
                           u(i+1,j,k)/(SOR%dxp( i ) * SOR%dxd(i-1)) + &
                           u(i,j-1,k)/(SOR%dyp(j-1) * SOR%dyd(j-1)) + &
                           u(i,j+1,k)/(SOR%dyp( j ) * SOR%dyd(j-1)) + &
                           u(i,j,k-1)/(SOR%dzp(k-1) * SOR%dzd(k-1)) + &
                           u(i,j,k+1)/(SOR%dzp( k ) * SOR%dzd(k-1)) &
                         - f(i,j,k) )/r
              enddo
            enddo
          enddo
          !$OMP END DO

          !$OMP DO
          do k=3,s(3)-1,2
            do j=2,s(2)-1
              do i=2,s(1)-1
                r = real(1.0,cp)/SOR%dxd(i-1)*(real(1.0,cp)/SOR%dxp(i) + real(1.0,cp)/SOR%dxp(i-1)) + & 
                    real(1.0,cp)/SOR%dyd(j-1)*(real(1.0,cp)/SOR%dyp(j) + real(1.0,cp)/SOR%dyp(j-1)) + & 
                    real(1.0,cp)/SOR%dzd(k-1)*(real(1.0,cp)/SOR%dzp(k) + real(1.0,cp)/SOR%dzp(k-1))

                u(i,j,k) = u(i,j,k)*(real(1.0,cp)-SOR%omega) + &

               SOR%omega*( u(i-1,j,k)/(SOR%dxp(i-1) * SOR%dxd(i-1)) + &
                           u(i+1,j,k)/(SOR%dxp( i ) * SOR%dxd(i-1)) + &
                           u(i,j-1,k)/(SOR%dyp(j-1) * SOR%dyd(j-1)) + &
                           u(i,j+1,k)/(SOR%dyp( j ) * SOR%dyd(j-1)) + &
                           u(i,j,k-1)/(SOR%dzp(k-1) * SOR%dzd(k-1)) + &
                           u(i,j,k+1)/(SOR%dzp( k ) * SOR%dzd(k-1)) &
                         - f(i,j,k) )/r
              enddo
            enddo
          enddo
          !$OMP END DO
          !$OMP END PARALLEL
#else
          do k=2,s(3)-1
            do j=2,s(2)-1
              do i=2,s(1)-1
                r = real(1.0,cp)/SOR%dxd(i-1)*(real(1.0,cp)/SOR%dxp(i) + real(1.0,cp)/SOR%dxp(i-1)) + & 
                    real(1.0,cp)/SOR%dyd(j-1)*(real(1.0,cp)/SOR%dyp(j) + real(1.0,cp)/SOR%dyp(j-1)) + & 
                    real(1.0,cp)/SOR%dzd(k-1)*(real(1.0,cp)/SOR%dzp(k) + real(1.0,cp)/SOR%dzp(k-1))

                u(i,j,k) = u(i,j,k)*(real(1.0,cp)-SOR%omega) + &

               SOR%omega*( u(i-1,j,k)/(SOR%dxp(i-1) * SOR%dxd(i-1)) + &
                           u(i+1,j,k)/(SOR%dxp( i ) * SOR%dxd(i-1)) + &
                           u(i,j-1,k)/(SOR%dyp(j-1) * SOR%dyd(j-1)) + &
                           u(i,j+1,k)/(SOR%dyp( j ) * SOR%dyd(j-1)) + &
                           u(i,j,k-1)/(SOR%dzp(k-1) * SOR%dzd(k-1)) + &
                           u(i,j,k+1)/(SOR%dzp( k ) * SOR%dzd(k-1)) &
                         - f(i,j,k) )/r
              enddo
            enddo
          enddo
#endif

          if (.not.TF_allDirichlet) then
            call applyAllBCs(u_bcs,u,g)
          endif

          if (getMinToleranceTF(ss).and.(ijk.eq.2)) then
            select case (SOR%gridType)
            case (1); call CC2CCLap(SOR%lapu,u,g)
            case (2); call myNodeLap(SOR%lapu,u,g)
            end select
            call compute(err,f(2:s(1)-1,2:s(2)-1,2:s(3)-1),SOR%lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
            call setTolerance(ss,getL2Rel(err))
          endif

          call setIteration(ss,ijk)

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          if (.not.continueLoop) exit
          ! ************************************************************************************
        enddo
        
        ! Subtract mean (for Pressure Poisson)
        if (getSubtractMean(ss)) u = u - sum(u)/(max(1,size(u)))

        if (getAnyDirichlet(u_bcs)) then
          call applyAllBCs(u_bcs,u,g)
        endif

        if (displayTF) then
          write(*,*) 'SOR parameter = ',SOR%omega
          write(*,*) '(Final,max) '//SOR%name//' iteration = ',ijk,maxIterations

          select case (SOR%gridType)
          case (1); call CC2CCLap(SOR%lapu,u,g)
          case (2); call myNodeLap(SOR%lapu,u,g)
          end select
          call compute(err,f(2:s(1)-1,2:s(2)-1,2:s(3)-1),SOR%lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
          call print(err,SOR%name//' Residuals for '//trim(adjustl(getName(ss))))
        endif

        call delete(SOR)
      end subroutine


      end module