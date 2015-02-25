      module mySOR_mod
      ! call mySOR(u,f,u_bcs,gd,ss,gridType,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), griddata (gd)
      ! and solver settings (ss) using the iterative Successive Over 
      ! Realxation (SOR) method
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     gd           = contains grid information (dhc,dhn)
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

      use constants_mod
      use simParams_mod
      use griddata_mod
      use BCs_mod
      use applyBCs_mod
      use myError_mod
      use vectorOps_mod
      use solverSettings_mod
      implicit none

      private

      public :: mySOR,solve
      
      private :: init,delete

      logical, parameter :: useGaussSeidel = .true.

      type mySOR
        character(len=3) :: name
        integer :: gridType
        real(dpn),dimension(:),allocatable :: dx1,dy1,dz1
        real(dpn),dimension(:),allocatable :: dx2,dy2,dz2
        real(dpn),dimension(:,:,:),allocatable :: lapu
        real(dpn) :: omega
        integer,dimension(3) :: s
      end type

      interface init;        module procedure initSOR;       end interface
      interface delete;      module procedure deleteSOR;     end interface
      interface solve;       module procedure solveSOR;      end interface

      contains

      subroutine initSOR(SOR,f,gd,gridType)
        implicit none
        type(mySOR),intent(inout) :: SOR
        real(dpn),dimension(:,:,:),intent(in) :: f
        integer,intent(in) :: gridType
        type(griddata),intent(in) :: gd
        integer :: Nx,Ny,Nz

        SOR%gridType = gridType
        SOR%s = shape(f)

        select case (gridType)
        case (1) ! Cell based (for pressure)
          Nx = SOR%s(1)-2; Ny = SOR%s(2)-2; Nz = SOR%s(3)-2
          allocate(SOR%dx1(Nx+1),SOR%dy1(Ny+1),SOR%dz1(Nz+1))
          allocate(SOR%dx2(Nx),SOR%dy2(Ny),SOR%dz2(Nz))
          call getDxcc(gd,SOR%dx1)
          call getDycc(gd,SOR%dy1)
          call getDzcc(gd,SOR%dz1)
          call getDxn(gd,SOR%dx2)
          call getDyn(gd,SOR%dy2)
          call getDzn(gd,SOR%dz2)
        case (2) ! Node based (for magnetic field)
          Nx = SOR%s(1)-1; Ny = SOR%s(2)-1; Nz = SOR%s(3)-1
          allocate(SOR%dx2(Nx+1),SOR%dy2(Ny+1),SOR%dz2(Nz+1))
          allocate(SOR%dx1(Nx),SOR%dy1(Ny),SOR%dz1(Nz))
          call getDxcc(gd,SOR%dx2)
          call getDycc(gd,SOR%dy2)
          call getDzcc(gd,SOR%dz2)
          call getDxn(gd,SOR%dx1)
          call getDyn(gd,SOR%dy1)
          call getDzn(gd,SOR%dz1)
        case default
          write(*,*) 'gridType in SOR must be 1 or 2. Terminating';stop
        end select
        allocate(SOR%lapu(SOR%s(1),SOR%s(2),SOR%s(3)))

        if (useGaussSeidel) then
          SOR%omega = 1.0
          SOR%name = 'GS '
        else
          SOR%omega = 2.0/(one + sqrt(one - & 
           ((cos(PI/dble(Nx+1)) + cos(PI/dble(Ny+1)) + cos(PI/dble(Nz+1)))/3.0)**2.0))
          SOR%name = 'SOR'
        endif
      end subroutine

      subroutine deleteSOR(SOR)
        implicit none
        type(mySOR),intent(inout) :: SOR
        deallocate(SOR%dx1)
        deallocate(SOR%dy1)
        deallocate(SOR%dz1)
        deallocate(SOR%dx2)
        deallocate(SOR%dy2)
        deallocate(SOR%dz2)
        deallocate(SOR%lapu)

        ! write(*,*) 'SOR object deleted'
      end subroutine


      subroutine solveSOR(SOR,u,f,u_bcs,gd,ss,err,gridType,displayTF)
        implicit none
        type(mySOR),intent(inout) :: SOR
        real(dpn),dimension(:,:,:),intent(inout) :: u
        real(dpn),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(griddata),intent(in) :: gd
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: err
        integer,intent(in) :: gridType
        logical,intent(in) :: displayTF
        ! Locals
        integer,dimension(3) :: s
        integer :: i,j,k,ijk
        real(dpn) :: r
        logical :: TF,continueLoop,TF_allDirichlet
        integer :: maxIterations

        call init(SOR,f,gd,gridType)

        s = shape(f)

        TF_allDirichlet = getAllDirichlet(u_bcs)
        call solverSettingsSet(ss)
        ijk = 0

        ! Boundaries
        call applyAllBCs(u_bcs,u,gd)

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
                r = one/SOR%dx2(i-1)*(one/SOR%dx1(i) + one/SOR%dx1(i-1)) + & 
                    one/SOR%dy2(j-1)*(one/SOR%dy1(j) + one/SOR%dy1(j-1)) + & 
                    one/SOR%dz2(k-1)*(one/SOR%dz1(k) + one/SOR%dz1(k-1))

                u(i,j,k) = u(i,j,k)*(one-SOR%omega) + &

               SOR%omega*( u(i-1,j,k)/(SOR%dx1(i-1) * SOR%dx2(i-1)) + &
                           u(i+1,j,k)/(SOR%dx1( i ) * SOR%dx2(i-1)) + &
                           u(i,j-1,k)/(SOR%dy1(j-1) * SOR%dy2(j-1)) + &
                           u(i,j+1,k)/(SOR%dy1( j ) * SOR%dy2(j-1)) + &
                           u(i,j,k-1)/(SOR%dz1(k-1) * SOR%dz2(k-1)) + &
                           u(i,j,k+1)/(SOR%dz1( k ) * SOR%dz2(k-1)) &
                         - f(i,j,k) )/r
              enddo
            enddo
          enddo
          !$OMP END DO

          !$OMP DO
          do k=3,s(3)-1,2
            do j=2,s(2)-1
              do i=2,s(1)-1
                r = one/SOR%dx2(i-1)*(one/SOR%dx1(i) + one/SOR%dx1(i-1)) + & 
                    one/SOR%dy2(j-1)*(one/SOR%dy1(j) + one/SOR%dy1(j-1)) + & 
                    one/SOR%dz2(k-1)*(one/SOR%dz1(k) + one/SOR%dz1(k-1))

                u(i,j,k) = u(i,j,k)*(one-SOR%omega) + &

               SOR%omega*( u(i-1,j,k)/(SOR%dx1(i-1) * SOR%dx2(i-1)) + &
                           u(i+1,j,k)/(SOR%dx1( i ) * SOR%dx2(i-1)) + &
                           u(i,j-1,k)/(SOR%dy1(j-1) * SOR%dy2(j-1)) + &
                           u(i,j+1,k)/(SOR%dy1( j ) * SOR%dy2(j-1)) + &
                           u(i,j,k-1)/(SOR%dz1(k-1) * SOR%dz2(k-1)) + &
                           u(i,j,k+1)/(SOR%dz1( k ) * SOR%dz2(k-1)) &
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
                r = one/SOR%dx2(i-1)*(one/SOR%dx1(i) + one/SOR%dx1(i-1)) + & 
                    one/SOR%dy2(j-1)*(one/SOR%dy1(j) + one/SOR%dy1(j-1)) + & 
                    one/SOR%dz2(k-1)*(one/SOR%dz1(k) + one/SOR%dz1(k-1))

                u(i,j,k) = u(i,j,k)*(one-SOR%omega) + &

               SOR%omega*( u(i-1,j,k)/(SOR%dx1(i-1) * SOR%dx2(i-1)) + &
                           u(i+1,j,k)/(SOR%dx1( i ) * SOR%dx2(i-1)) + &
                           u(i,j-1,k)/(SOR%dy1(j-1) * SOR%dy2(j-1)) + &
                           u(i,j+1,k)/(SOR%dy1( j ) * SOR%dy2(j-1)) + &
                           u(i,j,k-1)/(SOR%dz1(k-1) * SOR%dz2(k-1)) + &
                           u(i,j,k+1)/(SOR%dz1( k ) * SOR%dz2(k-1)) &
                         - f(i,j,k) )/r
              enddo
            enddo
          enddo
#endif

          if (.not.TF_allDirichlet) then
            call applyAllBCs(u_bcs,u,gd)
          endif

          if (getMinToleranceTF(ss).and.(ijk.eq.2)) then
            select case (SOR%gridType)
            case (1); call myCC2CCLap(SOR%lapu,u,gd)
            case (2); call myNodeLap(SOR%lapu,u,gd)
            end select
            call computeError(err,f(2:s(1)-1,2:s(2)-1,2:s(3)-1),SOR%lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
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
          call applyAllBCs(u_bcs,u,gd)
        endif

        if (displayTF) then
          write(*,*) 'SOR parameter = ',SOR%omega
          write(*,*) '(Final,max) '//SOR%name//' iteration = ',ijk,maxIterations

          select case (SOR%gridType)
          case (1); call myCC2CCLap(SOR%lapu,u,gd)
          case (2); call myNodeLap(SOR%lapu,u,gd)
          end select
          call computeError(err,f(2:s(1)-1,2:s(2)-1,2:s(3)-1),SOR%lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
          call printMyError(err,SOR%name//' Residuals for '//trim(adjustl(getName(ss))))
        endif

        call delete(SOR)
      end subroutine


      end module