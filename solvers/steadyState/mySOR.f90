      module mySOR_mod
      ! call mySOR(u,f,u_bcs,g,ss,gridType,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), grid (g)
      ! and solver settings (ss) using the iterative Successive Over 
      ! Realxation (SOR) method
      ! 
      ! Note that the variant of Gauss-Seidel/SOR called
      ! "red-black" Gauss-Seidel is used.
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

      ! Flags: (_PARALLELIZE_SOR_,_EXPORT_SOR_CONVERGENCE_)

      use grid_mod
      use BCs_mod
      use applyBCs_mod
      use myError_mod
      use delOps_mod
      use solverSettings_mod
#ifdef _EXPORT_SOR_CONVERGENCE_
      use myIO_mod
#endif
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
        real(cp),dimension(:,:,:),allocatable :: lapu,f ! f zeros mean
        real(cp) :: omega
        integer,dimension(3) :: s
        integer :: gt
      end type
      
      interface init;        module procedure initSOR;       end interface
      interface delete;      module procedure deleteSOR;     end interface
      interface solve;       module procedure solveSOR;      end interface

      contains

      subroutine initSOR(SOR,f,g)
        implicit none
        type(mySOR),intent(inout) :: SOR
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer :: Nx,Ny,Nz

        
        SOR%s = shape(f)

            if (SOR%s(1).eq.g%c(1)%sc) then; SOR%gridType = 1
        elseif (SOR%s(1).eq.g%c(1)%sn) then; SOR%gridType = 2
        else; stop 'Error: gridType was not determined in SOR.f90'
        endif

        select case (SOR%gridType)
        case (1) ! Cell based (for pressure)
          Nx = g%c(1)%sc-1; Ny = g%c(2)%sc-1; Nz = g%c(3)%sc-1   ! number of cells (excluding ghost)
          allocate(SOR%dxp(Nx),SOR%dyp(Ny),SOR%dzp(Nz))          ! Primary grid
          Nx = g%c(1)%sn-1; Ny = g%c(2)%sn-1; Nz = g%c(3)%sn-1   ! number of cells (excluding ghost)
          allocate(SOR%dxd(Nx),SOR%dyd(Ny),SOR%dzd(Nz))          ! Dual grid
          SOR%dxp = g%c(1)%dhc
          SOR%dyp = g%c(2)%dhc
          SOR%dzp = g%c(3)%dhc
          SOR%dxd = g%c(1)%dhn
          SOR%dyd = g%c(2)%dhn
          SOR%dzd = g%c(3)%dhn
          SOR%gt = 0
        case (2) ! Node based (for magnetic field)
          Nx = g%c(1)%sn-1; Ny = g%c(2)%sn-1; Nz = g%c(3)%sn-1   ! number of cells (excluding ghost)
          allocate(SOR%dxp(Nx),SOR%dyp(Ny),SOR%dzp(Nz))          ! Primary grid
          Nx = g%c(1)%sc-1; Ny = g%c(2)%sc-1; Nz = g%c(3)%sc-1   ! number of cells (excluding ghost)
          allocate(SOR%dxd(Nx),SOR%dyd(Ny),SOR%dzd(Nz))          ! Dual grid
          SOR%dxd = g%c(1)%dhc
          SOR%dyd = g%c(2)%dhc
          SOR%dzd = g%c(3)%dhc
          SOR%dxp = g%c(1)%dhn
          SOR%dyp = g%c(2)%dhn
          SOR%dzp = g%c(3)%dhn
          SOR%gt = 1
        case default
          write(*,*) 'gridType in SOR must be 1 or 2. Terminating';stop
        end select
        allocate(SOR%lapu(SOR%s(1),SOR%s(2),SOR%s(3)))
        allocate(SOR%f(SOR%s(1),SOR%s(2),SOR%s(3)))

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
        deallocate(SOR%f)

        ! write(*,*) 'SOR object deleted'
      end subroutine


      subroutine solveSOR(SOR,u,f,u_bcs,g,ss,norms,displayTF)
        implicit none
        type(mySOR),intent(inout) :: SOR
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: norms
        logical,intent(in) :: displayTF
        ! Locals
        integer,dimension(3) :: s
        integer :: i,j,k,ijk,gt
        real(cp) :: r
        logical :: TF,continueLoop,TF_allDirichlet
        integer :: maxIterations
#ifdef _EXPORT_SOR_CONVERGENCE_
        integer :: NU,n
#endif

        call init(SOR,f,g)

        s = shape(f); gt = SOR%gt

        TF_allDirichlet = getAllDirichlet(u_bcs)
        call solverSettingsSet(ss)
        ijk = 0

        ! Boundaries
        ! call applyAllGhost(u_bcs,u,g) ! Necessary with ghost nodes
        call applyAllBCs(u_bcs,u,g) ! Necessary with ghost nodes

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif
        continueLoop = .true.

        if (getSubtractMean(ss)) then
          SOR%f = f - sum(f)/(max(1,size(f)))
        else
          SOR%f = f
        endif

#ifdef _EXPORT_SOR_CONVERGENCE_
        open(NU,file=trim(strcompress('out\',n)) // trim(strcompress('norms_SOR',n)) // '.dat',pad='YES')
#endif

        do while (continueLoop.and.TF)
          ijk = ijk + 1
#ifdef _PARALLELIZE_SOR_
          !$OMP PARALLEL PRIVATE(r)
          !$OMP DO
#endif
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
                         - SOR%f(i,j,k) )/r
              enddo
            enddo
          enddo
#ifdef _PARALLELIZE_SOR_
          !$OMP END DO
          !$OMP DO
#endif
          do k=3,s(3)-1,2 ! 3 is correct (odd numbers)
            do j=2,s(2)-1
              do i=2,s(1)-1
                r = real(1.0,cp)/SOR%dxd(i-1+gt)*(real(1.0,cp)/SOR%dxp(i) + real(1.0,cp)/SOR%dxp(i-1)) + & 
                    real(1.0,cp)/SOR%dyd(j-1+gt)*(real(1.0,cp)/SOR%dyp(j) + real(1.0,cp)/SOR%dyp(j-1)) + & 
                    real(1.0,cp)/SOR%dzd(k-1+gt)*(real(1.0,cp)/SOR%dzp(k) + real(1.0,cp)/SOR%dzp(k-1))

                u(i,j,k) = u(i,j,k)*(real(1.0,cp)-SOR%omega) + &

               SOR%omega*( u(i-1,j,k)/(SOR%dxp(i-1) * SOR%dxd(i-1+gt)) + &
                           u(i+1,j,k)/(SOR%dxp( i ) * SOR%dxd(i-1+gt)) + &
                           u(i,j-1,k)/(SOR%dyp(j-1) * SOR%dyd(j-1+gt)) + &
                           u(i,j+1,k)/(SOR%dyp( j ) * SOR%dyd(j-1+gt)) + &
                           u(i,j,k-1)/(SOR%dzp(k-1) * SOR%dzd(k-1+gt)) + &
                           u(i,j,k+1)/(SOR%dzp( k ) * SOR%dzd(k-1+gt)) &
                         - SOR%f(i,j,k) )/r
              enddo
            enddo
          enddo
#ifdef _PARALLELIZE_SOR_
          !$OMP END DO
          !$OMP END PARALLEL
#endif

          ! if (.not.TF_allDirichlet) then
          !   call applyAllBCs(u_bcs,u,g)
          ! endif
          ! call applyAllGhost(u_bcs,u,g) ! Necessary with ghost nodes
          call applyAllBCs(u_bcs,u,g) ! Necessary with ghost nodes

          if (getMinToleranceTF(ss)) then
            select case (SOR%gridType)
            case (1); call CC2CCLap(SOR%lapu,u,g)
            case (2); call myNodeLap(SOR%lapu,u,g)
            end select
            call compute(norms,SOR%f(2:s(1)-1,2:s(2)-1,2:s(3)-1),SOR%lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
            call setTolerance(ss,getL2Rel(norms))
          endif

#ifdef _EXPORT_SOR_CONVERGENCE_
            select case (SOR%gridType)
            case (1); call CC2CCLap(SOR%lapu,u,g)
            case (2); call myNodeLap(SOR%lapu,u,g)
            end select
            call compute(norms,SOR%f(2:s(1)-1,2:s(2)-1,2:s(3)-1),SOR%lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
            write(NU,*) getL1(norms),getL2(norms),getLinf(norms)
#endif

          call setIteration(ss,ijk)

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          if (.not.continueLoop) exit
          ! ************************************************************************************
        enddo

#ifdef _EXPORT_SOR_CONVERGENCE_
        close(NU)
#endif
        
        ! Subtract mean (for Pressure Poisson)
        if (getSubtractMean(ss)) u = u - sum(u)/(max(1,size(u)))

        ! if (getAnyDirichlet(u_bcs)) then ! Obsolete with ghost nodes
        !   call applyAllBCs(u_bcs,u,g)
        ! endif

        if (displayTF) then
          write(*,*) 'SOR parameter = ',SOR%omega
          write(*,*) '(Final,max) '//SOR%name//' iteration = ',ijk,maxIterations

          select case (SOR%gridType)
          case (1); call CC2CCLap(SOR%lapu,u,g)
          case (2); call myNodeLap(SOR%lapu,u,g)
          end select
          call compute(norms,SOR%f(2:s(1)-1,2:s(2)-1,2:s(3)-1),SOR%lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
          call print(norms,SOR%name//' Residuals for '//trim(adjustl(getName(ss))))
        endif

        call delete(SOR)
      end subroutine
      end module