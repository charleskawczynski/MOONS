      module MG_mod
      ! call MGSolver(u,f,u_bcs,g,ss,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), griddata (g)
      ! and solver settings (ss) using the MultiGrid (MG) method
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     g            = contains grid information (dhc,dhn)
      !     ss           = solver settings (specifies max iterations, tolerance etc.)
      !     displayTF    = print residuals to screen (T,F)
      ! 
      ! 
      ! There are 3 iteration numbers that must be set:
      !      1) Number of V-Cycles - defined by maxIterations in ss
      !      2) Iterations per cycle - call setIterationsPerLevel()
      !      3) Iterations at the coarsest level - call setIterationsAtMaxLevel()
      use current_precision_mod
      use coordinates_mod
      use grid_mod
      use BCs_mod
      use solverSettings_mod
      use applyBCs_mod
      use norms_mod
      use MG_tools_mod
      use ops_discrete_mod
      use ops_aux_mod
      use IO_tools_mod
      use IO_scalarFields_mod
      use SOR_mod
      use jacobi_mod
      implicit none

      private

      public :: multiGrid,init,delete,solve
      public :: setIterationsPerLevel,setIterationsAtMaxLevel
      public :: testRP

      logical,parameter :: exportMGGrids = .true.

      type multiGrid
        private
        real(cp),dimension(:,:,:),allocatable :: u,f,lapU,res,e
        type(BCs) :: u_bcs
        type(grid) :: g
        type(norms) :: norm
        type(solverSettings) :: ss
        integer :: nLevels
        logical :: displayTF,MG_init
      end type

      interface init;       module procedure initMultiGrid;    end interface
      interface solve;      module procedure solveMultiGrid;   end interface

      interface delete;     module procedure deleteMG;         end interface
      interface delete;     module procedure deleteAllMG;      end interface

      contains

      subroutine initMultiGrid(mg,s,u_bcs,g_base,ss,displayTF)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        integer,dimension(3),intent(in) :: s
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g_base
        type(solverSettings),intent(in) :: ss
        logical,intent(in) :: displayTF
        ! character(len=*),intent(in) :: dir
        integer,dimension(3) :: N
        integer :: i,j,nLevels,bctype

        nLevels = size(mg)
        mg(:)%nLevels = size(mg)
        ! mg(:)%displayTF = displayTF
        mg(:)%displayTF = .false.

        ! ******************** Check size of data ********************
        if (all((/(s(i).eq.g_base%c(i)%sn,i=1,3)/))) then
        elseif (all((/(s(i).eq.g_base%c(i)%sc,i=1,3)/))) then
        else; stop 'Error: MG only supports N or CC data in MGSolver.f90'
        endif

        ! ******************** Initialize grids ********************
        call init(mg(1)%g,g_base)
        do i = 1,mg(1)%nLevels-1
          call restrict(mg(i+1)%g,mg(i)%g)
        enddo
        ! write(*,*) 'Multigrid levels:'
        ! do j = 1,mg(1)%nLevels
        !   write(*,*) 'N_cells of level ',j,' = ',(/(mg(j)%g%c(i)%sc,i=1,3)/)-2
        ! enddo

        ! ******************** Initialize fields ********************
        do j = 1,mg(1)%nLevels
          if (s(1).eq.g_base%c(1)%sn) then
            N = (/(mg(j)%g%c(i)%sn,i=1,3)/)
          elseif (s(1).eq.g_base%c(1)%sc) then
            N = (/(mg(j)%g%c(i)%sc,i=1,3)/)
          endif

          if (allocated(mg(j)%u))    deallocate(mg(j)%u)
          if (allocated(mg(j)%f))    deallocate(mg(j)%f)
          if (allocated(mg(j)%lapU)) deallocate(mg(j)%lapU)
          if (allocated(mg(j)%res))  deallocate(mg(j)%res)
          if (allocated(mg(j)%e))    deallocate(mg(j)%e)
          allocate(mg(j)%u(N(1),N(2),N(3)))
          allocate(mg(j)%f(N(1),N(2),N(3)))
          allocate(mg(j)%lapU(N(1),N(2),N(3)))
          allocate(mg(j)%res(N(1),N(2),N(3)))
          allocate(mg(j)%e(N(1),N(2),N(3)))

          mg(j)%u    = real(0.0,cp)
          mg(j)%f    = real(0.0,cp)
          mg(j)%lapU = real(0.0,cp)
          mg(j)%res  = real(0.0,cp)
          mg(j)%e    = real(0.0,cp)
        enddo

        ! ******************** Initialize norm/ss ********************
        do j = 1,mg(1)%nLevels
          call init(mg(j)%norm)
          call init(mg(j)%ss)
        enddo

        ! ******************** Initialize BCs *************************
        call init(mg(1)%u_bcs,u_bcs)
        do j = 2,mg(1)%nLevels

          if (s(1).eq.g_base%c(1)%sn) then
            N = (/(mg(j)%g%c(i)%sn,i=1,3)/)
          elseif (s(1).eq.g_base%c(1)%sc) then
            N = (/(mg(j)%g%c(i)%sc,i=1,3)/)
          endif

          ! RIGHT NOW ONLY HANDLES ZERO DIRICHLET AND ZERO NEUAMNN
          ! FACE/EDGE-DATA IS NOT SUPPORTED AND CANNOT BE SIMPLY 
          ! SINCE THE NUMBER OF DATA POINTS DIFFERS BY 1 BETWEEN
          ! EACH DIRECTION FOR STAGGERED DATA.
          if ((s(1).eq.g_base%c(1)%sn).and.(getAllNeumann(mg(1)%u_bcs))) bctype = 4       ! Neumann wall coincident
          if ((s(1).eq.g_base%c(1)%sc).and.(getAllNeumann(mg(1)%u_bcs))) bctype = 5       ! Neumann wall incoincident
          if ((s(1).eq.g_base%c(1)%sn).and.(.not.getAllNeumann(mg(1)%u_bcs))) bctype = 1  ! Dirichlet wall coincident
          if ((s(1).eq.g_base%c(1)%sc).and.(.not.getAllNeumann(mg(1)%u_bcs))) bctype = 2  ! Dirichlet wall incoincident
          call setAllZero(mg(j)%u_bcs,N(1),N(2),N(3),bctype)

        enddo
        mg(1)%MG_init = .true.

        call setIterationsPerLevel(mg,5)
        ! call setIterationsAtMaxLevel(mg,100)
        ! call setIterationsAtMaxLevel(mg,getMaxIterations(ss))

        ! call testRP(mg,'out\')
        ! write(*,*) 'Initialized MG'
      end subroutine

      subroutine setIterationsPerLevel(mg,n)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        integer,intent(in) :: n
        integer :: j
        do j = 1,mg(1)%nLevels
          call setMaxIterations(mg(j)%ss,n)
          call setName(mg(j)%ss,'MG level('//int2str(j)//')         ')
        enddo
      end subroutine

      subroutine setIterationsAtMaxLevel(mg,n)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        integer,intent(in) :: n
        call setMaxIterations(mg(size(mg))%ss,n)
        call setName(mg(size(mg))%ss,'MG level('//int2str(size(mg))//')         ')
      end subroutine

      subroutine deleteAllMG(mg)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        integer :: j
        do j = 1,size(mg)
          call delete(mg(j))
        enddo
      end subroutine

      subroutine deleteMG(mg)
        implicit none
        type(multiGrid),intent(inout) :: mg
        if (allocated(mg%u))    deallocate(mg%u)
        if (allocated(mg%f))    deallocate(mg%f)
        if (allocated(mg%lapU)) deallocate(mg%lapU)
        if (allocated(mg%res))  deallocate(mg%res)
        if (allocated(mg%e))    deallocate(mg%e)
        call delete(mg%g)
        call delete(mg%u_bcs)
      end subroutine

      subroutine testRP(mg,f,dir)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        real(cp),dimension(:,:,:),intent(in) :: f
        character(len=*),intent(in) :: dir
        integer :: i,nLevels
        integer,dimension(3) :: s
        nLevels = mg(1)%nLevels
        mg(1)%f = f
        s = shape(mg(1)%f)
        call writeScalarPhysical(mg(1)%g,mg(1)%f,dir,'f_Grid1')
        do i = 1,mg(1)%nLevels-1
          ! write(*,*) 'From ',shape(mg(i)%f), ' to ',shape(mg(i+1)%f)
          call restrict(mg(i+1)%f,mg(i)%f,mg(i)%g)
          s = shape(mg(i+1)%f)
          call writeScalarPhysical(mg(i+1)%g,mg(i+1)%f,dir,'f_Grid'//int2str(i+1))
          write(*,*) 'Finished restricting multigrid level ',i
        enddo
        write(*,*) '         Finished restricting'

        mg(nLevels)%u = mg(nLevels)%f
        write(*,*) '         Assigned coarsest level'
        call writeScalarPhysical(mg(nLevels)%g,mg(nLevels)%f,dir,'u_Grid'//int2str(nLevels))

        do i = mg(1)%nLevels, 2,-1
          ! write(*,*) 'From ',shape(mg(i)%u), ' to ',shape(mg(i-1)%u)
          call prolongate(mg(i-1)%u,mg(i)%u,mg(i-1)%g)
          s = shape(mg(i-1)%u)
          call writeScalarPhysical(mg(i-1)%g,mg(i-1)%f,dir,'u_Grid'//int2str(i-1))
          write(*,*) 'Finished prolongating multigrid level ',i
        enddo
        write(*,*) '         Finished prolongating'
      end subroutine

      subroutine solveMultiGrid(mg,u,f,u_bcs,g,ss,norm,displayTF)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(norms),intent(inout) :: norm
        type(jacobi) :: SOR
        logical,intent(in) :: displayTF
        integer,dimension(3) :: s
        logical :: continueLoop,TF
        integer :: i_MG,maxIterations,nLevels
#ifdef _EXPORT_MG_CONVERGENCE_
        integer :: NU
#endif

        nLevels = size(mg)

        mg(1)%f = f
        mg(1)%u = u

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif; continueLoop = .true.

#ifdef _EXPORT_MG_CONVERGENCE_
        NU = newAndOpen('out\','norm_MG')
#endif

        i_MG = 0
        do while (continueLoop.and.TF)

          call setIteration(ss,i_MG)

          ! 1) Smooth on finest level
          ! Improvement on efficiency: Pass back residual from smoother:
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%u_bcs,mg(1)%g,&
            mg(1)%ss,mg(1)%norm,mg(1)%displayTF)

          ! 2) Get residual on finest level
          call lap(mg(1)%lapU,mg(1)%u,mg(1)%g)
          mg(1)%res = mg(1)%f - mg(1)%lapU

          s = shape(mg(1)%res)
          ! Zero boundary values
          call zeroGhostPoints(mg(1)%res)

          ! 3) Begin decending into coarser grids, starting at level 2
          ! V-Cycle: Given whatever is needed, compute, "exactly" the error
          ! on level 2.
          call Vcycle(mg,1)

          ! 4) Prolong correction back to grid level 1
          call prolongate(mg(1)%e,mg(2)%e,mg(1)%g)
          mg(1)%u = mg(1)%u + mg(1)%e

          ! 5) Final smoothing sweeps
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%u_bcs,mg(1)%g,&
            mg(1)%ss,mg(1)%norm,mg(1)%displayTF)

#ifdef _EXPORT_MG_CONVERGENCE_
            call lap(mg(1)%lapu,mg(1)%u,mg(1)%g)
            mg(1)%res = mg(1)%lapu - mg(1)%f
            call zeroGhostPoints(mg(1)%res)
            call compute(norm,real(0.0,cp),mg(1)%res)
            write(NU,*) getL1(norm),getL2(norm),getLinf(norm)
#endif

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          ! continueLoop = .false.
          if (.not.continueLoop) exit
          ! ************************************************************************************
          i_MG = i_MG + 1
        enddo
        u = mg(1)%u
        norm = mg(1)%norm

#ifdef _EXPORT_MG_CONVERGENCE_
        close(NU)
#endif

        if (displayTF) then
          write(*,*) 'Number of V-Cycles = ',i_MG

          call lap(mg(1)%lapu,u,g)
          mg(1)%res = mg(1)%lapu - mg(1)%f
          call zeroGhostPoints(mg(1)%res)
          call compute(norm,real(0.0,cp),mg(1)%res)
          call print(norm,'MG Residuals for '//trim(adjustl(getName(ss))))
        endif
      end subroutine

      recursive subroutine Vcycle(mg,j)
        implicit none
        type(multigrid),dimension(:),intent(inout) :: mg
        integer,intent(in) :: j
        type(jacobi) :: SOR
        ! Locals
        integer :: nLevels
        integer,dimension(3) :: s

        nLevels = size(mg)

        ! 1) Restrict the residual onto the coarse grid
        !    and use it as the source term of the error
        call restrict(mg(j+1)%res,mg(j)%res,mg(j)%g)
        mg(j+1)%f = mg(j+1)%res

        if (j+1 .lt. nLevels) then
          ! j+1 is not the last level, so we are certain to have
          ! access to j+1 AND j+2

          ! Have not reached coarsest level, prepare to
          ! solve for error, and restrict residual to coarse
          ! grid

          ! 2) Smooth
          call solve(SOR,mg(j+1)%u,mg(j+1)%f,mg(j+1)%u_bcs,mg(j+1)%g,&
            mg(j+1)%ss,mg(j+1)%norm,mg(j+1)%displayTF)

          ! 3) Get residual
          call lap(mg(j+1)%lapU,mg(j+1)%u,mg(j+1)%g)
          mg(j+1)%res = mg(j+1)%f - mg(j+1)%lapU
          ! Zero boundary values
          s = shape(mg(j+1)%res)
          call zeroGhostPoints(mg(j+1)%res)

          ! 4) Decend to coarser level
          call Vcycle(mg,j+1)

          ! Finished Vcycle decent. Prepare to ascend back to level 1
          ! 5) Prolongate the error

          call prolongate(mg(j+1)%e,mg(j+2)%e,mg(j+1)%g)
          mg(j+1)%u = mg(j+1)%u + mg(j+1)%e

          ! 6) Final smoothing sweeps
          call solve(SOR,mg(j+1)%u,mg(j+1)%f,mg(j+1)%u_bcs,mg(j+1)%g,&
            mg(j+1)%ss,mg(j+1)%norm,mg(j+1)%displayTF)
          ! The solution on any grid above the 
          ! base grid is the error!
          mg(j+1)%e = mg(j+1)%u

        else ! At coarsest level. Solve exactly.

          ! call setMaxIterations(mg(j+1)%ss,5) ! Fixed
          ! call setMaxIterations(mg(j+1)%ss,floor(exp(real(mg(j+1)%nlevels,cp)))) ! Dynamic
          call solve(SOR,mg(j+1)%u,mg(j+1)%f,mg(j+1)%u_bcs,mg(j+1)%g,&
            mg(j+1)%ss,mg(j+1)%norm,mg(j+1)%displayTF)

          ! The solution on any grid above the 
          ! base grid is the correction on the 
          ! finer grid!
          mg(j+1)%e = mg(j+1)%u
        endif
      end subroutine

      end module