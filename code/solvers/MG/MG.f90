      module MG_mod
      ! call MGSolver(u,f,u_bcs,m,ss,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), griddata (m)
      ! and solver settings (ss) using the MultiGrid (MG) method
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     m            = contains mesh information (dhc,dhn)
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
      use mesh_mod
      use SF_mod
      use BCs_mod
      use solverSettings_mod
      use apply_BCs_mod
      use norms_mod
      use MG_tools_mod
      use ops_discrete_mod
      use ops_aux_mod
      use IO_tools_mod
      use IO_SF_mod
      use SOR_mod
      ! use jacobi_mod
      implicit none

      private

      public :: multiGrid,init,delete,solve
      public :: setIterationsPerLevel,setIterationsAtMaxLevel
      public :: testRP

      logical,parameter :: exportMGGrids = .true.

      type multiGrid
        private
        type(SF) :: u,f,lapU,res,e
        type(SF) :: temp_rx  ! (or temp_pzy), temp fields for restriction / prolongation
        type(SF) :: temp_rxy ! (or temp_pz) , temp fields for restriction / prolongation
        type(mesh) :: m,m_rx,m_rxy
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

      subroutine initMultiGrid(mg,u,m_base,ss,displayTF)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m_base
        type(solverSettings),intent(in) :: ss
        logical,intent(in) :: displayTF
        integer,dimension(3) :: s
        integer :: i,j,nLevels

        nLevels = size(mg)
        mg(:)%nLevels = size(mg)
        ! mg(:)%displayTF = displayTF
        mg(:)%displayTF = displayTF
        s = u%RF(1)%s

        ! ******************** Check size of data ********************
        if (u%is_Node) then
        elseif (u%is_CC) then
        else; stop 'Error: MG only supports N or CC data in MG.f90'
        endif

        ! ******************** Initialize grids ********************
        call init(mg(1)%m,m_base)
        do i = 1,mg(1)%nLevels-1
          call restrict(mg(i+1)%m,mg(i)%m)
        enddo
        write(*,*) 'Multigrid levels:'
        do j = 1,mg(1)%nLevels
          write(*,*) 'N_cells of grid 1 level ',j,' = ',(/(mg(j)%m%g(1)%c(i)%sc,i=1,3)/)-2
        enddo

        ! ******************** Initialize fields ********************
        do j = 1,mg(1)%nLevels
          if (u%is_Node) then
            call init_node(mg(j)%u,mg(j)%m)
            call init_node(mg(j)%f,mg(j)%m)
            call init_node(mg(j)%lapU,mg(j)%m)
            call init_node(mg(j)%res,mg(j)%m)
            call init_node(mg(j)%e,mg(j)%m)
          elseif (u%is_CC) then
            call init_CC(mg(j)%u,mg(j)%m)
            call init_CC(mg(j)%f,mg(j)%m)
            call init_CC(mg(j)%lapU,mg(j)%m)
            call init_CC(mg(j)%res,mg(j)%m)
            call init_CC(mg(j)%e,mg(j)%m)
          endif

          call assign(mg(j)%u,0.0_cp)
          call assign(mg(j)%f,0.0_cp)
          call assign(mg(j)%lapU,0.0_cp)
          call assign(mg(j)%res,0.0_cp)
          call assign(mg(j)%e,0.0_cp)
        enddo

        ! ******************** Initialize intermediate fields ********************
        ! THIS NEEDS TO BE FIXED: need to use intermediate fields for each
        ! Maybe call them 
        !          temp_rpx (restricted/prolongated in x)
        !          temp_rpy (restricted/prolongated in y)
        ! 
        ! Need to choose a convention, which mesh do these transition fields live?
        ! 
        call init(mg(1)%m_rx,m_base)
        call init(mg(1)%m_rxy,m_base)
        do i = 1,mg(1)%nLevels
          call restrict_x(mg(i)%m_rx,mg(i)%m)
          call restrict_xy(mg(i)%m_rxy,mg(i)%m)
        enddo
        do j = 1,mg(1)%nLevels
          if (u%is_Node) then
            call init_Node(mg(j)%temp_rx,mg(j)%m_rx)
            call init_Node(mg(j)%temp_rxy,mg(j)%m_rxy)
          elseif (u%is_CC) then
            call init_CC(mg(j)%temp_rx,mg(j)%m_rx)
            call init_CC(mg(j)%temp_rxy,mg(j)%m_rxy)
          endif
          call assign(mg(j)%temp_rx,0.0_cp)
          call assign(mg(j)%temp_rxy,0.0_cp)
        enddo
        do i = 1,mg(1)%nLevels
          call delete(mg(i)%m_rx)
          call delete(mg(i)%m_rxy)
        enddo

        ! ******************** Initialize norm/ss ********************
        do j = 1,mg(1)%nLevels
          call init(mg(j)%norm)
          call init(mg(j)%ss)
        enddo

        ! ******************** Initialize BCs *************************
        mg(1)%MG_init = .true.

        call setIterationsPerLevel(mg,5)
        call setIterationsAtMaxLevel(mg,100)
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
        call delete(mg%u)
        call delete(mg%f)
        call delete(mg%lapU)
        call delete(mg%res)
        call delete(mg%e)
        call delete(mg%m)
        call delete(mg%m_rx)
        call delete(mg%m_rxy)
        call delete(mg%temp_rx)
        call delete(mg%temp_rxy)
      end subroutine

      subroutine testRP(mg,f,dir)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        type(SF),intent(in) :: f
        character(len=*),intent(in) :: dir
        integer :: i,nLevels
        nLevels = mg(1)%nLevels
        call assign(mg(1)%f,f)
        call export_3D_1C(mg(1)%m,mg(1)%f,dir,'f_Grid1',1)
        do i = 1,mg(1)%nLevels-1
          call restrict(mg(i+1)%f,mg(i)%f,mg(i)%m,mg(i)%temp_rx,mg(i)%temp_rxy)
          call export_3D_1C(mg(i+1)%m,mg(i+1)%f,dir,'f_Grid'//int2str(i+1),1)
          write(*,*) 'Finished restricting multigrid level ',i
        enddo
        write(*,*) '         Finished restricting'

        mg(nLevels)%u = mg(nLevels)%f
        write(*,*) '         Assigned coarsest level'
        call export_3D_1C(mg(nLevels)%m,mg(nLevels)%f,dir,'u_Grid'//int2str(nLevels),1)

        do i = mg(1)%nLevels, 2,-1
          call prolongate(mg(i-1)%u,mg(i)%u,mg(i-1)%m,mg(i-1)%temp_rxy,mg(i-1)%temp_rx)
          call export_3D_1C(mg(i-1)%m,mg(i-1)%f,dir,'u_Grid'//int2str(i-1),1)
          write(*,*) 'Finished prolongating multigrid level ',i
        enddo
        write(*,*) '         Finished prolongating'
      end subroutine

      subroutine solveMultiGrid(mg,u,f,m,ss,norm,displayTF)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        type(solverSettings),intent(inout) :: ss
        type(norms),intent(inout) :: norm
        type(SORSolver) :: SOR
        logical,intent(in) :: displayTF
        logical :: continueLoop,L
        integer :: i_MG,maxIterations,nLevels
#ifdef _EXPORT_MG_CONVERGENCE_
        integer :: NU
#endif

        nLevels = size(mg)

        call assign(mg(1)%f,f)
        call assign(mg(1)%u,u)

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          L = (maxIterations.ge.1)
        else; L = .true.
        endif; continueLoop = .true.

#ifdef _EXPORT_MG_CONVERGENCE_
        NU = new_and_open('out\','norm_MG')
#endif

        i_MG = 0
        do while (continueLoop.and.L)

          call setIteration(ss,i_MG)

          ! 1) Smooth on finest level
          ! Improvement on efficiency: Pass back residual from smoother:
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%m,&
            mg(1)%ss,mg(1)%norm,mg(1)%displayTF)

          ! 2) Get residual on finest level
          call lap(mg(1)%lapU,mg(1)%u,mg(1)%m)
          call subtract(mg(1)%res,mg(1)%f,mg(1)%lapU)

          ! Zero boundary values
          call zeroGhostPoints(mg(1)%res)

          ! 3) Begin decending into coarser grids, starting at level 2
          ! V-Cycle: Given whatever is needed, find, "exactly" the error
          ! on level 2.
          call Vcycle(mg,1)

          ! 4) Prolong correction back to mesh level 1
          call prolongate(mg(1)%e,mg(2)%e,mg(1)%m,mg(1)%temp_rxy,mg(1)%temp_rx)
          call add(mg(1)%u,mg(1)%e)

          ! 5) Final smoothing sweeps
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%m,&
            mg(1)%ss,mg(1)%norm,mg(1)%displayTF)

#ifdef _EXPORT_MG_CONVERGENCE_
            call lap(mg(1)%lapu,mg(1)%u,mg(1)%m)
            call subtract(mg(1)%res,mg(1)%lapu,mg(1)%f)
            call zeroGhostPoints(mg(1)%res)
            call compute(norm,mg(1)%res,mg(1)%m)
            write(NU,*) norm%L1,norm%L2,norm%Linf
#endif

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          ! continueLoop = .false.
          if (.not.continueLoop) exit
          ! ************************************************************************************
          i_MG = i_MG + 1
        enddo
        call assign(u,mg(1)%u)
        call init(norm,mg(1)%norm)

#ifdef _EXPORT_MG_CONVERGENCE_
        close(NU)
#endif

        if (displayTF) then
          write(*,*) 'Number of V-Cycles = ',i_MG

          call lap(mg(1)%lapu,u,m)
          call subtract(mg(1)%res,mg(1)%lapu,mg(1)%f)
          call zeroGhostPoints(mg(1)%res)
          call compute(norm,mg(1)%res,mg(1)%m)
          call print(norm,'MG Residuals for '//trim(adjustl(getName(ss))))
        endif
      end subroutine

      recursive subroutine Vcycle(mg,j)
        implicit none
        type(multigrid),dimension(:),intent(inout) :: mg
        integer,intent(in) :: j
        type(SORSolver) :: SOR
        ! Locals
        integer :: nLevels

        nLevels = size(mg)

        ! 1) Restrict the residual onto the coarse mesh
        !    and use it as the source term of the error
        call restrict(mg(j+1)%res,mg(j)%res,mg(j)%m,mg(j)%temp_rx,mg(j)%temp_rxy)
        mg(j+1)%f = mg(j+1)%res

        if (j+1 .lt. nLevels) then
          ! j+1 is not the last level, so we are certain to have
          ! access to j+1 AND j+2

          ! Have not reached coarsest level, prepare to
          ! solve for error, and restrict residual to coarse
          ! mesh

          ! 2) Smooth
          call solve(SOR,mg(j+1)%u,mg(j+1)%f,mg(j+1)%m,&
            mg(j+1)%ss,mg(j+1)%norm,mg(j+1)%displayTF)

          ! 3) Get residual
          call lap(mg(j+1)%lapU,mg(j+1)%u,mg(j+1)%m)
          call subtract(mg(j+1)%res,mg(j+1)%f,mg(j+1)%lapU)
          ! Zero boundary values
          call zeroGhostPoints(mg(j+1)%res)

          ! 4) Decend to coarser level
          call Vcycle(mg,j+1)

          ! Finished Vcycle decent. Prepare to ascend back to level 1
          ! 5) Prolongate the error

          call prolongate(mg(j+1)%e,mg(j+2)%e,mg(j+1)%m,mg(j+1)%temp_rxy,mg(j+1)%temp_rx)
          call add(mg(j+1)%u,mg(j+1)%e)

          ! 6) Final smoothing sweeps
          call solve(SOR,mg(j+1)%u,mg(j+1)%f,mg(j+1)%m,&
            mg(j+1)%ss,mg(j+1)%norm,mg(j+1)%displayTF)
          ! The solution on any mesh above the 
          ! base mesh is the error!
          call assign(mg(j+1)%e,mg(j+1)%u)

        else ! At coarsest level. Solve exactly.

          ! call setMaxIterations(mg(j+1)%ss,5) ! Fixed
          ! call setMaxIterations(mg(j+1)%ss,floor(exp(real(mg(j+1)%nlevels,cp)))) ! Dynamic
          call solve(SOR,mg(j+1)%u,mg(j+1)%f,mg(j+1)%m,&
            mg(j+1)%ss,mg(j+1)%norm,mg(j+1)%displayTF)

          ! The solution on any mesh above the 
          ! base mesh is the correction on the 
          ! finer mesh!
          call assign(mg(j+1)%e,mg(j+1)%u)
        endif
      end subroutine

      end module