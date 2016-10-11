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
      !     u_bcs        = boundary conditions for u. Refer to boundary_conditions_mod for more info.
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
      use SF_mod
      use VF_mod
      use boundary_conditions_mod
      use solverSettings_mod
      use applyBCs_mod
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
        type(VF) :: sigma
        type(grid) :: g,g_rx,g_rxy
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

      subroutine initMultiGrid(mg,u,sigma,g_base,ss,displayTF)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        type(SF),intent(in) :: u
        type(VF),intent(in) :: sigma
        type(grid),intent(in) :: g_base
        type(solverSettings),intent(in) :: ss
        logical,intent(in) :: displayTF
        type(SF) :: temp_sigma
        integer,dimension(3) :: s
        integer :: i,j,nLevels,bctype

        nLevels = size(mg)
        mg(:)%nLevels = size(mg)
        ! mg(:)%displayTF = displayTF
        mg(:)%displayTF = .false.
        s = u%BF(1)%GF%s

        ! ******************** Check size of data ********************
        if (all((/(s(i).eq.g_base%c(i)%sn,i=1,3)/))) then
        elseif (all((/(s(i).eq.g_base%c(i)%sc,i=1,3)/))) then
        else; stop 'Error: MG only supports N or CC data in MG.f90'
        endif

        ! ******************** Initialize grids ********************
        call init(mg(1)%g,g_base)
        call init(mg(1)%sigma,sigma)
        do i = 1,mg(1)%nLevels-1
          call restrict(mg(i+1)%g,mg(i)%g)
          call restrict(mg(i+1)%sigma,mg(i)%sigma,mg(i)%g_rx,mg(i)%g_rxy)
        enddo
        write(*,*) 'Multigrid levels:'
        do j = 1,mg(1)%nLevels
          write(*,*) 'N_cells of level ',j,' = ',(/(mg(j)%g%c(i)%sc,i=1,3)/)-2
        enddo

        ! ******************** Initialize fields ********************
        do j = 1,mg(1)%nLevels
          if (s(1).eq.g_base%c(1)%sn) then
            call init_node(mg(j)%u,mg(j)%g)
            call init_node(mg(j)%f,mg(j)%g)
            call init_node(mg(j)%lapU,mg(j)%g)
            call init_node(mg(j)%res,mg(j)%g)
            call init_node(mg(j)%e,mg(j)%g)
          elseif (s(1).eq.g_base%c(1)%sc) then
            call init_CC(mg(j)%u,mg(j)%g)
            call init_CC(mg(j)%f,mg(j)%g)
            call init_CC(mg(j)%lapU,mg(j)%g)
            call init_CC(mg(j)%res,mg(j)%g)
            call init_CC(mg(j)%e,mg(j)%g)
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
        ! Need to choose a convention, which grid do these transition fields live?
        ! 
        call init(mg(1)%g_rx,g_base)
        call init(mg(1)%g_rxy,g_base)
        do i = 1,mg(1)%nLevels
          call restrict_x(mg(i)%g_rx,mg(i)%g)
          call restrict_xy(mg(i)%g_rxy,mg(i)%g)
        enddo
        do j = 1,mg(1)%nLevels
          if (s(1).eq.g_base%c(1)%sn) then
            call init_Node(mg(j)%temp_rx,mg(j)%g_rx)
            call init_Node(mg(j)%temp_rxy,mg(j)%g_rxy)
          elseif (s(1).eq.g_base%c(1)%sc) then
            call init_CC(mg(j)%temp_rx,mg(j)%g_rx)
            call init_CC(mg(j)%temp_rxy,mg(j)%g_rxy)
          endif
          call assign(mg(j)%temp_rx,0.0_cp)
          call assign(mg(j)%temp_rxy,0.0_cp)
        enddo
        do i = 1,mg(1)%nLevels
          call delete(mg(i)%g_rx)
          call delete(mg(i)%g_rxy)
        enddo

        ! ******************** Initialize norm/ss ********************
        do j = 1,mg(1)%nLevels
          call init(mg(j)%norm)
          call init(mg(j)%ss)
        enddo

        ! ******************** Initialize BCs *************************
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
        call delete(mg%u)
        call delete(mg%sigma)
        call delete(mg%f)
        call delete(mg%lapU)
        call delete(mg%res)
        call delete(mg%e)
        call delete(mg%g)
        call delete(mg%g_rx)
        call delete(mg%g_rxy)
        call delete(mg%temp_rx)
        call delete(mg%temp_rxy)
      end subroutine

      subroutine compute_Au_MG(Au,u,sigma,g,temp)
        implicit none
        type(SF),intent(inout) :: Au
        type(SF),intent(inout) :: u
        type(grid),intent(in) :: g
        type(VF),intent(in) :: sigma
        type(VF),intent(inout) :: temp
        call grad(temp,u,g)
        call multiply(temp,sigma)
        call div(Au,temp,g)
      end subroutine

      subroutine testRP(mg,f,dir)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        type(SF),intent(in) :: f
        character(len=*),intent(in) :: dir
        integer :: i,nLevels
        nLevels = mg(1)%nLevels
        call assign(mg(1)%f,f)
        call export_1C_SF(mg(1)%g,mg(1)%f,dir,'f_Grid1',1)
        do i = 1,mg(1)%nLevels-1
          call restrict(mg(i+1)%f,mg(i)%f,mg(i)%g,mg(i)%temp_rx,mg(i)%temp_rxy)
          call export_1C_SF(mg(i+1)%g,mg(i+1)%f,dir,'f_Grid'//int2str(i+1),1)
          write(*,*) 'Finished restricting multigrid level ',i
        enddo
        write(*,*) '         Finished restricting'

        mg(nLevels)%u = mg(nLevels)%f
        write(*,*) '         Assigned coarsest level'
        call export_1C_SF(mg(nLevels)%g,mg(nLevels)%f,dir,'u_Grid'//int2str(nLevels),1)

        do i = mg(1)%nLevels, 2,-1
          call prolongate(mg(i-1)%u,mg(i)%u,mg(i-1)%g,mg(i-1)%temp_rxy,mg(i-1)%temp_rx)
          call export_1C_SF(mg(i-1)%g,mg(i-1)%f,dir,'u_Grid'//int2str(i-1),1)
          write(*,*) 'Finished prolongating multigrid level ',i
        enddo
        write(*,*) '         Finished prolongating'
      end subroutine

      subroutine solveMultiGrid(mg,u,f,u_bcs,g,ss,norm,displayTF)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
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
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%sigma,mg(1)%g,&
            mg(1)%ss,mg(1)%norm,mg(1)%displayTF)

          ! 2) Get residual on finest level
          call compute_Au(mg(1)%lapU,mg(1)%u,mg(1)%sigma,mg(1)%g)
          call subtract(mg(1)%res,mg(1)%f,mg(1)%lapU)

          ! Zero boundary values
          call zeroGhostPoints(mg(1)%res)

          ! 3) Begin decending into coarser grids, starting at level 2
          ! V-Cycle: Given whatever is needed, find, "exactly" the error
          ! on level 2.
          call Vcycle(mg,1)

          ! 4) Prolong correction back to grid level 1
          call prolongate(mg(1)%e,mg(2)%e,mg(1)%g,mg(1)%temp_rxy,mg(1)%temp_rx)
          call add(mg(1)%u,mg(1)%e)

          ! 5) Final smoothing sweeps
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%sigma,mg(1)%g,&
            mg(1)%ss,mg(1)%norm,mg(1)%displayTF)

#ifdef _EXPORT_MG_CONVERGENCE_
            call compute_Au(mg(1)%lapu,mg(1)%u,mg(1)%sigma,mg(1)%g)
            call subtract(mg(1)%res,mg(1)%lapu,mg(1)%f)
            call zeroGhostPoints(mg(1)%res)
            call compute(norm,mg(1)%res,mg(1)%g)
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

          call compute_Au(mg(1)%lapu,u,mg(1)%sigma,g)
          call subtract(mg(1)%res,mg(1)%lapu,mg(1)%f)
          call zeroGhostPoints(mg(1)%res)
          call compute(norm,mg(1)%res,mg(1)%g)
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

        ! 1) Restrict the residual onto the coarse grid
        !    and use it as the source term of the error
        call restrict(mg(j+1)%res,mg(j)%res,mg(j)%g,mg(j)%temp_rx,mg(j)%temp_rxy)
        mg(j+1)%f = mg(j+1)%res

        if (j+1 .lt. nLevels) then
          ! j+1 is not the last level, so we are certain to have
          ! access to j+1 AND j+2

          ! Have not reached coarsest level, prepare to
          ! solve for error, and restrict residual to coarse
          ! grid

          ! 2) Smooth
          call solve(SOR,mg(j+1)%u,mg(j+1)%f,mg(j+1)%sigma,mg(j+1)%g,&
            mg(j+1)%ss,mg(j+1)%norm,mg(j+1)%displayTF)

          ! 3) Get residual
          call compute_Au(mg(j+1)%lapU,mg(j+1)%u,mg(j+1)%sigma,mg(j+1)%g)
          call subtract(mg(j+1)%res,mg(j+1)%f,mg(j+1)%lapU)
          ! Zero boundary values
          call zeroGhostPoints(mg(j+1)%res)

          ! 4) Decend to coarser level
          call Vcycle(mg,j+1)

          ! Finished Vcycle decent. Prepare to ascend back to level 1
          ! 5) Prolongate the error

          call prolongate(mg(j+1)%e,mg(j+2)%e,mg(j+1)%g,mg(j+1)%temp_rxy,mg(j+1)%temp_rx)
          call add(mg(j+1)%u,mg(j+1)%e)

          ! 6) Final smoothing sweeps
          call solve(SOR,mg(j+1)%u,mg(j+1)%f,mg(j+1)%sigma,mg(j+1)%g,&
            mg(j+1)%ss,mg(j+1)%norm,mg(j+1)%displayTF)
          ! The solution on any grid above the 
          ! base grid is the error!
          call assign(mg(j+1)%e,mg(j+1)%u)

        else ! At coarsest level. Solve exactly.

          ! call setMaxIterations(mg(j+1)%ss,5) ! Fixed
          ! call setMaxIterations(mg(j+1)%ss,floor(exp(real(mg(j+1)%nlevels,cp)))) ! Dynamic
          call solve(SOR,mg(j+1)%u,mg(j+1)%f,mg(j+1)%sigma,mg(j+1)%g,&
            mg(j+1)%ss,mg(j+1)%norm,mg(j+1)%displayTF)

          ! The solution on any grid above the 
          ! base grid is the correction on the 
          ! finer grid!
          call assign(mg(j+1)%e,mg(j+1)%u)
        endif
      end subroutine

      end module