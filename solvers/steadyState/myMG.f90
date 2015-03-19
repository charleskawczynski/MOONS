      module myMG_mod
      ! call myMG(u,f,u_bcs,g,ss,gridType,displayTF)
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
      !     gridType     = (1,2) = (cell-based,node-based)
      !     displayTF    = print residuals to screen (T,F)

      use coordinates_mod
      use myDebug_mod
      use grid_mod
      use BCs_mod
      use myIO_mod
      use solverSettings_mod
      use applyBCs_mod
      use myError_mod
      use MG_tools_mod
      use delOps_mod
      use mySOR_mod
      implicit none

      private

      public :: multiGrid,init,delete,solve

      ! integer,parameter :: nLevels = 10

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type multiGrid
        private
        real(cp),dimension(:,:,:),allocatable :: u,f,lapU,res,e
        type(BCs) :: u_bcs
        type(grid) :: g
        type(myError) :: norms
        type(solverSettings) :: ss
        integer :: nLevels,gt
        logical :: displayTF
      end type

      interface init;       module procedure initMultiGrid;    end interface
      interface solve;      module procedure solveMultiGrid;   end interface

      interface delete;     module procedure deleteMG;         end interface
      interface delete;     module procedure deleteAllMG;      end interface

      contains

      subroutine initMultiGrid(mg,u,f,u_bcs,g_base,ss,gridType,displayTF)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        real(cp),dimension(:,:,:),intent(in) :: u,f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g_base
        type(solverSettings),intent(in) :: ss
        integer,intent(in) :: gridType
        logical,intent(in) :: displayTF
        ! character(len=*),intent(in) :: dir
        integer,dimension(3) :: N,s,sj
        integer :: i,j,nLevels

        nLevels = size(mg)
        mg(:)%nLevels = size(mg)
        mg(:)%gt = gridType
        mg(:)%displayTF = displayTF

        ! ******************** Initialize grids ********************
        call init(mg(1)%g,g_base)
        do i = 1,mg(1)%nLevels-1
          call restrict(mg(i+1)%g,mg(i)%g)
        enddo
        ! ******************** Initialize fields ********************

        s = shape(u)
        write(*,*) 'Multigrid levels:'

        do j = 1,mg(1)%nLevels
          sj = s/(2**(j-1)) + 1 - floor(real(1/j,cp))
          N = (/sj(1),sj(2),sj(3)/)
          write(*,*) 'shape(level_j) = ',sj
          if (allocated(mg(j)%u))    deallocate(mg(j)%u)
          if (allocated(mg(j)%f))    deallocate(mg(j)%f)
          if (allocated(mg(j)%lapU)) deallocate(mg(j)%lapU)
          if (allocated(mg(j)%res))  deallocate(mg(j)%res)
          if (allocated(mg(j)%e)) deallocate(mg(j)%e)
          allocate(mg(j)%u(N(1),N(2),N(3)))
          allocate(mg(j)%f(N(1),N(2),N(3)))
          allocate(mg(j)%lapU(N(1),N(2),N(3)))
          allocate(mg(j)%res(N(1),N(2),N(3)))
          allocate(mg(j)%e(N(1),N(2),N(3)))

          mg(j)%u = 0.0
          mg(j)%f = 0.0
          mg(j)%lapU = 0.0
          mg(j)%res = 0.0
          mg(j)%e = 0.0
        enddo

        do j = 1,mg(1)%nLevels
          call init(mg(j)%norms)
          call init(mg(j)%ss)
          call setMaxIterations(mg(j)%ss,getMaxIterations(ss))
          call setName(mg(j)%ss,'MG level('//int2str(j)//')         ')
        enddo

        call init(mg(1)%u_bcs,u_bcs)
        do j = 2,mg(1)%nLevels
          sj = s/(2**(j-1)) + 1 - floor(real(1/j,cp))
          N = (/sj(1),sj(2),sj(3)/)
          call setAllZero(mg(j)%u_bcs,N(1),N(2),N(3),1) ! Dirichlet wall coincident
        enddo

        mg(1)%f = f
        mg(1)%u = u

        write(*,*) 'Initialized MG'
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

      subroutine testRP(mg,dir)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        character(len=*),intent(in) :: dir
        integer :: i,nLevels
        nLevels = mg(1)%nLevels
        call writeToFile(mg(1)%g%c(1)%hn,mg(1)%g%c(2)%hn,mg(1)%g%c(3)%hn,mg(1)%f,dir,'f_Grid1')
        do i = 1,mg(1)%nLevels-1
          call restrict(mg(i+1)%f,mg(i)%f,mg(i)%g)
          call writeToFile(mg(i+1)%g%c(1)%hn,mg(i+1)%g%c(2)%hn,mg(i+1)%g%c(3)%hn,mg(i+1)%f,dir,'f_Grid'//int2str(i+1))
          write(*,*) 'Finished restricting multigrid level ',i
        enddo

        mg(nLevels)%u = mg(nLevels)%f
        call writeToFile(mg(nLevels)%g%c(1)%hn,mg(nLevels)%g%c(2)%hn,&
        mg(nLevels)%g%c(3)%hn,mg(nLevels)%u,dir,'u_Grid'//int2str(nLevels))
        do i = mg(1)%nLevels, 2,-1
          call prolongate(mg(i-1)%u,mg(i)%u,mg(i)%g)
          call writeToFile(mg(i-1)%g%c(1)%hn,mg(i-1)%g%c(2)%hn,mg(i-1)%g%c(3)%hn,mg(i-1)%u,dir,'u_Grid'//int2str(i-1))
          write(*,*) 'Finished prolongating multigrid level ',i
        enddo
      end subroutine

      subroutine solveMultiGridWorkingFor2Grid(mg,u,f,u_bcs,g,ss,norms,gridType,displayTF)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: norms
        type(mySOR) :: SOR
        integer,intent(in) :: gridType
        logical,intent(in) :: displayTF
        integer,dimension(3) :: s
        logical :: continueLoop,TF
        integer :: i_MG,maxIterations,nLevels

        write(*,*) '************** MG IS STARTING **************'
        write(*,*) '************** MG IS STARTING **************'
        write(*,*) '************** MG IS STARTING **************'
        write(*,*) '************** MG IS STARTING **************'
        write(*,*) '************** MG IS STARTING **************'
        nLevels = size(mg)
        call init(mg,u,f,u_bcs,g,ss,gridType,displayTF)

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif; continueLoop = .true.

        ! write(*,*) 'maxval(mg(1)%u) = ',maxval(mg(1)%u)

        i_MG = 0
        do while (continueLoop.and.TF)

          call setIteration(ss,i_MG)

          ! 1) Smooth on finest level
          ! Improvement on efficiency: Pass back residual from SOR:
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%u_bcs,mg(1)%g,&
            mg(1)%ss,mg(1)%norms,mg(1)%gt,mg(1)%displayTF)

          ! 2) Get residual on finest level
          call myNodeLap(mg(1)%lapU,mg(1)%u,mg(1)%g)
          mg(1)%res = mg(1)%f - mg(1)%lapU

          s = shape(mg(1)%res)
          ! Zero boundary values
          mg(1)%res(1,:,:) = 0.0; mg(1)%res(s(1),:,:) = 0.0
          mg(1)%res(:,1,:) = 0.0; mg(1)%res(:,s(2),:) = 0.0
          mg(1)%res(:,:,1) = 0.0; mg(1)%res(:,:,s(3)) = 0.0

          ! 3) Begin decending into coarser grids, starting at level 2
          ! V-Cycle: Given whatever is needed, compute, "exactly" the error
          ! on level 2.
          call VcycleWorkingFor2Grid(mg(2:nLevels),mg(1)%res,mg(1)%g)

          ! 4) Prolong correction back to grid level 1
          call prolongate(mg(1)%e,mg(2)%e,mg(1)%g)
          mg(1)%u = mg(1)%u + mg(1)%e

          ! 5) Final smoothing sweeps
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%u_bcs,mg(1)%g,&
            mg(1)%ss,mg(1)%norms,mg(1)%gt,mg(1)%displayTF)

          ! write(*,*) 'maxval(mg(1)%u) after smoothing = ',maxval(mg(1)%u)
          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          continueLoop = .false.
          if (.not.continueLoop) exit
          ! ************************************************************************************
          i_MG = i_MG + 1
        enddo
        u = mg(1)%u
        norms = mg(1)%norms

       call delete(mg)
        write(*,*) '************** MG IS ENDING **************'
        write(*,*) '************** MG IS ENDING **************'
        write(*,*) '************** MG IS ENDING **************'
        write(*,*) '************** MG IS ENDING **************'
        write(*,*) '************** MG IS ENDING **************'
      end subroutine

      recursive subroutine VcycleWorkingFor2Grid(mg,res,g)
        implicit none
        type(multigrid),dimension(:),intent(inout) :: mg
        real(cp),dimension(:,:,:),intent(in) :: res
        type(grid),intent(in) :: g
        type(mySOR) :: SOR
        ! Locals
        integer :: levelsLeft
        integer,dimension(3) :: s

        levelsLeft = size(mg)
        write(*,*) 'V-Cycle WAS CALLED!!!! ---------------------------------------------$$'

        ! 1) Restrict the residual onto the coarse grid
        !    and use it as the source term of the error
        call restrict(mg(1)%res,res,g)
        mg(1)%f = mg(1)%res

        if (levelsLeft .gt. 1) then
          ! stop 'Levels are not greater than 1!'

          ! Have not reached coarsest level, prepare to
          ! solve for error, and restrict residual to coarse
          ! grid

          ! 2) Smooth
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%u_bcs,mg(1)%g,&
            mg(1)%ss,mg(1)%norms,mg(1)%gt,mg(1)%displayTF)

          ! 3) Get residual
          call myNodeLap(mg(1)%lapU,mg(1)%u,mg(1)%g)
          mg(1)%res = mg(1)%f - mg(1)%lapU
          ! Zero boundary values
          s = shape(mg(1)%res)
          mg(1)%res(1,:,:) = 0.0; mg(1)%res(s(1),:,:) = 0.0
          mg(1)%res(:,1,:) = 0.0; mg(1)%res(:,s(2),:) = 0.0
          mg(1)%res(:,:,1) = 0.0; mg(1)%res(:,:,s(3)) = 0.0

          ! 4) Decend to coarser level
          call VcycleWorkingFor2Grid(mg(2:levelsLeft),mg(1)%res,mg(1)%g)

          ! Finished Vcycle decent. Prepare to ascend back to level 1
          ! 5) Prolongate the error

          call prolongate(mg(1)%e,mg(2)%e,mg(1)%g)
          mg(1)%u = mg(1)%u + mg(1)%e

          ! 6) Final smoothing sweeps
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%u_bcs,mg(1)%g,&
            mg(1)%ss,mg(1)%norms,mg(1)%gt,mg(1)%displayTF)
          ! The solution on any grid above the 
          ! base grid is the error!
          mg(1)%e = mg(1)%u

        else ! At coarsest level. Solve exactly.

          call setMaxIterations(mg(1)%ss,100)
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%u_bcs,mg(1)%g,&
            mg(1)%ss,mg(1)%norms,mg(1)%gt,mg(1)%displayTF)

          ! The solution on any grid above the 
          ! base grid is the error!
          mg(1)%e = mg(1)%u
        endif
      end subroutine

      subroutine solveMultiGrid(mg,u,f,u_bcs,g,ss,norms,gridType,displayTF)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: norms
        type(mySOR) :: SOR
        integer,intent(in) :: gridType
        logical,intent(in) :: displayTF
        integer,dimension(3) :: s
        logical :: continueLoop,TF
        integer :: i_MG,maxIterations,nLevels

        write(*,*) '************** MG IS STARTING **************'
        write(*,*) '************** MG IS STARTING **************'
        write(*,*) '************** MG IS STARTING **************'
        write(*,*) '************** MG IS STARTING **************'
        write(*,*) '************** MG IS STARTING **************'
        nLevels = size(mg)
        call init(mg,u,f,u_bcs,g,ss,gridType,displayTF)

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif; continueLoop = .true.

        ! write(*,*) 'maxval(mg(1)%u) = ',maxval(mg(1)%u)

        i_MG = 0
        do while (continueLoop.and.TF)

          call setIteration(ss,i_MG)

          ! 1) Smooth on finest level
          ! Improvement on efficiency: Pass back residual from SOR:
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%u_bcs,mg(1)%g,&
            mg(1)%ss,mg(1)%norms,mg(1)%gt,mg(1)%displayTF)

          ! 2) Get residual on finest level
          call myNodeLap(mg(1)%lapU,mg(1)%u,mg(1)%g)
          mg(1)%res = mg(1)%f - mg(1)%lapU

          s = shape(mg(1)%res)
          ! Zero boundary values
          ! mg(1)%res(1,:,:) = 0.0; mg(1)%res(s(1),:,:) = 0.0
          ! mg(1)%res(:,1,:) = 0.0; mg(1)%res(:,s(2),:) = 0.0
          ! mg(1)%res(:,:,1) = 0.0; mg(1)%res(:,:,s(3)) = 0.0

          ! 3) Begin decending into coarser grids, starting at level 2
          ! V-Cycle: Given whatever is needed, compute, "exactly" the error
          ! on level 2.
          call Vcycle(mg,1)

          ! 4) Prolong correction back to grid level 1
          call prolongate(mg(1)%e,mg(2)%e,mg(1)%g)
          mg(1)%u = mg(1)%u + mg(1)%e

          ! 5) Final smoothing sweeps
          call solve(SOR,mg(1)%u,mg(1)%f,mg(1)%u_bcs,mg(1)%g,&
            mg(1)%ss,mg(1)%norms,mg(1)%gt,mg(1)%displayTF)

          ! write(*,*) 'maxval(mg(1)%u) after smoothing = ',maxval(mg(1)%u)
          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          ! continueLoop = .false.
          if (.not.continueLoop) exit
          ! ************************************************************************************
          i_MG = i_MG + 1
        enddo
        u = mg(1)%u
        norms = mg(1)%norms

       call delete(mg)
        write(*,*) '************** MG IS ENDING **************'
        write(*,*) '************** MG IS ENDING **************'
        write(*,*) '************** MG IS ENDING **************'
        write(*,*) '************** MG IS ENDING **************'
        write(*,*) '************** MG IS ENDING **************'
      end subroutine

      recursive subroutine Vcycle(mg,j)
        implicit none
        type(multigrid),dimension(:),intent(inout) :: mg
        integer,intent(in) :: j
        type(mySOR) :: SOR
        ! Locals
        integer :: nLevels
        integer,dimension(3) :: s

        nLevels = size(mg)
        write(*,*) 'V-Cycle WAS CALLED!!!! ---------------------------------------------$$'

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
            mg(j+1)%ss,mg(j+1)%norms,mg(j+1)%gt,mg(j+1)%displayTF)

          ! 3) Get residual
          call myNodeLap(mg(j+1)%lapU,mg(j+1)%u,mg(j+1)%g)
          mg(j+1)%res = mg(j+1)%f - mg(j+1)%lapU
          ! Zero boundary values
          s = shape(mg(j+1)%res)
          ! mg(1)%res(1,:,:) = 0.0; mg(1)%res(s(1),:,:) = 0.0
          ! mg(1)%res(:,1,:) = 0.0; mg(1)%res(:,s(2),:) = 0.0
          ! mg(1)%res(:,:,1) = 0.0; mg(1)%res(:,:,s(3)) = 0.0

          ! 4) Decend to coarser level
          call Vcycle(mg,j+1)

          ! Finished Vcycle decent. Prepare to ascend back to level 1
          ! 5) Prolongate the error

          call prolongate(mg(j+1)%e,mg(j+2)%e,mg(j+1)%g)
          mg(j+1)%u = mg(j+1)%u + mg(j+1)%e

          ! 6) Final smoothing sweeps
          call solve(SOR,mg(j+1)%u,mg(j+1)%f,mg(j+1)%u_bcs,mg(j+1)%g,&
            mg(j+1)%ss,mg(j+1)%norms,mg(j+1)%gt,mg(j+1)%displayTF)
          ! The solution on any grid above the 
          ! base grid is the error!
          mg(j+1)%e = mg(j+1)%u

        else ! At coarsest level. Solve exactly.

          call setMaxIterations(mg(j+1)%ss,100)
          call solve(SOR,mg(j+1)%u,mg(j+1)%f,mg(j+1)%u_bcs,mg(j+1)%g,&
            mg(j+1)%ss,mg(j+1)%norms,mg(j+1)%gt,mg(j+1)%displayTF)

          ! The solution on any grid above the 
          ! base grid is the correction on the 
          ! finer grid!
          mg(j+1)%e = mg(j+1)%u
        endif
      end subroutine

      end module