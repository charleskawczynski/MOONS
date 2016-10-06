      module MG_mod
      ! call MGSolver(u,f,m,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), griddata (m)
      !  the MultiGrid (MG) method
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
      use Jacobi_mod
      implicit none

      private

      public :: multiGrid,init,delete,solve
      public :: setIterationsPerLevel,setIterationsAtMaxLevel
      public :: testRP

      logical,parameter :: exportMGGrids = .true.

      type multiGrid
        private
        type(Jacobi) :: smooth
        type(CG_solver) :: direct

        type(SF) :: k            ! material properties, e.g.

        type(SF) :: temp_rx      ! (or temp_pzy), temp fields (partially restricted / prolongated)
        type(SF) :: temp_rxy     ! (or temp_pz) , temp fields (partially restricted / prolongated)
        type(mesh) :: m_rx,m_rxy ! meshes (partially restricted)

        integer :: n_iter
        integer :: n_levels
        logical :: displayTF,MG_init
      end type

      interface init;       module procedure initMultiGrid;    end interface
      interface solve;      module procedure solveMultiGrid;   end interface

      interface delete;     module procedure deleteMG;         end interface
      interface delete;     module procedure deleteAllMG;      end interface

      contains

      subroutine initMultiGrid(mg,u,m_base,n_iter,displayTF)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m_base
        integer,intent(in) :: n_iter
        logical,intent(in) :: displayTF
        integer,dimension(3) :: s
        integer :: i,j,n_levels

        n_levels = size(mg)
        mg(:)%n_levels = size(mg)
        ! mg(:)%displayTF = displayTF
        mg(:)%displayTF = displayTF
        s = u%BF(1)%GF%s

        ! ******************** Check size of data ********************
        if (u%is_Node) then
        elseif (u%is_CC) then
        else; stop 'Error: MG only supports N or CC data in MG.f90'
        endif

        ! ******************** Initialize grids ********************
        call init(mg(1)%m,m_base)
        do i = 1,mg(1)%n_levels-1
          call restrict(mg(i+1)%m,mg(i)%m)
        enddo
        write(*,*) 'Multigrid levels:'
        do j = 1,mg(1)%n_levels
          write(*,*) 'N_cells of grid 1 level ',j,' = ',(/(mg(j)%m%B(1)%g%c(i)%sc,i=1,3)/)-2
        enddo

        ! ******************** Initialize fields ********************
        do j = 1,mg(1)%n_levels
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
        do i = 1,mg(1)%n_levels
          call restrict_x(mg(i)%m_rx,mg(i)%m)
          call restrict_xy(mg(i)%m_rxy,mg(i)%m)
        enddo
        do j = 1,mg(1)%n_levels
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
        do i = 1,mg(1)%n_levels
          call delete(mg(i)%m_rx)
          call delete(mg(i)%m_rxy)
        enddo

        ! ******************** Initialize norm/n_iter ********************
        do j = 1,mg(1)%n_levels
          call init(mg(j)%norm)
          mg(j)%n_iter = n_iter
        enddo

        ! ******************** Initialize BCs *************************
        mg(1)%MG_init = .true.

        ! call testRP(mg,'out\')
        ! write(*,*) 'Initialized MG'
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
        call delete(mg%smooth)
        call delete(mg%direct)
        call delete(mg%temp_rx)
        call delete(mg%temp_rxy)
        call delete(mg%m_rx)
        call delete(mg%m_rxy)
        call delete(mg%e)
        n_iter = 0
        n_levels = 0
        displayTF = .true.
        MG_init = .false.
      end subroutine

      subroutine testRP(mg,f,dir)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: mg
        type(SF),intent(in) :: f
        character(len=*),intent(in) :: dir
        integer :: i,n_levels
        n_levels = mg(1)%n_levels
        call assign(mg(1)%f,f)
        call export_3D_1C(mg(1)%m,mg(1)%f,dir,'f_Grid1',1)
        do i = 1,mg(1)%n_levels-1
          call restrict(mg(i+1)%f,mg(i)%f,mg(i)%m,mg(i)%temp_rx,mg(i)%temp_rxy)
          call export_3D_1C(mg(i+1)%m,mg(i+1)%f,dir,'f_Grid'//int2str(i+1),1)
          write(*,*) 'Finished restricting multigrid level ',i
        enddo
        write(*,*) '         Finished restricting'

        mg(n_levels)%u = mg(n_levels)%f
        write(*,*) '         Assigned coarsest level'
        call export_3D_1C(mg(n_levels)%m,mg(n_levels)%f,dir,'u_Grid'//int2str(n_levels),1)

        do i = mg(1)%n_levels, 2,-1
          call prolongate(mg(i-1)%u,mg(i)%u,mg(i-1)%m,mg(i-1)%temp_rxy,mg(i-1)%temp_rx)
          call export_3D_1C(mg(i-1)%m,mg(i-1)%f,dir,'u_Grid'//int2str(i-1),1)
          write(*,*) 'Finished prolongating multigrid level ',i
        enddo
        write(*,*) '         Finished prolongating'
      end subroutine


      end module