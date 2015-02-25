      module myMG_mod
      ! call myMG(u,f,u_bcs,gd,ss,gridType,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), griddata (gd)
      ! and solver settings (ss) using the MultiGrid (MG) method
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     gd           = contains grid information (dhc,dhn)
      !     ss           = solver settings (specifies max iterations, tolerance etc.)
      !     gridType     = (1,2) = (cell-based,node-based)
      !     displayTF    = print residuals to screen (T,F)

      use constants_mod
      use griddata_mod
      use allBCs_mod
      use myAllocate_mod
      use coordinates_mod
      use applyBCs_mod
      use myError_mod
      use vectorOps_mod
      use mySOR
      implicit none

      private

      public :: myMG
      integer,parameter :: nLevels = 10

      type multiGrid
        private
        type(grid),dimension(nLevels) :: g
      end type

      type grid
        private
        real(dpn),dimension(:,:,:) :: u,f,lapu,res
        type(BCs) :: u_bcs
        type(coordinates),dimension(3) :: c
        type(solverSettings) :: ss
      end type

      type coordinates
        private
        integer :: s
        real(dpn),dimension(:),allocatable :: hn,hc
      end type

      interface init;       module procedure initMultiGrid;    end interface
      interface init;       module procedure initGrid;         end interface
      interface init;       module procedure initCoordinates;  end interface

      interface solve;      module procedure solveMG;          end interface

      interface getMGXn;    module procedure getMGXn;          end interface
      interface getMGYn;    module procedure getMGYn;          end interface
      interface getMGZn;    module procedure getMGZn;          end interface

      contains

      subroutine initMultiGrid(mg,s)
        implicit none
        type(multiGrid),intent(inout) :: mg
        integer,dimension(3),intent(in) :: s
        integer,dimension(3) :: sj
        integer :: j
        do j = 1,nLevels
          sj = s/(2**(j-1))+1-floor(1/(j)) ! shape of jth grid
          call init(mg%g(j),sj,j)
        enddo
      end subroutine

      subroutine initGrid(g,s,L)
        implicit none
        type(grid),intent(inout) :: g
        integer,dimension(3) :: s ! shape of level
        integer,intent(in) :: L   ! depth of level
        if (.not.allocated(g%f)) allocate(g%f(s(1),s(2),s(3)))
        if (.not.allocated(g%u)) allocate(g%u(s(1),s(2),s(3)))
        if (.not.allocated(g%lapu)) allocate(g%lapu(s(1),s(2),s(3)))
        if (.not.allocated(g%res)) allocate(g%res(s(1),s(2),s(3)))
        call init(g%c(1),s(1))
        call init(g%c(2),s(2))
        call init(g%c(3),s(3))
        g%c(1)%s = s(1)
        g%c(2)%s = s(2)
        g%c(3)%s = s(3)
      end subroutine

      subroutine initCoordinates(c,s)
        implicit none
        type(coordinates),intent(inout) :: c
        integer,intent(in) :: s
        if (.not.allocated(c%h)) allocate(c%h(s))
      end subroutine

      subroutine deleleteMG(mg)
        implicit none
        type(multiGrid),intent(inout) :: mg
        do j = 1,nLevels
          call delete(mg%g(j))
        enddo
      end subroutine

      subroutine deleleteMG(g)
        implicit none
        type(grid),intent(inout) :: g
        if (allocated(g%u)) deallocate(g%u)
        if (allocated(g%f)) deallocate(g%f)
        if (allocated(g%lapu)) deallocate(g%lapu)
        if (allocated(g%res)) deallocate(g%res)
        call delete(g%c(1))
        call delete(g%c(2))
        call delete(g%c(3))
      end subroutine

      subroutine deleteCoordinates(c)
        implicit none
        type(coordinates),intent(inout) :: c
        if (allocated(c%h)) deallocate(c%h)
      end subroutine

      subroutine restrictCoordinates(cr,c)
       implicit none
       type(coordinates),dimension(3),intent(inout) :: cr
       type(coordinates),dimension(3),intent(in) :: c
       real(dpn),dimension(:),allocatable :: hi
       integer :: dir,i
       do dir = 1,3                             ! Directions 1,2,3
         if (c(dir)%s.lt.2) then                ! No restriction necessary
           cr(dir)%h = c%(dir)%h
         else
           if (mod(c(dir)%s,2).eq.0) then       ! s is even, interpolate, then restrict
             do i = 1,cr(dir)%s
               hi(i) = c(dir)%h(2*i)
             enddo
           else                                 ! s is odd, restrict
             do i = 1,cr(dir)%s
               cr(dir)%h(i) = c(dir)%h(2*i)
             enddo
           endif
         endif
       enddo
      end subroutine




      subroutine setBCs(mg,u_bcs)
        implicit none
        type(multiGrid),intent(inout) :: mg
        type(BCs),intent(in) :: u_bcs
        integer :: j
        mg%g(1)%u_bcs = u_bcs
        do j = 2,nLevels
          ! Sets jth BCs and returns BCs for j+1
          call setBCs(mg%g(j) , mg%g(j-1)%u_bcs , mg%g(j)%u_bcs)
        enddo
      end subroutine

      subroutine setBCs(g,u_bcs,r_bcs)
        implicit none
        type(grid),intent(inout) :: g       ! course grid
        type(BCs),intent(in) :: u_bcs       ! BCs from finer grid
        type(coordinates),intent(in) :: c   ! coordinates from finer grid
        ! Set shape
        call setAllZero(g%r_bcs,g%s(1),g%s(2),g%s(3),1)
        ! Set types
        call setXminType(g%u_bcs,getXminType(u_bcs))
        call setXmaxType(g%u_bcs,getXmaxType(u_bcs))
        call setYminType(g%u_bcs,getYminType(u_bcs))
        call setYmaxType(g%u_bcs,getYmaxType(u_bcs))
        call setZminType(g%u_bcs,getZminType(u_bcs))
        call setZmaxType(g%u_bcs,getZmaxType(u_bcs))

        ! Restrict To next level
        call restrictBCs(g%u_bcs%XminVals,u_bcs%XminVals,g%c,c,1)
        call restrictBCs(g%u_bcs%XmaxVals,u_bcs%XmaxVals,g%c,c,1)
        call restrictBCs(g%u_bcs%YminVals,u_bcs%YminVals,g%c,c,2)
        call restrictBCs(g%u_bcs%YmaxVals,u_bcs%YmaxVals,g%c,c,2)
        call restrictBCs(g%u_bcs%ZminVals,u_bcs%ZminVals,g%c,c,3)
        call restrictBCs(g%u_bcs%ZmaxVals,u_bcs%ZmaxVals,g%c,c,3)
      end subroutine

      subroutine restrictBCs(r,u,cr,c,sr,s,dir)
        implicit none
        real(dpn),dimension(:,:),intent(in) :: u    ! 
        real(dpn),dimension(:,:),intent(inout) :: r ! coarse BCs
        type(coordinates),intent(in) :: cr,c        ! coarse and fine grid

        integer,dimension(3) :: su,sr
        real(dpn),dimension(:,:,:) :: temp
        integer :: s
        s = shape(r)

        allocate(temp)

        su = shape(u); sr = shape(r)
        allocate(temp1(sr(1),su(2),su(3)))
        ! Restrict in x if necessary
        select case (mod(s(1),2))
          case (0); temp1 = u
          case (1); call restrictBCs(temp1,u,gd,1)
        end select

        allocate(temp2(sr(1),sr(2),su(3)))
        ! Restrict in y if necessary
        select case (mod(s(2),2))
          case (0); temp2 = temp1
          case (1); call restrict(temp3,temp2,gd,1)
        end select

        allocate(temp2(sr(1),sr(2),sr(3)))
        ! Restrict in z if necessary
        select case (mod(s(3),2))
          case (0); r = temp3
          case (1); call restrict(r,temp3,gd,1)
        end select
        deallocate(temp1,temp2,temp3)
      end subroutine

      subroutine restrictBCsDirection(r,u,c,dir)
        implicit none
        real(dpn),dimension(:,:),intent(in) :: u     ! size = s
        real(dpn),dimension(:,:),intent(inout) :: r  ! size = (s-1)/2+1
        type(coordinates),intent(in) :: c
        integer,intent(in) :: dir

        integer,dimension(2) :: s
        real(dpn),dimension(:) :: hn
        real(dpn) :: alpha,t
        integer :: i,j,k,x,y,z
        s = shape(u)

        select case (dir)
        case (1); x=1;y=0;z=0; allocate(hn(s(1))); hn = c%x
        case (2); x=0;y=1;z=0; allocate(hn(s(2))); hn = c%y
        case (3); x=0;y=0;z=1; allocate(hn(s(3))); hn = c%z
        end select

        ! Every odd becomes the average of the value itself
        ! and its linearly interpolated neighbors:
        select case (dir)
        case (1);    do j=1+2*y,s(1)-2*y,1+y
                      do k=1+2*z,s(2)-2*z,1+z
                        t = j*y + k*z
                        alpha = (hn(t)-hn(t+1))/(hn(t-1)-hn(t+1))
                        r((j+1)/2,(k+1)/2) = 0.5*(u(j,k) + &
                        u(j-y,k-z)*alpha + &
                        u(j+y,k+z)*(1.0-alpha))
                      enddo
                     enddo
        case (2);    do i=1+2*x,s(1)-2*x,1+x
                      do k=1+2*z,s(2)-2*z,1+z
                        t = i*x + k*z
                        alpha = (hn(t)-hn(t+1))/(hn(t-1)-hn(t+1))
                        r((i+1)/2,(k+1)/2) = 0.5*(u(i,k) + &
                        u(i-x,k-z)*alpha + &
                        u(i+x,k+z)*(1.0-alpha))
                      enddo
                     enddo
        case (3);    do i=1+2*x,s(1)-2*x,1+x
                      do j=1+2*y,s(2)-2*y,1+y
                        t = i*x + j*y
                        alpha = (hn(t)-hn(t+1))/(hn(t-1)-hn(t+1))
                        r((i+1)/2,(j+1)/2) = 0.5*(u(i,j) + &
                        u(i-x,j-y)*alpha + &
                        u(i+x,j+y)*(1.0-alpha))
                      enddo
                     enddo
        end select
        ! Boundary values, normal to dir, remain the same:
        select case (dir)
        case (1); r(1,:,:) = u(1,:,:); r((s(1)-1)/2+1,:,:) = u(s(1),:,:)
        case (2); r(:,1,:) = u(:,1,:); r(:,(s(2)-1)/2+1,:) = u(:,s(2),:)
        case (3); r(:,:,1) = u(:,:,1); r(:,:,(s(3)-1)/2+1) = u(:,:,s(3))
        end select
        select case (dir)
        case (1); r(1,:,:) = u(1,:,:); r((s(1)-1)/2+1,:,:) = u(s(1),:,:)
        case (2); r(:,1,:) = u(:,1,:); r(:,(s(2)-1)/2+1,:) = u(:,s(2),:)
        case (3); r(:,:,1) = u(:,:,1); r(:,:,(s(3)-1)/2+1) = u(:,:,s(3))
        end select
      end subroutine



      subroutine interpO2(f,g,gd,dir,grid)
        ! interpO2 interpolates g from the primary grid to the
        ! dual grid using a 2nd order accurate stencil for non-uniform 
        ! grids. f lives on the dual grid. It is expected that
        ! f lives between g. That is:
        ! 
        ! grid = 1   :   f(cc grid), g(node/face grid)
        !                extrapolation required
        ! 
        !            f  g  f  g  f  g  f
        !            o--|--o--|--o--|--o   --> dir
        !                  *     *
        ! 
        ! grid = 2   :   f(node/face grid), g(cc grid)
        !                no extrapolation required
        ! 
        !            g  f  g  f  g  f  g
        !            o--|--o--|--o--|--o      --> dir
        !               *     *     *
        ! 
        ! * = assigned in this routine. This way, the entire
        ! array of f and g may be passed, without having to 
        ! index.
        ! 
        ! Therefore, shape(g) = shape(f) + 1 along dir
        ! Otherwise, shape(g) = shape(f)
        ! 
        ! When grid = 1, f is assumed to be located at the cell center.
        ! 
        implicit none
        real(dpn),dimension(:),intent(inout) :: f_out
        real(dpn),dimension(:),intent(in) :: f_in
        type(grid),intent(in) :: g
        integer,intent(in) :: dir
        integer :: i,j,k,t,x,y,z
        real(dpn) :: alpha
        real(dpn),dimension(:),allocatable :: hn,hc
        integer,dimension(3) :: s_in,s_out
        s_in = size(f_in); s_out = size(f_out)

        select case (dir)
        case (1); x=1;y=0;z=0
        if (grid.eq.2) then
          allocate(hn(sg(1)-1)); hn = xn
          allocate(hc(sg(1)));   hc = xc
        endif
        case (2); x=0;y=1;z=0
        if (grid.eq.2) then
          allocate(hn(sg(2)-1)); hn = yn
          allocate(hc(sg(2)));   hc = yc
        endif
        case (3); x=0;y=0;z=1
        if (grid.eq.2) then
          allocate(hn(sg(3)-1)); hn = zn
          allocate(hc(sg(3)));   hc = zc
        endif
        case default
          write(*,*) 'Error: dir must = 1,2,3 in interpO2 (MultiGrid).';stop
        end select

        select case (grid)
        case (1) ! f(cc grid), g(node/face grid)
        !            f  g  f  g  f  g  f
        !            o--|--o--|--o--|--o   --> dir
        !                  *     *
        
        !$OMP PARALLEL DO
        do k=1,sg(3)-z
          do j=1,sg(2)-y
            do i=1,sg(1)-x
              f(i+x,j+y,k+z) = (g(i,j,k)+g(i+x,j+y,k+z))/two
            enddo
          enddo
        enddo
        !$OMP END PARALLEL DO
        case (2) ! f(node/face grid), g(cc grid)
        !            g  f  g  f  g  f  g
        !            o--|--o--|--o--|--o      --> dir
        !               *     *     *

        !$OMP PARALLEL PRIVATE(alpha,t)
        !$OMP DO
        do k=1,sg(3)-z
          do j=1,sg(2)-y
            do i=1,sg(1)-x
              t = i*x + j*y + k*z
              alpha = (hn(t) - hc(t+1))/(hc(t) - hc(t+1))
              f(i,j,k) = g(i,j,k)*alpha +&
                            g(i+x,j+y,k+z)*(one-alpha)
            enddo
          enddo
        enddo
        !$OMP END DO
        !$OMP END PARALLEL
        case default
          write(*,*) 'grid must be 1 or 2. Terminating.';stop
        end select

        if (grid.eq.2) then
          deallocate(hn,hc)
        endif
      end subroutine

      subroutine extrapO2(f,g,gd,dir) ! Seems to be working.
        ! extrapO2 extrapolates g from the primary grid to the
        ! dual grid using a 2nd order accurate stencil for 
        ! non-uniform grids. f lives on the dual grid. 
        ! It is expected that f lives between g. That is:
        ! 
        ! For interpO2, this corresponds to grid = 1
        ! f(cc grid), g(node/face grid)
        ! 
        !            f  g  f  g  f  g  f
        !            o--|--o--|--o--|--o   --> dir
        !            *                 *
        ! 
        ! * = assigned in this routine. This way, the entire
        ! array of f and g may be passed, without having to 
        ! index.
        ! 
        ! Therefore, size(g) = size(f) + 1 along dir
        ! Otherwise, size(g) = size(f)
        ! 
        implicit none
        real(dpn),dimension(:,:,:),intent(in) :: g
        real(dpn),dimension(:,:,:),intent(inout) :: f
        type(griddata),intent(in) :: gd
        integer,intent(in) :: dir
        real(dpn) :: a1,a2 ! differences in Taylor expansion
        real(dpn) :: c1,c2 ! Coefficients of function values
        real(dpn),dimension(:),allocatable :: hn,hc
        integer,dimension(3) :: sg,sf
        sg = shape(g); sf = shape(f)
        select case (dir)
        case (1); allocate(hn(sf(1)-1)); call getXn(gd,hn)
        allocate(hc(sf(1))); call getXcc(gd,hc)
        case (2); allocate(hn(sf(2)-1)); call getYn(gd,hn)
        allocate(hc(sf(2))); call getYcc(gd,hc)
        case (3); allocate(hn(sf(3)-1)); call getZn(gd,hn)
        allocate(hc(sf(3))); call getZcc(gd,hc)
        case default
          write(*,*) 'Error: dir must = 1,2,3 in extrapO2.';stop
        end select
        ! BACKWARD EXTRAPOLATION (* = assigned)
        !            f  g  f  g  f  g  f
        !            o--|--o--|--o--|--o   --> dir
        !            *
        a1 = hc(1) - hn(1)
        a2 = hn(1) - hn(2)
        c1 = one + a1/a2
        c2 = -a1/a2
        select case (dir)
        case (1)
        f(1,:,:) = c1*g(1,:,:) + c2*g(2,:,:)
        case (2)
        f(:,1,:) = c1*g(:,1,:) + c2*g(:,2,:)
        case (3)
        f(:,:,1) = c1*g(:,:,1) + c2*g(:,:,2)
        end select
        ! FORWARD EXTRAPOLATION (* = assigned)
        !            f  g  f  g  f  g  f
        !            o--|--o--|--o--|--o   --> dir
        !                              *
        a1 = hc(sf(dir)) - hn(sf(dir)-1)
        a2 = hn(sg(dir)) - hn(sg(dir)-1)
        c1 = one + a1/a2
        c2 = -a1/a2
        select case (dir)
        case (1)
        f(sf(1),:,:)=c1*g(sg(1),:,:)+c2*g(sg(1)-1,:,:)
        case (2)
        f(:,sf(2),:)=c1*g(:,sg(2),:)+c2*g(:,sg(2)-1,:)
        case (3)
        f(:,:,sf(3))=c1*g(:,:,sg(3))+c2*g(:,:,sg(3)-1)
        end select
        deallocate(hn,hc)
      end subroutine



      subroutine solveMultiGrid(mg,u,f,u_bcs,gd,ss,gridType,displayTF)
        implicit none
        type(multiGrid),intent(inout) :: mg
        real(dpn),dimension(:,:,:),intent(inout) :: u
        real(dpn),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(griddata),intent(in) :: gd
        type(solverSettings),intent(inout) :: ss
        integer,intent(in) :: gridType
        logical,intent(in) :: displayTF

        type(mgGrid) :: mgd
        integer :: i_MG,n_MG,maxIterations
        integer :: sx,sy,sz

        ! 1) Determine number of levels along each direction
        call init(mg,shape(u)) ! set shapes of levels
        call setBCs(mg,u_bcs)  ! set BCs at each level
        call setSS(mg,ss)      ! set solver settings at each level

        call initializeSolverSettings(ss_temp)
        call setMaxIterations(ss_temp,2)

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif; continueLoop = .true.

        ! Allocate for coarse grid correction
        call getRestrictedSize(f,sx,sy,sz)
        allocate(uc_correction(sx,sy,sz))
        allocate(u_correction(s(1),s(2),s(3)))

        call myAllocate(Nx,Ny,Nz,u)
        allocate(xn(Nx),yn(Ny),zn(Nz))
        call getX(gd,xn); call setX(mgd%xyz_n,xn)
        call getY(gd,yn); call setY(mgd%xyz_n,yn)
        call getZ(gd,zn); call setZ(mgd%xyz_n,zn)

        do while (continueLoop.and.TF)

          call setIteration(ss,i_MG)

          ! 1) Smooth on finest level
          call mySOR(u,f,u_bcs,gd,ss_temp,2)

          ! 2) Get residual on finest level
          call myNodeLap(lapU,u,gd)
          res = f - lapU
          ! Zero boundary values
          res(1,:,:) = 0; res(s(1),:,:) = 0
          res(:,1,:) = 0; res(:,s(2),:) = 0
          res(:,:,1) = 0; res(:,:,s(3)) = 0

          ! 3) Begin decending into coarser grids, starting at level 2
          call Vcycle(uc_correction,res,2,gd,ss_temp)

          ! 4) Prolong correction back to grid level 1
          call prolong(u_correction,uc_correction,gd)
          u = u + u_correction

          ! 5) Final smoothing sweeps
          call mySOR(u,f,u_bcs,gd,ss_temp,2)

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          if (.not.continueLoop) exit
          ! ************************************************************************************
        enddo
        deallocate(uc_correction,u_correction)
        deallocate(lapU,res)

       call deleteCoordinates(mgd%xyz_n)
       deallocate(xn,yn,zn)
      end subroutine



      subroutine solveMultiGrid(mg,u,f,u_bcs,gd,ss,gridType,displayTF)
        implicit none
        type(multiGrid),intent(inout) :: mg
        real(dpn),dimension(:,:,:),intent(inout) :: u
        real(dpn),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(griddata),intent(in) :: gd
        integer,intent(in) :: gridType

        type(mgGrid) :: mgd
        real(dpn),dimension(:,:,:),allocatable :: lapU,res
        real(dpn),dimension(:,:,:),allocatable :: uc_correction,u_correction
        type(solverSettings),intent(inout) :: ss
        type(solverSettings) :: ss_temp
        logical :: continueLoop,TF
        integer :: i_MG,n_MG,maxIterations
        integer :: sx,sy,sz
        integer :: Nx,Ny,Nz
        real(dpn),dimension(:),allocatable :: xn,yn,zn

        ! 1) Determine number of levels along each direction
        call init(mg)
        s = shape(u)
        allocate(lapU(s(1),s(2),s(3)))
        allocate(res(s(1),s(2),s(3)))

        call initializeSolverSettings(ss_temp)
        call setMaxIterations(ss_temp,2)

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif; continueLoop = .true.

        ! Allocate for coarse grid correction
        call getRestrictedSize(f,sx,sy,sz)
        allocate(uc_correction(sx,sy,sz))
        allocate(u_correction(s(1),s(2),s(3)))

        call myAllocate(Nx,Ny,Nz,u)
        allocate(xn(Nx),yn(Ny),zn(Nz))
        call getX(gd,xn); call setX(mgd%xyz_n,xn)
        call getY(gd,yn); call setY(mgd%xyz_n,yn)
        call getZ(gd,zn); call setZ(mgd%xyz_n,zn)

        do while (continueLoop.and.TF)

          call setIteration(ss,i_MG)

          ! 1) Smooth on finest level
          call mySOR(u,f,u_bcs,gd,ss_temp,2)

          ! 2) Get residual on finest level
          call myNodeLap(lapU,u,gd)
          res = f - lapU
          ! Zero boundary values
          res(1,:,:) = 0; res(s(1),:,:) = 0
          res(:,1,:) = 0; res(:,s(2),:) = 0
          res(:,:,1) = 0; res(:,:,s(3)) = 0

          ! 3) Begin decending into coarser grids, starting at level 2
          call Vcycle(uc_correction,res,2,gd,ss_temp)

          ! 4) Prolong correction back to grid level 1
          call prolong(u_correction,uc_correction,gd)
          u = u + u_correction

          ! 5) Final smoothing sweeps
          call mySOR(u,f,u_bcs,gd,ss_temp,2)

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          if (.not.continueLoop) exit
          ! ************************************************************************************
        enddo
        deallocate(uc_correction,u_correction)
        deallocate(lapU,res)

       call deleteCoordinates(mgd%xyz_n)
       deallocate(xn,yn,zn)
      end subroutine

      recursive subroutine Vcycle(uc,resf,lev,u_bcs,gd,ss)
        implicit none
        real(dpn),dimension(:,:,:),intent(inout) :: uc ! u coarse
        real(dpn),dimension(:,:,:),intent(in) :: resf  ! residual fine
        type(BCs),intent(in) :: u_bcs                  ! BCs fine
        integer,intent(in) :: lev                      ! level
        type(mgGrid),intent(in) :: gd                  ! griddata
        type(solverSettings),intent(inout) :: ss       ! solver settings
        ! Locals
        real(dpn),dimension(:,:,:),allocatable :: lapu,res,f,u_correction
        real(dpn) :: maxLev
        type(solverSettings) :: ss_temp
        logical :: continueLoop,TF
        integer :: sx,sy,sz
        integer :: i_MG,n_MG,maxIterations

        s = shape(uc)

        ! 1) Restrict the residual onto grid at level lev 
        ! and use this as RHS data
        call getRestrictedSize(uc,sx,sy,sz)
        allocate(f(sx,sy,sz))
        call restrict(f,resf)

        call initializeSolverSettings(ss_temp)
        call setMaxIterations(ss_temp,2)

        if (lev .lt. maxLev) then
          ! Have not reached coarsest level, prepare to decend!

          ! 1) Smooth
          call restrictBCs(uc_bcs,u_bcs)
          call mySOR(uc,f,uc_bcs,gd,ss_temp,2)

          ! 2) Get residual
          call myNodeLap(lapU,uc,gd)
          res = f - lapU
          ! Zero boundary values
          res(1,:,:) = 0; res(s(1),:,:) = 0
          res(:,1,:) = 0; res(:,s(2),:) = 0
          res(:,:,1) = 0; res(:,:,s(3)) = 0

          ! 3) Decend to coarser level
          call Vcycle(ucc,res,lev+1,uc_bcs,gd,ss_temp)

          ! Finished Vcycle decent. Prepare to acend back to level 1

          ! 4) Prolong correction
          ! ucc contains correction at coarser level lev+1
          ! Prolong this correction to this level (lev)
          call prolong(u_correction,ucc,gd)
          u = u + u_correction

          ! 5) Final smoothing sweeps
          call myPoisson(u,f,u_bcs,gd,ss_temp,2)

        else ! At coarsest level. Solve exactly.

          call setMaxIterations(ss_temp,100)
          call mySOR(uc,f,u_bcs,gd,ss_temp,2)

        endif

      end subroutine

      subroutine restrictXYZ(r,u,gd)
        implicit none
        real(dpn),dimension(:,:,:),intent(in) :: u
        real(dpn),dimension(:,:,:),intent(inout) :: r
        type(mgGrid),intent(in) :: gd
        integer,dimension(3) :: su,sr
        real(dpn),dimension(:,:,:) :: temp1,temp2,temp3

        su = shape(u); sr = shape(r)
        allocate(temp1(sr(1),su(2),su(3)))
        ! Restrict in x if necessary
        select case (mod(s(1),2))
          case (0); temp1 = u
          case (1); call restrict(temp1,u,gd,1)
        end select

        allocate(temp2(sr(1),sr(2),su(3)))
        ! Restrict in y if necessary
        select case (mod(s(2),2))
          case (0); temp2 = temp1
          case (1); call restrict(temp3,temp2,gd,1)
        end select

        allocate(temp2(sr(1),sr(2),sr(3)))
        ! Restrict in z if necessary
        select case (mod(s(3),2))
          case (0); r = temp3
          case (1); call restrict(r,temp3,gd,1)
        end select
        deallocate(temp1,temp2,temp3)
      end subroutine

      subroutine getRestrictedSize(f,sx,sy,sz)
        implicit none
        real(dpn),dimension(:,:,:),intent(in) :: f
        real(dpn),dimension(:,:,:),intent(inout) :: sx,sy,sz
        integer,dimension(3) :: s
        integer sx,sy,sz
        s = shape(f)
        select case (mod(s(1),2))
          case (0); sx=s(1)
          case (1); sx=(s(1)-1)/2+1
        end select
        select case (mod(s(2),2))
          case (0); sy=s(2)
          case (1); sy=(s(2)-1)/2+1
        end select
        select case (mod(s(3),2))
          case (0); sz=s(3)
          case (1); sz=(s(3)-1)/2+1
        end select
      end subroutine

      subroutine prolongateXYZ(p,u,gd)
        implicit none
        real(dpn),dimension(:,:,:),intent(in) :: u
        real(dpn),dimension(:,:,:),intent(inout) :: p
        type(mgGrid),intent(in) :: gd
        real(dpn),dimension(:,:,:),allocatable :: temp1,temp2
        integer,dimension(3) :: su,sp
        su = shape(u)
        sp = shape(p)
        allocate(temp1(sp(1),su(2),su(3)))
        call prolongate(temp1,u,1)
        allocate(temp2(sp(1),sp(2),su(3)))
        call prolongate(temp2,temp1,2)
        call prolongate(p,u,3)

        deallocate(temp1,temp2)
      end subroutine

      subroutine restrict(r,u,gd,dir)
        ! This routine expects that u has an ODD number of function
        ! values along dir
        implicit none
        real(dpn),dimension(:,:,:),intent(in) :: u     ! size = s
        real(dpn),dimension(:,:,:),intent(inout) :: r  ! size = (s-1)/2+1
        type(mgGrid),intent(in) :: gd
        integer,intent(in) :: dir
        integer,dimension(3) :: s
        real(dpn),dimension(:) :: hn
        real(dpn) :: alpha,t
        integer :: i,j,k,x,y,z
        s = shape(u)
        select case (dir)
        case (1); x=1;y=0;z=0; allocate(hn(s(1))); call getMGXn(gd,hn)
        case (2); x=0;y=1;z=0; allocate(hn(s(2))); call getMGYn(gd,hn)
        case (3); x=0;y=0;z=1; allocate(hn(s(3))); call getMGZn(gd,hn)
        end select

        ! Every odd becomes the average of the value itself
        ! and its linearly interpolated neighbors:
        do i=1+2*x,s(1)-2*x,1+x
         do j=1+2*y,s(2)-2*y,1+y
          do k=1+2*z,s(3)-2*z,1+z
            t = i*x + j*y + k*z
            alpha = (hn(t)-hn(t+1))/(hn(t-1)-hn(t+1))
            r((i+1)/2,(j+1)/2,(k+1)/2) = 0.5*(u(i,j,k) + &
            u(i-x,j-y,k-z)*alpha + &
            u(i+x,j+y,k+z)*(1.0-alpha))
          enddo
         enddo
        enddo
        ! Boundary values, normal to dir, remain the same:
        select case (dir)
        case (1); r(1,:,:) = u(1,:,:); r((s(1)-1)/2+1,:,:) = u(s(1),:,:)
        case (2); r(:,1,:) = u(:,1,:); r(:,(s(2)-1)/2+1,:) = u(:,s(2),:)
        case (3); r(:,:,1) = u(:,:,1); r(:,:,(s(3)-1)/2+1) = u(:,:,s(3))
        end select
      end subroutine

      subroutine prolongate(p,u,gd,dir) ! (Linear Interpolation)
        ! This routine expects that u has an ODD number of function
        ! values along dir
        implicit none
        real(dpn),dimension(:,:,:),intent(in) :: u    ! size = s
        real(dpn),dimension(:,:,:),intent(inout) :: p ! size = 2*s-1
        type(mgGrid),intent(in) :: gd
        integer,intent(in) :: dir
        integer,dimension(3) :: s
        real(dpn),dimension(:) :: hn
        real(dpn) :: alpha,t
        integer :: i,j,k,x,y,z
        s = shape(u)
        select case (dir)
        case (1); x=1;y=0;z=0; allocate(hn(s(1))); call getMGXn(gd,hn)
        case (2); x=0;y=1;z=0; allocate(hn(s(2))); call getMGYn(gd,hn)
        case (3); x=0;y=0;z=1; allocate(hn(s(3))); call getMGZn(gd,hn)
        end select

        ! Odd locations have coincident values:
        ! Index for p must be odd: 2n-1
        do i=1,s(1),1+x
         do j=1,s(2),1+y
          do k=1,s(3),1+z
            p((1+x)*i-x,(1+y)*j-y,(1+z)*k-z) = u(i,j,k)
          enddo
         enddo
        enddo

        ! Even locations are interpolated:
        ! Index for p must be even: 2n
        do i=1+x,s(1)-1,1+x
         do j=1+y,s(2)-1,1+y
          do k=1+z,s(3)-1,1+z
            t = i*x + j*y + k*z
            alpha = (hn(t)-hn(t+1))/(hn(t-1)-hn(t+1))
            p((1+x)*i,(1+y)*j,(1+z)*k) = u(i,j,k)*alpha + &
                                               u(i+x,j+y,k+z)*(1.0-alpha)
          enddo
         enddo
        enddo
      end subroutine

       subroutine getMGXn(this,x)
         implicit none
         real(dpn),dimension(:),intent(inout) :: x
         type(mgGrid),intent(in) :: this
         call getX(this%xyz_n,x)
       end subroutine

       subroutine getMGYn(this,y)
         implicit none
         real(dpn),dimension(:),intent(inout) :: y
         type(mgGrid),intent(in) :: this
         call getY(this%xyz_n,y)
       end subroutine

       subroutine getMGZn(this,z)
         implicit none
         real(dpn),dimension(:),intent(inout) :: z
         type(mgGrid),intent(in) :: this
         call getZ(this%xyz_n,z)
       end subroutine

      end module