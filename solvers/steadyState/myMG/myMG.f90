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
      integer,parameter :: nMaxCycle = 10

      type grid
        private
        integer :: sx,sy,sz
        real(dpn),dimension(:),allocatable :: x,y,z
      end type

      type myMG
        private
        real(dpn),dimension(:,:,:) :: u,f,lapu,res
        type(BCs) :: u_bcs
        type(grid) :: g
        type(solverSettings) :: ss
      end type

      ! Interface needed to not interfere with griddata routines

      interface getMGXn
        module procedure getMGXn
      end interface
      interface getMGYn
        module procedure getMGYn
      end interface
      interface getMGZn
        module procedure getMGZn
      end interface

      contains

      subroutine myMG(u,f,u_bcs,gd,ss,gridType,displayTF)
        implicit none
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