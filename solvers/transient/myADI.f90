      module myADI_mod
      ! call solve(ADI,u,f,u_bcs,g,ss,err,gridType,displayTF) ! To steady state, multi-scale time step
      !     u_t = alpha*(u_xx + u_yy + u_zz) - f = 0
      !           alpha*(u_xx + u_yy + u_zz) = f
      ! 
      ! call apply(ADI,u,f,u_bcs,g,ss,err,gridType,displayTF) ! One time step
      !     u_t = alpha*(u_xx + u_yy + u_zz) - f
      ! 
      ! using the Alternating Direction Implicit (ADI) 
      ! method described in:
      ! "Douglas, J. Alternating direction methods for 
      ! three space variables. Numer. Math. 4, 41–63 (1962)."
      ! 
      ! NOTE: Before solve() and apply(), must set:
      ! 
      ! call setDt(ADI,dt)           ! Required for apply
      ! call setAlpha(ADI,alpha)     ! Required for both
      ! 
      use BCs_mod
      use applyBCs_mod
      use grid_mod
      use solverSettings_mod
      use vectorOps_mod
      use myTriSolver_mod
      use myTriOperator_mod
      use myError_mod
      implicit none

      private

      public :: myADI,solve,apply
      public :: setDt,setAlpha
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

      type sparseMatrix
       real(cp),dimension(:),allocatable :: loDiag,diag,upDiag
      end type

      type myADI
        type(myTrisolver),dimension(3) :: triSolver
        type(myTriOperator),dimension(3) :: triOperator
        real(cp),dimension(:,:,:),allocatable :: lapu
        real(cp),dimension(:),allocatable :: dtj
        integer,dimension(3) :: s
        integer :: nTimeLevels,gridType
        real(cp) :: alpha,dt
        real(cp),dimension(:,:,:),allocatable :: alpha_var
        logical :: var_coeff = .false.
      end type

      interface init;        module procedure initADI;           end interface
      interface delete;      module procedure deleteADI;         end interface
      interface solve;       module procedure solveADI;          end interface
      interface apply;       module procedure applyADI;          end interface

      interface setAlpha;    module procedure setAlphaUniform;   end interface
      interface setAlpha;    module procedure setAlphaVariable;  end interface

      contains

      subroutine initSystem(ADI,s,g,dir)
        implicit none
        type(myADI),intent(inout) :: ADI
        integer,dimension(3),intent(in) :: s
        type(grid),intent(in) :: g
        integer,intent(in) :: dir
        real(cp),dimension(:),allocatable :: upDiag,diag,loDiag

        ! Prep matrix:
        allocate(diag(s(dir)))
        allocate(upDiag(s(dir)-1))
        allocate(loDiag(s(dir)-1))

        call setUpSystem(loDiag,diag,upDiag,real(0.5,cp)*ADI%dt*ADI%alpha,&
         g%c(dir)%dhn,g%c(dir)%dhc,s(dir))
        call init(ADI%triOperator(dir),loDiag,diag,upDiag)

        call setUpSystem(loDiag,diag,upDiag,real(-0.5,cp)*ADI%dt*ADI%alpha,&
         g%c(dir)%dhn,g%c(dir)%dhc,s(dir))
        call addIdentity(diag)
        call init(ADI%triSolver(dir),loDiag,diag,upDiag)

        deallocate(upDiag,diag,loDiag)
      end subroutine

      subroutine initADI(ADI,s,g,gridType)
        implicit none
        type(myADI),intent(inout) :: ADI
        integer,dimension(3),intent(in) :: s
        type(grid),intent(in) :: g
        integer,intent(in) :: gridType
        integer :: j,smean
        real(cp) :: hj,h0
        ! Set preliminary parameters
        ADI%gridType = gridType; ADI%s = s

        ! Set multi-scale time steps:
        smean = (s(1)+s(2)+s(3))/3
        ! Try p = 0,1,...,2/dx = 2*M levels. This might be the issue

        h0 = g%dhMin ! Smallest spatial step on grid
        if (allocated(ADI%dtj)) deallocate(ADI%dtj)
        ADI%nTimeLevels = floor(log(real(smean+1))/log(real(2.0,cp)))     ! Dirichlet
        ! ADI%nTimeLevels = floor(log(real(smean+1))/log(real(2.0,cp))) - 2 ! Neumann
        allocate(ADI%dtj(ADI%nTimeLevels))
        do j = 1,ADI%nTimeLevels
          hj = (real(2.0,cp)**real(j-1,cp))*h0
          ADI%dtj(j) = real(4.0,cp)*(hj**(real(2.0,cp)))/(ADI%alpha*PI**real(2.0,cp))
        enddo

        ! Set up tridiagonal systems:
        allocate(ADI%lapu(s(1),s(2),s(3)))
      end subroutine

      subroutine deleteADI(ADI)
       implicit none
       type(myADI),intent(inout) :: ADI
       call delete(ADI%triSolver(1))
       call delete(ADI%triSolver(2))
       call delete(ADI%triSolver(3))
       call delete(ADI%triOperator(1))
       call delete(ADI%triOperator(2))
       call delete(ADI%triOperator(3))
       if (allocated(ADI%lapu)) deallocate(ADI%lapu)
       if (allocated(ADI%dtj)) deallocate(ADI%dtj)
       if (allocated(ADI%alpha_var)) deallocate(ADI%alpha_var)
      end subroutine

      subroutine relax3DOldButWorking(ADI,u,f,u_bcs,g)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        ! Locals
        real(cp),dimension(:,:,:),allocatable :: temp1,temp2,temp3,fstar

        allocate(temp1(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(temp2(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(temp3(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(fstar(ADI%s(1),ADI%s(2),ADI%s(3)))
        fstar = real(0.0,cp); temp2 = real(0.0,cp); temp3 = real(0.0,cp);

        ! Compute (0.5 dt d^2/dh^2) u^* for h = x,y,z
        call apply(ADI%triOperator(1),temp1,u,1,0)
        call apply(ADI%triOperator(2),temp2,u,2,0)
        call apply(ADI%triOperator(3),temp3,u,3,0)

        ! ---------- Step in x ----------------
        ! Compute RHS:
        ! fstar = (I + .5 dt d_xx + dt d_yy + dt d_zz)u - dt*f
        call addInterior(fstar,u,1)
        call addInterior(fstar,temp1,1)
        call addInterior(fstar,real(2.0,cp)*temp2,1)
        call addInterior(fstar,real(2.0,cp)*temp3,1)
        call addInterior(fstar,-ADI%dt*f,1)

        ! Apply BCs along x
        call applyBCFace(u_bcs,fstar,g,1)
        call applyBCFace(u_bcs,fstar,g,4)

        ! Solve (I - 0.5 dt d_xx) u^* = fstar
        call apply(ADI%triSolver(1),temp1,fstar,1,1)

        ! ---------- Step in y ----------------
        ! Compute RHS:
        ! fstar = un* - .5 dt d_yy
        fstar = temp1 - temp2
        ! fstar = temp1
        ! call addInterior(fstar,-temp2,2)


        ! Apply BCs along y
        call applyBCFace(u_bcs,fstar,g,2)
        call applyBCFace(u_bcs,fstar,g,5)

        ! Solve (I - 0.5 dt d_yy) u^* = fstar
        call apply(ADI%triSolver(2),temp1,fstar,2,1)

        ! ---------- Step in z ----------------
        ! Compute RHS:
        ! fstar = un** - .5 dt d_zz
        fstar = temp1 - temp3
        ! fstar = temp1
        ! call addInterior(fstar,-temp3,3)

        ! Apply BCs along z
        call applyBCFace(u_bcs,fstar,g,3)
        call applyBCFace(u_bcs,fstar,g,6)

        ! Solve (I - 0.5 dt d_zz) u^* = fstar
        call apply(ADI%triSolver(3),u,fstar,3,1)

        ! Re-apply BCs for u along x and y
        call applyAllBCs(u_bcs,u,g)
        ! call applyBCFace(u_bcs,u,g,1)
        ! call applyBCFace(u_bcs,u,g,2)
        ! call applyBCFace(u_bcs,u,g,3)
        ! call applyBCFace(u_bcs,u,g,4)
        ! call applyBCFace(u_bcs,u,g,6)
        ! call applyBCFace(u_bcs,u,g,5)

        deallocate(temp1,temp2,temp3,fstar)
      end subroutine

      subroutine relax3D(ADI,u,f,u_bcs,g)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        ! Locals
        real(cp),dimension(:,:,:),allocatable :: temp1,temp2,temp3,fstar

        allocate(temp1(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(temp2(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(temp3(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(fstar(ADI%s(1),ADI%s(2),ADI%s(3)))
        fstar = real(0.0,cp)

        ! Compute (0.5 dt d^2/dh^2) u^* for h = x,y,z
        call apply(ADI%triOperator(1),temp1,u,1,0)
        call apply(ADI%triOperator(2),temp2,u,2,0)
        call apply(ADI%triOperator(3),temp3,u,3,0)

        ! ---------- Step in x ----------------
        ! Compute RHS:
        ! fstar = (I + .5 dt d_xx + dt d_yy + dt d_zz)u - dt*f
        call addInterior(fstar,u,1)
        call addInterior(fstar,temp1,1)
        call addInterior(fstar,real(2.0,cp)*temp2,1)
        call addInterior(fstar,real(2.0,cp)*temp3,1)
        call addInterior(fstar,-ADI%dt*f,1)

        ! Apply BCs along x
        call applyBCFace(u_bcs,fstar,g,1)
        call applyBCFace(u_bcs,fstar,g,4)

        ! Solve (I - 0.5 dt d_xx) u^* = fstar
        call apply(ADI%triSolver(1),temp1,fstar,1,1)

        ! ---------- Step in y ----------------
        ! Compute RHS:
        ! fstar = un* - .5 dt d_yy
        fstar = temp1 - temp2
        ! fstar = temp1
        ! call addInterior(fstar,-temp2,2)


        ! Apply BCs along y
        call applyBCFace(u_bcs,fstar,g,2)
        call applyBCFace(u_bcs,fstar,g,5)

        ! Solve (I - 0.5 dt d_yy) u^* = fstar
        call apply(ADI%triSolver(2),temp1,fstar,2,1)

        ! ---------- Step in z ----------------
        ! Compute RHS:
        ! fstar = un** - .5 dt d_zz
        fstar = temp1 - temp3
        ! fstar = temp1
        ! call addInterior(fstar,-temp3,3)

        ! Apply BCs along z
        call applyBCFace(u_bcs,fstar,g,3)
        call applyBCFace(u_bcs,fstar,g,6)

        ! Solve (I - 0.5 dt d_zz) u^* = fstar
        call apply(ADI%triSolver(3),u,fstar,3,1)

        ! Re-apply BCs for u along x and y
        call applyAllBCs(u_bcs,u,g)
        ! call applyBCFace(u_bcs,u,g,1)
        ! call applyBCFace(u_bcs,u,g,2)
        ! call applyBCFace(u_bcs,u,g,3)
        ! call applyBCFace(u_bcs,u,g,4)
        ! call applyBCFace(u_bcs,u,g,6)
        ! call applyBCFace(u_bcs,u,g,5)

        deallocate(temp1,temp2,temp3,fstar)
      end subroutine

      subroutine applyADI(ADI,u,f,u_bcs,g,ss,err,gridType,displayTF)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(in) :: ss
        type(myError),intent(inout) :: err
        integer,intent(in) :: gridType
        logical,intent(in) :: displayTF
        integer,dimension(3) :: s

        s = shape(f); ADI%s = s
        call initADI(ADI,s,g,gridType)

        call initSystem(ADI,s,g,1)
        call initSystem(ADI,s,g,2)
        call initSystem(ADI,s,g,3)
        call relax3D(ADI,u,f,u_bcs,g)

        if (displayTF) then
          write(*,*) 'ADI time step = ',ADI%dt
          ! This is not the correct way to compute the residuals
          ! since the time-dependent term is not included
          ! and has not reached SS in the one step
          select case (ADI%gridType)
          case (1); call CC2CCLap(ADI%lapu,u,g)
          case (2); call myNodeLap(ADI%lapu,u,g)
          end select
          call compute(err,f(2:s(1)-1,2:s(2)-1,2:s(3)-1),ADI%lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
          call print(err,'ADI Residuals for '//trim(adjustl(getName(ss))))
        endif

        call delete(ADI)
      end subroutine

      subroutine solveADI(ADI,u,f,u_bcs,g,ss,err,gridType,displayTF)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(in) :: ss
        type(myError),intent(inout) :: err
        integer,intent(in) :: gridType
        logical,intent(in) :: displayTF
        integer,dimension(3) :: s
        integer :: i,j,Nt

        s = shape(f); ADI%s = s
        call initADI(ADI,s,g,gridType)

        Nt = maxval((/getMaxIterations(ss)/ADI%nTimeLevels/2, 1/))
        Nt = 10

        do i=1,Nt

          ! Begin V-cycle of multi-scale
          do j = 1,ADI%nTimeLevels
            call setDt(ADI,ADI%dtj(j))
            ! call setDt(ADI,real(100.0,cp))
            call initSystem(ADI,s,g,1)
            call initSystem(ADI,s,g,2)
            call initSystem(ADI,s,g,3)
            call relax3D(ADI,u,f,u_bcs,g)
          enddo
          do j = ADI%nTimeLevels, 1, -1
            call setDt(ADI,ADI%dtj(j))
            ! call setDt(ADI,real(100.0,cp))
            call initSystem(ADI,s,g,1)
            call initSystem(ADI,s,g,2)
            call initSystem(ADI,s,g,3)
            call relax3D(ADI,u,f,u_bcs,g)
          enddo

        enddo

        if (displayTF) then
          write(*,*) 'ADI Multi-scale Time Steps = ',ADI%dtj
          write(*,*) 'Number of multi-scale sweeps = ',Nt
          write(*,*) 'Number of multi-scale time levels = ',ADI%nTimeLevels
          write(*,*) 'Number of ADI iterations = ',2*Nt*ADI%nTimeLevels

          select case (ADI%gridType)
          case (1); call CC2CCLap(ADI%lapu,u,g)
          case (2); call myNodeLap(ADI%lapu,u,g)
          end select
          call compute(err,f(2:s(1)-1,2:s(2)-1,2:s(3)-1),ADI%lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
          call print(err,'ADI Residuals for '//trim(adjustl(getName(ss))))
        endif

        call delete(ADI)
      end subroutine

      subroutine setUpSystem(loDiag,diag,upDiag,alpha,dhn,dhc,s)
        implicit none
        real(cp),dimension(:),intent(inout) :: loDiag,diag,upDiag
        real(cp),intent(in) :: alpha
        real(cp),dimension(:),intent(in) :: dhn,dhc
        integer,intent(in) :: s
        integer :: j

        diag(1) = real(1.0,cp)
        upDiag(1) = real(0.0,cp)
        do j=2,s-1
          loDiag(j-1) = alpha/(dhn(j-1)*dhc(j-1))
          diag(j) = -real(2.0,cp)*alpha/(dhn(j-1)*dhc(j-1))
          upDiag(j) = alpha/(dhn(j-1)*dhc(j-1))
        enddo
        diag(s) = real(1.0,cp)
        loDiag(s-1) = real(0.0,cp)
      end subroutine

      subroutine setUpSystemNew(loDiag,diag,upDiag,alpha,dh_p,dh_d,s)
        ! dh_p = primary grid (data lives on primary grid)
        ! dh_d = dual    grid (data lives on primary grid)
        ! diag = size(f)
        ! size(dh_p) = size(f)-1
        ! size(dh_d) = ??
        implicit none
        real(cp),dimension(:),intent(inout) :: loDiag,diag,upDiag
        real(cp),intent(in) :: alpha
        real(cp),dimension(:),intent(in) :: dh_p,dh_d
        integer,intent(in) :: s
        integer :: j

        diag(1) = real(1.0,cp)
        upDiag(1) = real(0.0,cp)
        do j=2,s-1
          loDiag(j-1) = alpha/(dh_p(j-1)*dh_d(j-1))
          diag(j) = -real(2.0,cp)*alpha/(dh_p(j-1)*dh_d(j-1))
          upDiag(j) = alpha/(dh_p(j-1)*dh_d(j-1))
        enddo
        diag(s) = real(1.0,cp)
        loDiag(s-1) = real(0.0,cp)
      end subroutine

      subroutine setUpSystemVarCoeff(loDiag,diag,upDiag,alpha,dhn,dhc,s)
        implicit none
        real(cp),dimension(:),intent(inout) :: loDiag,diag,upDiag
        real(cp),dimension(:),intent(in) :: dhn,dhc,alpha
        integer,intent(in) :: s
        integer :: j

        diag(1) = real(1.0,cp)
        upDiag(1) = real(0.0,cp)
        do j=2,s-1
          loDiag(j-1) = alpha(j-1)/(dhn(j-1)*dhc(j-1))
          diag(j) = -real(2.0,cp)*alpha(j-1)/(dhn(j-1)*dhc(j-1))
          upDiag(j) = alpha(j-1)/(dhn(j-1)*dhc(j-1))
        enddo
        diag(s) = real(1.0,cp)
        loDiag(s-1) = real(0.0,cp)
      end subroutine

      subroutine setDt(ADI,dt)
       implicit none
       type(myADI),intent(inout) :: ADI
       real(cp),intent(in) :: dt
       ADI%dt = dt
      end subroutine

      subroutine setAlphaUniform(ADI,alpha)
       implicit none
       type(myADI),intent(inout) :: ADI
       real(cp),intent(in) :: alpha
       ADI%alpha = alpha
      end subroutine

      subroutine setAlphaVariable(ADI,alpha)
       implicit none
       type(myADI),intent(inout) :: ADI
       real(cp),dimension(:,:,:),intent(in) :: alpha
       integer,dimension(3) :: s
       s = shape(alpha)
       if (allocated(ADI%alpha_var)) deallocate(ADI%alpha_var)
       allocate(ADI%alpha_var(s(1),s(2),s(3)))
       ADI%alpha_var = alpha
       ADI%var_coeff = .true.
      end subroutine

      subroutine addInterior(a,b,dir)
       implicit none
       real(cp),dimension(:,:,:),intent(inout) :: a
       real(cp),dimension(:,:,:),intent(in) :: b
       integer,intent(in) :: dir
       integer,dimension(3) :: s
       integer :: i,j,k,x,y,z
       select case (dir)
       case (1); x=1;y=0;z=0
       case (2); x=0;y=1;z=0
       case (3); x=0;y=0;z=1
       case default
       write(*,*) 'Error: dir must = 1,2,3 in addInterior.'; stop
       end select
       s = shape(a)

       !$OMP PARALLEL DO
       do i=1+x,s(1)-x
        do j=1+y,s(2)-y
         do k=1+z,s(3)-z
          a(i,j,k) = a(i,j,k) + b(i,j,k)
         enddo
        enddo
       enddo
       !$OMP END PARALLEL DO
      end subroutine

      subroutine subtractInterior(a,b,dir)
       implicit none
       real(cp),dimension(:,:,:),intent(inout) :: a
       real(cp),dimension(:,:,:),intent(in) :: b
       integer,intent(in) :: dir
       integer,dimension(3) :: s
       integer :: i,j,k,x,y,z
       select case (dir)
       case (1); x=1;y=0;z=0
       case (2); x=0;y=1;z=0
       case (3); x=0;y=0;z=1
       case default
       write(*,*) 'Error: dir must = 1,2,3 in addInterior.'; stop
       end select
       s = shape(a)

       !$OMP PARALLEL DO
       do i=1+x,s(1)-x
        do j=1+y,s(2)-y
         do k=1+z,s(3)-z
          a(i,j,k) = a(i,j,k) - b(i,j,k)
         enddo
        enddo
       enddo
       !$OMP END PARALLEL DO
      end subroutine

      subroutine addIdentity(diag)
        implicit none
        real(cp),dimension(:),intent(inout) :: diag
        integer :: j,s
        s = size(diag)
        do j = 2,s-1
          diag(j) = real(1.0,cp) + diag(j)
        enddo
      end subroutine

      end module