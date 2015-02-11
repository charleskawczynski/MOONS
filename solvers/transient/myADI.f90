      module myADI_mod
      ! call solve(ADI,u,f,u_bcs,gd,ss,err,gridType,displayTF)
      ! Solves the equation:
      ! 
      !     u_xx + u_yy + u_zz = f
      ! 
      ! to steady state using the Alternating Direction 
      ! Implicit (ADI) method described in:
      ! 
      ! "Douglas, J. Alternating direction methods for 
      ! three space variables. Numer. Math. 4, 41–63 (1962)."
      ! 
      use constants_mod
      use myIO_mod
      use BCs_mod
      use applyBCs_mod
      use griddata_mod
      use rundata_mod
      use solverSettings_mod
      use vectorOps_mod
      use myTriSolver_mod
      use myTriOperator_mod
      use myError_mod
      implicit none

      private

      public :: myADI,solve
      private :: initialize,delete

      public :: solveADI1D ! for 1D testing
      private :: relax1D   ! for 1D testing

      type myADI
        type(myTrisolver),dimension(3) :: triSolver
        type(myTriOperator),dimension(3) :: triOperator
        real(dpn),dimension(:,:,:),allocatable :: lapu
        real(dpn),dimension(:),allocatable :: dtj
        integer,dimension(3) :: s
        integer :: nTimeLevels
        real(dpn) :: alpha,dt
        integer :: gridType
      end type

      interface initialize;  module procedure initializeADI; end interface
      interface delete;      module procedure deleteADI;     end interface
      interface solve;       module procedure solveADI;      end interface

      contains

      subroutine initialize1D(ADI,s,gd,gridType,alpha,dir)
        implicit none
        type(myADI),intent(inout) :: ADI
        integer,dimension(3),intent(in) :: s
        type(griddata),intent(in) :: gd
        integer,intent(in) :: gridType
        real(dpn),intent(in) :: alpha
        integer,intent(in) :: dir
        real(dpn),dimension(:),allocatable :: dhn,dhc
        real(dpn),dimension(:),allocatable :: upDiag,diag,loDiag

        select case (gridType)
        case (1) ! For cell-centered data
          select case (dir)
          case (1); allocate(dhn(s(1)-2)); call getDXn(gd,dhn)
                    allocate(dhc(s(1)-1)); call getDXcc(gd,dhc)
          case (2); allocate(dhn(s(2)-2)); call getDYn(gd,dhn)
                    allocate(dhc(s(2)-1)); call getDYcc(gd,dhc)
          case (3); allocate(dhn(s(3)-2)); call getDZn(gd,dhn)
                    allocate(dhc(s(3)-1)); call getDZcc(gd,dhc)
          case default
            write(*,*) 'Error: dir must = 1,2,3 in initialize1D.';stop
          end select
        case (2) ! For node-based data
          select case (dir)
          case (1); allocate(dhn(s(1)-1)); call getDXn(gd,dhn)
                    allocate(dhc(s(1)));   call getDXcc(gd,dhc)
          case (2); allocate(dhn(s(2)-1)); call getDYn(gd,dhn)
                    allocate(dhc(s(2)));   call getDYcc(gd,dhc)
          case (3); allocate(dhn(s(3)-1)); call getDZn(gd,dhn)
                    allocate(dhc(s(3)));   call getDZcc(gd,dhc)
          case default
            write(*,*) 'Error: dir must = 1,2,3 in initialize1D.';stop
          end select
        case default
          write(*,*) 'Error: gridType must = 1,2 in initialize1D of myADI';stop
        end select

        ! Prep matrix:
        allocate(diag(s(dir)))
        allocate(upDiag(s(dir)-1))
        allocate(loDiag(s(dir)-1))

        call setUpSystem(loDiag,diag,upDiag,0.5d0*ADI%dt*alpha,dhn,dhc,s,dir)
        call initialize(ADI%triOperator(dir),loDiag,diag,upDiag)

        call setUpSystem(loDiag,diag,upDiag,-0.5d0*ADI%dt*alpha,dhn,dhc,s,dir)
        call addIdentity(diag)
        call initialize(ADI%triSolver(dir),loDiag,diag,upDiag)

        deallocate(upDiag,diag,loDiag)
      end subroutine

      subroutine initializeADI(ADI,s,gd,gridType,alpha,Af)
        implicit none
        type(myADI),intent(inout) :: ADI
        integer,dimension(3),intent(in) :: s
        type(griddata),intent(in) :: gd
        real(dpn),intent(in) :: alpha,Af
        integer,intent(in) :: gridType
        integer :: j
        real(dpn) :: hj,h0
        ! Set preliminary parameters
        ADI%gridType = gridType; ADI%alpha = alpha; ADI%s = s

        ! Set multi-scale time steps:
        ADI%nTimeLevels = floor(log(real(s(1)+1))/log(real(2.0,dpn)))
        h0 = getDhMin(gd)
        allocate(ADI%dtj(ADI%nTimeLevels))
        do j = 1,ADI%nTimeLevels
          hj = (2.0**dble(j))*h0;
          ADI%dtj(j) = Af/ADI%alpha*4.0*(hj**2.0)/(PI**2.0)
        enddo

        ! Set up tridiagonal systems:
        allocate(ADI%lapu(s(1),s(2),s(3)))
        call initialize1D(ADI,s,gd,gridType,alpha,1)
        call initialize1D(ADI,s,gd,gridType,alpha,2)
        call initialize1D(ADI,s,gd,gridType,alpha,3)
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
      end subroutine

      subroutine setDt(ADI,dt)
       implicit none
       type(myADI),intent(inout) :: ADI
       real(dpn),intent(in) :: dt
       ADI%dt = dt
      end subroutine

      subroutine interiorAdd(a,b,dir)
       implicit none
       real(dpn),dimension(:,:,:),intent(inout) :: a
       real(dpn),dimension(:,:,:),intent(in) :: b
       integer,intent(in) :: dir
       integer,dimension(3) :: s
       integer :: i,j,k,x,y,z
       select case (dir)
       case (1); x=1;y=0;z=0
       case (2); x=0;y=1;z=0
       case (3); x=0;y=0;z=1
       case default
       write(*,*) 'Error: dir must = 1,2,3 in interiorAdd.'; stop
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

      subroutine addIdentity(diag)
        implicit none
        real(dpn),dimension(:),intent(inout) :: diag
        integer :: j,s
        s = size(diag)
        do j = 2,s-1
          diag(j) = 1.0 + diag(j)
        enddo
      end subroutine

      subroutine relax3D(ADI,u,f,u_bcs,gd)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(dpn),dimension(:,:,:),intent(inout) :: u
        real(dpn),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(griddata),intent(in) :: gd
        ! Locals
        real(dpn),dimension(:,:,:),allocatable :: temp1,temp2,temp3,fstar

        allocate(temp1(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(temp2(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(temp3(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(fstar(ADI%s(1),ADI%s(2),ADI%s(3)))
        fstar = zero; temp2 = zero; temp3 = zero;

        ! Compute (0.5 dt d/dh) u^* for h = x,y,z
        call apply(ADI%triOperator(1),temp1,u,1,0)
        call apply(ADI%triOperator(2),temp2,u,2,0)
        call apply(ADI%triOperator(3),temp3,u,3,0)

        ! ---------- Step in x ----------------
        ! Compute RHS:
        ! fstar = (I + .5 dt d/dx + dt d/dy + dt d/dz)u - dt*f
        call interiorAdd(fstar,u,1)
        call interiorAdd(fstar,temp1,1)
        call interiorAdd(fstar,2.0d0*temp2,1)
        call interiorAdd(fstar,2.0d0*temp3,1)
        call interiorAdd(fstar,-ADI%dt*f,1)

        ! Apply BCs along x
        call applyBCFace(u_bcs,fstar,gd,1)
        call applyBCFace(u_bcs,fstar,gd,4)

        ! Solve (I-0.5 dt d_x) u^* = fstar
        call apply(ADI%triSolver(1),temp1,fstar,1,1)

        ! ---------- Step in y ----------------
        ! Compute RHS:
        ! fstar = un* - .5 dt du/dy
        fstar = temp1 - temp2
        ! fstar = temp1
        ! call interiorAdd(fstar,-temp2,2)


        ! Apply BCs along y
        call applyBCFace(u_bcs,fstar,gd,2)
        call applyBCFace(u_bcs,fstar,gd,5)

        ! Solve (I-0.5 dt d_y) u^* = fstar
        call apply(ADI%triSolver(2),temp1,fstar,2,1)

        ! ---------- Step in z ----------------
        ! Compute RHS:
        ! fstar = un** - .5 dt du/dz
        fstar = temp1 - temp3
        ! fstar = temp1
        ! call interiorAdd(fstar,-temp3,3)

        ! Apply BCs along z
        call applyBCFace(u_bcs,fstar,gd,3)
        call applyBCFace(u_bcs,fstar,gd,6)

        ! Solve (I-0.5 dt d_z) u^* = fstar
        call apply(ADI%triSolver(3),u,fstar,3,1)

        deallocate(temp1,temp2,temp3,fstar)
      end subroutine

      subroutine solveADI(ADI,u,f,u_bcs,gd,ss,err,gridType,displayTF)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(dpn),dimension(:,:,:),intent(inout) :: u
        real(dpn),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(griddata),intent(in) :: gd
        type(solverSettings),intent(in) :: ss
        type(myError),intent(inout) :: err
        integer,intent(in) :: gridType
        logical,intent(in) :: displayTF
        integer,dimension(3) :: s
        real(dpn) :: Af
        integer :: i,j,Nt

        s = shape(f); ADI%s = s
        ! Af = (maxval(f)-minval(f))/2.0d0 ! Amplitude of f
        ! Af = 1.0d-3
        ! Af = 1.0d-2
        Af = 1.0d0
        ! write(*,*) 'Af = ',Af
        ! Alpha, may not be 1.          MAKE SURE
        !                                  |
        !                                  |
        !                                  |
        !                                  V
        call initialize(ADI,s,gd,gridType,one,Af)

        Nt = 5

        do i=1,Nt

          ! Begin V-cycle of multi-scale
          do j = 1,ADI%nTimeLevels
            call setDt(ADI,ADI%dtj(j))
            ADI%dt = 1.0d-4
            call relax3D(ADI,u,f,u_bcs,gd)
          enddo
          do j = ADI%nTimeLevels, 1, -1
            call setDt(ADI,ADI%dtj(j))
            ADI%dt = 1.0d-4
            call relax3D(ADI,u,f,u_bcs,gd)
          enddo

        enddo

        ! select case (ADI%gridType)
        ! case (1); call myCC2CCLap(ADI%lapu,u,gd)
        ! case (2); call myNodeLap(ADI%lapu,u,gd)
        ! end select
        ! call computeError(err,f(2:s(1)-1,2:s(2)-1,2:s(3)-1),ADI%lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))

        ! if (getSubtractMean(ss)) u = u - sum(u)/(max(1,size(u)))

        if (displayTF) then
          write(*,*) 'Multi-scale Time Steps = ',ADI%dtj
          write(*,*) 'Number of multi-scale sweeps = ',Nt
          write(*,*) 'Number of ADI iterations = ',real(Nt,dpn)*ADI%nTimeLevels

          select case (ADI%gridType)
          case (1); call myCC2CCLap(ADI%lapu,u,gd)
          case (2); call myNodeLap(ADI%lapu,u,gd)
          end select
          call computeError(err,f(2:s(1)-1,2:s(2)-1,2:s(3)-1),ADI%lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
          call printMyError(err,'ADI Residuals for '//trim(adjustl(getName(ss))))
        endif

        call delete(ADI)
      end subroutine

      subroutine setUpSystem(loDiag,diag,upDiag,alpha,dhn,dhc,s,dir)
        implicit none
        real(dpn),dimension(:),intent(inout) :: loDiag,diag,upDiag
        real(dpn),intent(in) :: alpha
        real(dpn),dimension(:),intent(in) :: dhn,dhc
        integer,dimension(3),intent(in) :: s
        integer,intent(in) :: dir
        integer :: j

        diag(1) = real(1.0,dpn)
        upDiag(1) = real(0.0,dpn)
        do j=2,s(dir)-1
          loDiag(j-1) = alpha/(dhn(j-1)*dhc(j-1))
          diag(j) = -real(2.0,dpn)*alpha/(dhn(j-1)*dhc(j-1))
          upDiag(j) = alpha/(dhn(j-1)*dhc(j-1))
        enddo
        diag(s(dir)) = real(1.0,dpn)
        loDiag(s(dir)-1) = real(0.0,dpn)
      end subroutine

      ! --------------------- 1D TESTS ---------------------

      subroutine relax1D(ADI,u,f,u_bcs,gd,dir)
        ! Solves
        ! (I - 0.5*dt*A)*uOut = (I + 0.5*dt*A)*uIn - dt*f
        implicit none
        type(myADI),intent(inout) :: ADI
        real(dpn),dimension(:,:,:),intent(inout) :: u
        real(dpn),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(griddata),intent(in) :: gd
        integer,intent(in) :: dir
        real(dpn),dimension(:,:,:),allocatable :: fstar
        ADI%s = shape(f)
        
        allocate(fstar(ADI%s(1),ADI%s(2),ADI%s(3)))
        call initialize1D(ADI,ADI%s,gd,2,ADI%alpha,dir)

        fstar = 0.0d0
        call apply(ADI%triOperator(dir),fstar,u,dir,0)

        call interiorAdd(fstar,u,dir)
        call interiorAdd(fstar,-ADI%dt*f,dir)

        call applyBCFace(u_bcs,fstar,gd,1)
        call applyBCFace(u_bcs,fstar,gd,4)

        call apply(ADI%triSolver(dir),u,fstar,dir,0)
        deallocate(fstar)
      end subroutine

      subroutine solveADI1D(ADI,u,f,u_bcs,gd)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(dpn),dimension(:,:,:),intent(inout) :: u
        real(dpn),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(griddata),intent(in) :: gd
        real(dpn) :: dt,h0,hj
        integer :: i,j,nTimeLevels,Nt

        ADI%s = shape(f)
        nTimeLevels = floor(log(real(ADI%s(1)+1))/log(real(2.0,dpn)))
        Nt = 5
        h0 = getDhMin(gd)

        do i=1,Nt

          ! Begin V-cycle of multi-scale
          do j = 1,nTimeLevels
            hj = (2.0**dble(j))*h0;
            dt = 4.0*(hj**2.0)/(ADI%alpha*PI**2.0);
            call setDt(ADI,dt)
            call relax1D(ADI,u,f,u_bcs,gd,1)
          enddo
          do j = nTimeLevels, 1, -1
            hj = (2.0**dble(j))*h0;
            dt = 4.0*(hj**2.0)/(PI**2.0);
            call setDt(ADI,dt)
            call relax1D(ADI,u,f,u_bcs,gd,1)
          enddo

        enddo

        do j = 1,nTimeLevels
          hj = (2.0**dble(j))*h0;
          dt = 4.0*(hj**2.0)/(PI**2.0);
          write(*,*) 'dt(',j,') = ',dt
        enddo
      end subroutine

      end module