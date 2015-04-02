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
      ! Flags: (_EXPORT_ADI_CONVERGENCE_)
      ! 
      use BCs_mod
      use applyBCs_mod
      use grid_mod
      use del_mod
      use scalarField_mod
      use solverSettings_mod
      use delOps_mod
      use myTriSolver_mod
#ifdef _EXPORT_ADI_CONVERGENCE_
      use myIO_mod
#endif
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
        real(cp),dimension(:,:,:),allocatable :: lapu,r,ftemp
        real(cp),dimension(:),allocatable :: dtj
        integer,dimension(3) :: s
        integer :: nTimeLevels,gridType
        real(cp) :: alpha,dt
        real(cp),dimension(:,:,:),allocatable :: alpha_var
        type(scalarField),dimension(3) :: temp
        logical :: var_coeff = .false.
      end type

      interface init;        module procedure initADI;                      end interface
      interface delete;      module procedure deleteADI;                    end interface
      interface solve;       module procedure solveADI;                     end interface
      interface apply;       module procedure applyADI;                     end interface

      interface addIdentity; module procedure addIdentityInterior;          end interface

      interface setAlpha;    module procedure setAlphaUniform;              end interface
      interface setAlpha;    module procedure setAlphaVariable;             end interface

      contains

      subroutine initSystem(ADI,s,g,dir)
        implicit none
        type(myADI),intent(inout) :: ADI
        integer,dimension(3),intent(in) :: s
        type(grid),intent(in) :: g
        integer,intent(in) :: dir
        real(cp),dimension(:),allocatable :: upDiag,diag,loDiag
        integer :: gt

        ! Prep matrix:
        allocate(diag(s(dir)))
        allocate(upDiag(s(dir)-1))
        allocate(loDiag(s(dir)-1))

        if (s(dir).eq.g%c(dir)%sc) then
          gt = 1
          call setUpSystem(loDiag,diag,upDiag,real(-0.5,cp)*ADI%dt*ADI%alpha,&
           g%c(dir)%dhc,g%c(dir)%dhn,s(dir),gt)
        elseif (s(dir).eq.g%c(dir)%sn) then
          gt = 0
          call setUpSystem(loDiag,diag,upDiag,real(-0.5,cp)*ADI%dt*ADI%alpha,&
           g%c(dir)%dhn,g%c(dir)%dhc,s(dir),gt)
        else
          stop 'Error: shape mismatch in initSystem in myADI.f90'
        endif

        call addIdentity(diag)
        call init(ADI%triSolver(dir),loDiag,diag,upDiag)

        deallocate(upDiag,diag,loDiag)
      end subroutine

      subroutine initADI(ADI,s,g)
        implicit none
        type(myADI),intent(inout) :: ADI
        integer,dimension(3),intent(in) :: s
        type(grid),intent(in) :: g
        integer :: j,smean
        real(cp) :: hj,h0
        ! Set preliminary parameters
        ADI%s = s
            if (ADI%s(1).eq.g%c(1)%sc) then; ADI%gridType = 1
        elseif (ADI%s(1).eq.g%c(1)%sn) then; ADI%gridType = 2
        else; stop 'Error: gridType was not determined in myADI.f90'
        endif

        ! Set multi-scale time steps:
        smean = (s(1)+s(2)+s(3))/3
        ! Try p = 0,1,...,2/dx = 2*M levels. This might be the issue

        h0 = g%dhMin ! Smallest spatial step on grid
        if (allocated(ADI%dtj)) deallocate(ADI%dtj)
        ADI%nTimeLevels = floor(log(real(smean))/log(real(2.0,cp)))
        allocate(ADI%dtj(ADI%nTimeLevels))
        do j = 1,ADI%nTimeLevels
          hj = (real(2.0,cp)**real(j-1,cp))*h0
          ADI%dtj(j) = real(4.0,cp)*(hj**(real(2.0,cp)))/(ADI%alpha*PI**real(2.0,cp))
        enddo

        ! Set up tridiagonal systems:
        allocate(ADI%lapu(s(1),s(2),s(3)))
        allocate(ADI%r(s(1),s(2),s(3)))
        allocate(ADI%ftemp(ADI%s(1),ADI%s(2),ADI%s(3)))
      end subroutine

      subroutine deleteADI(ADI)
       implicit none
       type(myADI),intent(inout) :: ADI
       call delete(ADI%triSolver(1))
       call delete(ADI%triSolver(2))
       call delete(ADI%triSolver(3))
       if (allocated(ADI%lapu)) deallocate(ADI%lapu)
       if (allocated(ADI%r)) deallocate(ADI%r)
       if (allocated(ADI%ftemp)) deallocate(ADI%ftemp)
       if (allocated(ADI%dtj)) deallocate(ADI%dtj)
       if (allocated(ADI%alpha_var)) deallocate(ADI%alpha_var)
      end subroutine

      subroutine relax3D_good_for_uniform_grids_but_not_var_dt(ADI,u,f,u_bcs,g)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        integer,dimension(3) :: s
        type(del) :: d
        ! Locals
        real(cp),dimension(:,:,:),allocatable :: temp1,temp2,temp3,fstar,lapUbc,ftemp,u_bc,u_in
        s = ADI%s
        allocate(temp1(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(temp2(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(temp3(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(fstar(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(u_in(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(u_bc(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(ftemp(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(lapUbc(ADI%s(1),ADI%s(2),ADI%s(3)))
        fstar = real(0.0,cp); temp1 = real(0.0,cp); temp2 = real(0.0,cp); temp3 = real(0.0,cp)

        u_in = u
        call zeroGhostPoints(u_in)

        ! Apply BCs along x
        call applyAllBCs(u_bcs,u,g)
        u_bc = u
        u_bc(2:s(1)-1,2:s(2)-1,2:s(3)-1) = real(0.0,cp)

        ! Compute (0.5 dt d^2/dh^2) u^* for h = x,y,z
        call d%assign(temp1,u,g,2,1,1) ! Must use ghost points on RHS operator
        call d%assign(temp2,u,g,2,2,1) ! Must use ghost points on RHS operator
        call d%assign(temp3,u,g,2,3,1) ! Must use ghost points on RHS operator
        temp1 = temp1*real(0.5,cp)*ADI%dt*ADI%alpha
        temp2 = temp2*real(0.5,cp)*ADI%dt*ADI%alpha
        temp3 = temp3*real(0.5,cp)*ADI%dt*ADI%alpha

        ! ---------- Step in x ----------------
        ! Compute RHS:
        ! fstar = (I + .5 dt d_xx + dt d_yy + dt d_zz)u - dt*f

        fstar = u_in ! fstar includes ghost points on RHS
        fstar = fstar + temp1
        fstar = fstar + real(2.0,cp)*temp2
        fstar = fstar + real(2.0,cp)*temp3

        ftemp = f ! let f be only interior and boundary values:
        call zeroGhostPoints(ftemp)

        fstar = fstar - ADI%dt*ftemp

        call d%assign(lapUbc,u_bc,g,2,1,1)
        fstar = fstar + real(0.5,cp)*ADI%dt*ADI%alpha*lapUbc

        ! Solve (I - 0.5 dt d_xx) u^* = fstar
        call apply(ADI%triSolver(1),temp1,fstar,1,1)
        call applyAllBCs(u_bcs,temp1,g)

        ! ---------- Step in y ----------------
        ! Compute RHS:
        ! fstar = un* - .5 dt d_yy
        ! Apply BCs along x

        fstar = temp1 - temp2
        call d%assign(lapUbc,u_bc,g,2,2,1)
        fstar = fstar + real(0.5,cp)*ADI%dt*ADI%alpha*lapUbc

        ! Solve (I - 0.5 dt d_yy) u^* = fstar
        call apply(ADI%triSolver(2),temp1,fstar,2,1)
        call applyAllBCs(u_bcs,temp1,g)

        ! ---------- Step in z ----------------
        ! Compute RHS:
        ! fstar = un** - .5 dt d_zz
        ! Apply BCs along y
        fstar = temp1 - temp3
        call d%assign(lapUbc,u_bc,g,2,3,1)
        fstar = fstar + real(0.5,cp)*ADI%dt*ADI%alpha*lapUbc

        ! Solve (I - 0.5 dt d_zz) u^* = fstar
        call apply(ADI%triSolver(3),u,fstar,3,1)
        call applyAllBCs(u_bcs,u,g)

        deallocate(temp1,temp2,temp3,fstar)
        deallocate(lapUbc,ftemp,u_bc,u_in)
      end subroutine

      subroutine relax3D(ADI,u,f,u_bcs,g)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        integer,dimension(3) :: s
        type(del) :: d
        ! Locals
        real(cp),dimension(:,:,:),allocatable :: temp1,temp2,temp3,fstar,lapUbc,ftemp,u_bc,u_in
        s = ADI%s
        allocate(temp1(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(temp2(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(temp3(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(fstar(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(u_in(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(u_bc(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(ftemp(ADI%s(1),ADI%s(2),ADI%s(3)))
        allocate(lapUbc(ADI%s(1),ADI%s(2),ADI%s(3)))
        fstar = real(0.0,cp); temp1 = real(0.0,cp); temp2 = real(0.0,cp); temp3 = real(0.0,cp)

        u_in = u
        call zeroGhostPoints(u_in)

        ! Apply BCs along x
        call applyAllBCs(u_bcs,u,g)

        ! Compute (0.5 dt d^2/dh^2) u^* for h = x,y,z
        call d%assign(temp1,u,g,2,1,1) ! Must use ghost points on RHS operator
        call d%assign(temp2,u,g,2,2,1) ! Must use ghost points on RHS operator
        call d%assign(temp3,u,g,2,3,1) ! Must use ghost points on RHS operator
        temp1 = temp1*real(0.5,cp)*ADI%dt*ADI%alpha
        temp2 = temp2*real(0.5,cp)*ADI%dt*ADI%alpha
        temp3 = temp3*real(0.5,cp)*ADI%dt*ADI%alpha

        ! ---------- Step in x ----------------
        ! Compute RHS:
        ! fstar = (I + .5 dt d_xx + dt d_yy + dt d_zz)u - dt*f

        fstar = u_in ! fstar includes ghost points on RHS
        fstar = fstar + temp1
        fstar = fstar + real(2.0,cp)*temp2
        fstar = fstar + real(2.0,cp)*temp3

        ftemp = f ! let f be only interior and boundary values:
        call zeroGhostPoints(ftemp)

        fstar = fstar - ADI%dt*ftemp

        u_bc = u
        u_bc(2:s(1)-1,2:s(2)-1,2:s(3)-1) = real(0.0,cp)
        call d%assign(lapUbc,u_bc,g,2,1,1)
        fstar = fstar + real(0.5,cp)*ADI%dt*ADI%alpha*lapUbc

        ! Solve (I - 0.5 dt d_xx) u^* = fstar
        call apply(ADI%triSolver(1),temp1,fstar,1,1)
        call applyAllBCs(u_bcs,temp1,g)

        ! ---------- Step in y ----------------
        ! Compute RHS:
        ! fstar = un* - .5 dt d_yy
        ! Apply BCs along x

        fstar = temp1 - temp2
        u_bc = temp1
        u_bc(2:s(1)-1,2:s(2)-1,2:s(3)-1) = real(0.0,cp)
        call d%assign(lapUbc,u_bc,g,2,2,1)
        fstar = fstar + real(0.5,cp)*ADI%dt*ADI%alpha*lapUbc

        ! Solve (I - 0.5 dt d_yy) u^* = fstar
        call apply(ADI%triSolver(2),temp1,fstar,2,1)
        call applyAllBCs(u_bcs,temp1,g)

        ! ---------- Step in z ----------------
        ! Compute RHS:
        ! fstar = un** - .5 dt d_zz
        ! Apply BCs along y
        fstar = temp1 - temp3
        u_bc = temp1
        u_bc(2:s(1)-1,2:s(2)-1,2:s(3)-1) = real(0.0,cp)
        call d%assign(lapUbc,u_bc,g,2,3,1)
        fstar = fstar + real(0.5,cp)*ADI%dt*ADI%alpha*lapUbc

        ! Solve (I - 0.5 dt d_zz) u^* = fstar
        call apply(ADI%triSolver(3),u,fstar,3,1)
        call applyAllBCs(u_bcs,u,g)

        deallocate(temp1,temp2,temp3,fstar)
        deallocate(lapUbc,ftemp,u_bc,u_in)
      end subroutine

      subroutine applyADI(ADI,u,f,u_bcs,g,ss,err,displayTF)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(in) :: ss
        type(myError),intent(inout) :: err
        logical,intent(in) :: displayTF
        integer,dimension(3) :: s

        s = shape(f); ADI%s = s
        call initADI(ADI,s,g)

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

      subroutine solveADI(ADI,u,f,u_bcs,g,ss,norms,displayTF)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(in) :: ss
        type(myError),intent(inout) :: norms
        logical,intent(in) :: displayTF
        integer,dimension(3) :: s
        integer :: i,j,Nt,maxIt
#ifdef _EXPORT_ADI_CONVERGENCE_
        integer :: NU
#endif

        s = shape(f); ADI%s = s
        call initADI(ADI,s,g)

        ! Nt = maxval((/getMaxIterations(ss)/ADI%nTimeLevels/2, 1/))
        Nt = getMaxIterations(ss)
        ! Nt = 1

        ! The following was not used when testing the three poisson
        ! methods, and good results were obtained.
        ! This is being temporarily introduced because non-uniform grids
        ! are being tested
        ADI%ftemp = f

        maxIt = 0

#ifdef _EXPORT_ADI_CONVERGENCE_
        NU = newAndOpen('out\','norms_ADI')
#endif
        do i=1,Nt

          ! Begin V-cycle of multi-scale
          do j = 1,ADI%nTimeLevels
            ! call setDt(ADI,ADI%dtj(j))
            call initSystem(ADI,s,g,1)
            call initSystem(ADI,s,g,2)
            call initSystem(ADI,s,g,3)
            call relax3D(ADI,u,ADI%ftemp,u_bcs,g)

#ifdef _EXPORT_ADI_CONVERGENCE_
            select case (ADI%gridType)
            case (1); call CC2CCLap(ADI%lapu,u,g)
            case (2); call myNodeLap(ADI%lapu,u,g)
            end select
            ADI%r = ADI%lapu - ADI%ftemp
            call zeroGhostPoints(ADI%r)
            call compute(norms,real(0.0,cp),ADI%r)
            write(NU,*) getL1(norms),getL2(norms),getLinf(norms)
#endif
            maxIt = maxIt + 1
          enddo
          if (maxIt.ge.Nt) exit
          do j = ADI%nTimeLevels, 1, -1
            ! call setDt(ADI,ADI%dtj(j))
            call initSystem(ADI,s,g,1)
            call initSystem(ADI,s,g,2)
            call initSystem(ADI,s,g,3)
            call relax3D(ADI,u,ADI%ftemp,u_bcs,g)

#ifdef _EXPORT_ADI_CONVERGENCE_
            select case (ADI%gridType)
            case (1); call CC2CCLap(ADI%lapu,u,g)
            case (2); call myNodeLap(ADI%lapu,u,g)
            end select
            ADI%r = ADI%lapu - ADI%ftemp
            call zeroGhostPoints(ADI%r)
            call compute(norms,real(0.0,cp),ADI%r)
            write(NU,*) getL1(norms),getL2(norms),getLinf(norms)
#endif
            maxIt = maxIt + 1
          enddo
          if (maxIt.ge.Nt) exit
        enddo

#ifdef _EXPORT_ADI_CONVERGENCE_
        close(NU)
#endif

        ! This step is not necessary if mean(f) = 0 and all BCs are Neumann.
        if (allNeumann(u_bcs)) then
          u = u - sum(u)/(max(1,size(u)))
        endif

        ! Okay for SOR alone when comparing with u_exact, but not okay for MG
        ! if (.not.allNeumann(u_bcs)) then
        !   u = u - sum(u)/(max(1,size(u)))
        ! endif

        if (displayTF) then
          write(*,*) 'ADI Multi-scale Time Steps = ',ADI%dtj
          write(*,*) 'Number of multi-scale sweeps = ',Nt
          write(*,*) 'Number of multi-scale time levels = ',ADI%nTimeLevels
          write(*,*) 'Number of ADI iterations = ',maxIt
          select case (ADI%gridType)
          case (1); call CC2CCLap(ADI%lapu,u,g)
          case (2); call myNodeLap(ADI%lapu,u,g)
          end select
          ADI%r = ADI%lapu - ADI%ftemp
          call zeroGhostPoints(ADI%r)
          call compute(norms,real(0.0,cp),ADI%r)
          call print(norms,'ADI Residuals for '//trim(adjustl(getName(ss))))
        endif

        call delete(ADI)
      end subroutine

      subroutine setUpSystem(loDiag,diag,upDiag,alpha,dhp,dhd,s,gt)
        ! dhp = primary grid (data lives on primary grid)
        ! dhd = dual    grid (data lives on primary grid)
        ! diag = size(f)
        ! size(dhp) = size(f)-1
        implicit none
        real(cp),dimension(:),intent(inout) :: loDiag,diag,upDiag
        real(cp),intent(in) :: alpha
        real(cp),dimension(:),intent(in) :: dhp,dhd
        integer,intent(in) :: s,gt
        integer :: i
        ! write(*,*) 's(diag) = ',size(diag)
        ! write(*,*) 's(loDiag) = ',size(loDiag)
        ! write(*,*) 's(upDiag) = ',size(upDiag)
        ! write(*,*) 's(dhp) = ',size(dhp)
        ! write(*,*) 's(dhd) = ',size(dhd)
        ! write(*,*) 's = ',s
        ! write(*,*) 'gt = ',gt
        ! write(*,*) 'dhp = ',dhp
        ! write(*,*) 'dhd = ',dhd
        ! stop 'printed'

        diag(1) = real(0.0,cp)
        upDiag(1) = real(0.0,cp)
        do i=2,s-1
          loDiag(i-1) = alpha/(dhp(i-1)*dhd(i-1+gt))
          diag(i) = -(alpha/dhp(i-1)+alpha/dhp(i))/dhd(i-1+gt)
          upDiag(i) = alpha/(dhp(i)*dhd(i-1+gt))
        enddo
        diag(s) = real(0.0,cp)
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

      subroutine addIdentityInterior(diag)
        implicit none
        real(cp),dimension(:),intent(inout) :: diag
        integer :: j,s
        s = size(diag)
        do j = 2,s-1
          diag(j) = real(1.0,cp) + diag(j)
        enddo
      end subroutine

      end module