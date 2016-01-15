      module SOR_mod
      ! call SORSolver(SOR,u,f,u_bcs,m,ss,norm,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), mesh (m)
      ! and solver settings (ss) using the iterative Successive Over 
      ! Realxation (SOR) method
      ! 
      ! Note that the variant of Gauss-Seidel/SOR called
      ! "red-black" Gauss-Seidel is used, where the fields are 
      ! traversed in a 3D checkerboarding manner.
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     m            = contains mesh information (dhc,dhn)
      !     ss           = solver settings (specifies max iterations, tolerance etc.)
      !     norm         = Ln norms of residual
      !     displayTF    = print residuals to screen (T,F)
      ! 
      ! Flags: (_PARALLELIZE_SOR_,_EXPORT_SOR_CONVERGENCE_)

      use grid_mod
      use mesh_mod
      use apply_BCs_mod
      use apply_stitches_mod
      use BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod

      use solverSettings_mod
#ifdef _EXPORT_SOR_CONVERGENCE_
      use IO_tools_mod
#endif
      implicit none

      private
      public :: SORSolver,solve
      public :: init,delete

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif
       real(cp),parameter :: PI = 3.14159265358979_cp

      logical, parameter :: useGaussSeidel = .true.

      type SORSolver
        character(len=5) :: name
        type(mesh) :: p,d ! Primary / Dual grids
        type(SF) :: lapu,res,r ! laplacian, residual, coefficient
        real(cp) :: omega
        integer,dimension(3) :: gt,s
        logical :: setCoeff
      end type
      
      interface init;        module procedure initSOR;       end interface
      interface delete;      module procedure deleteSOR;     end interface
      interface solve;       module procedure solveSOR;      end interface

      interface init_r;      module procedure init_r_RF;     end interface
      interface init_r;      module procedure init_r_SF;     end interface

      contains

      subroutine initSOR(SOR,u,m)
        implicit none
        type(SORSolver),intent(inout) :: SOR
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        integer,dimension(3) :: s
        integer :: Nx,Ny,Nz,i,t
        
        s = u%RF(1)%s
        SOR%s = s
        call init(SOR%p,m)
        call init(SOR%d,m)

        if (u%is_CC) then
          do t=1,u%s; do i=1,3
            call init(SOR%p%g(t),m%g(t)%c(i)%hc,i) ! mesh made from cc --> p%dhn is dhc
            SOR%gt(i) = 1
          enddo; enddo
        elseif(u%is_Node) then
          do t=1,u%s; do i=1,3
            call init(SOR%p%g(t),m%g(t)%c(i)%hc,i) ! mesh made from cc --> p%dhn is dhc
              SOR%gt(i) = 0
          enddo; enddo
        elseif (u%is_Face) then
        elseif (u%is_Edge) then
        else; stop 'Error: mesh type was not determined in SOR.f90'
        endif

        call init(SOR%lapu,u)
        call init(SOR%res,u)
        call init(SOR%r,u)
        call init_r(SOR%r,SOR%p,SOR%d,SOR%gt)

        if (useGaussSeidel) then
          SOR%omega = 1.0_cp
          SOR%name = 'GS '
        else
          Nx = s(1); Ny = s(2); Nz = s(3)
          SOR%omega = 2.0_cp/(1.0_cp + sqrt(1.0_cp - & 
           ((cos(PI/real(Nx+1,cp)) + cos(PI/real(Ny+1,cp)) + &
             cos(PI/real(Nz+1,cp)))/3.0_cp)**2.0_cp))
          SOR%name = 'SOR'
        endif
      end subroutine

      subroutine deleteSOR(SOR)
        implicit none
        type(SORSolver),intent(inout) :: SOR
        call delete(SOR%p)
        call delete(SOR%d)
        call delete(SOR%lapu)
        call delete(SOR%res)
        call delete(SOR%r)
      end subroutine


      subroutine solveSOR(SOR,u,f,m,ss,norm,displayTF)
        implicit none
        type(SORSolver),intent(inout) :: SOR
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        type(solverSettings),intent(inout) :: ss
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        ! Locals
        integer :: ijk
        logical :: TF,continueLoop
        integer :: maxIterations
#ifdef _EXPORT_SOR_CONVERGENCE_
        integer :: NU
#endif

        call solverSettingsSet(ss)
        ijk = 0

        ! Boundaries
        call apply_BCs(u,m) ! Necessary with ghost nodes

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif
        continueLoop = .true.

#ifdef _EXPORT_SOR_CONVERGENCE_
        NU = newAndOpen('out\','norm_SOR')
#endif

        do while (continueLoop.and.TF)
          ijk = ijk + 1

          ! THE ORDER OF THESE ROUTINE CALLS IS IMPORTANT. DO NOT CHANGE.

#ifdef _PARALLELIZE_SOR_
          !$OMP PARALLEL

#endif

          call innerLoop(u,f,m,SOR%r,SOR,(/0,0,0/)) ! Even in odd plane

          call innerLoop(u,f,m,SOR%r,SOR,(/1,0,0/)) ! Even in even plane
          call innerLoop(u,f,m,SOR%r,SOR,(/0,1,0/)) ! Even in even plane
          call innerLoop(u,f,m,SOR%r,SOR,(/0,0,1/)) ! Even in even plane


#ifdef _PARALLELIZE_SOR_
          !$OMP END PARALLEL

#endif

#ifdef _PARALLELIZE_SOR_
          !$OMP PARALLEL

#endif

          call innerLoop(u,f,m,SOR%r,SOR,(/1,1,1/)) ! Odd in odd plane

          call innerLoop(u,f,m,SOR%r,SOR,(/0,1,1/)) ! Odd in even plane
          call innerLoop(u,f,m,SOR%r,SOR,(/1,0,1/)) ! Odd in even plane
          call innerLoop(u,f,m,SOR%r,SOR,(/1,1,0/)) ! Odd in even plane

#ifdef _PARALLELIZE_SOR_
          !$OMP END PARALLEL

#endif

          call apply_BCs(u,m)

          if (getMinToleranceTF(ss)) then
            call lap(SOR%lapu,u,m)
            call subtract(SOR%res,SOR%lapu,f)
            call zeroGhostPoints(SOR%res)
            call compute(norm,SOR%res,m)
            call setTolerance(ss,norm%L2)
          endif

#ifdef _EXPORT_SOR_CONVERGENCE_
            call lap(SOR%lapu,u,m)
            call subtract(SOR%res,SOR%lapu,f)
            call zeroGhostPoints(SOR%res)
            call compute(norm,SOR%res,m)
            write(NU,*) norm%L1,norm%L2,norm%Linf
#endif

          call setIteration(ss,ijk)

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          if (.not.continueLoop) exit
          ! ************************************************************************************
        enddo

#ifdef _EXPORT_SOR_CONVERGENCE_
        close(NU)
#endif
        
        ! Subtract mean (for Pressure Poisson)
        ! Okay for SOR alone when comparing with u_exact, but not okay for MG
        ! This step is not necessary if mean(f) = 0 and all BCs are Neumann.
        if (u%all_Neumann) call subtract(u,mean(u))
        if (displayTF) then
          write(*,*) 'SOR parameter = ',SOR%omega
          write(*,*) '(Final,max) '//SOR%name//' iteration = ',ijk,maxIterations

          call lap(SOR%lapu,u,m)
          call subtract(SOR%res,SOR%lapu,f)
          call zeroGhostPoints(SOR%res)
          call compute(norm,SOR%res,m)
          call print(norm,SOR%name//' Residuals for '//trim(adjustl(getName(ss))))
        endif

      end subroutine

      subroutine innerLoop(u,f,m,r,SOR,odd)
        implicit none
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f,r
        type(mesh),intent(in) :: m
        type(SORSolver),intent(in) :: SOR
        integer,dimension(3),intent(in) :: odd
        integer :: i
        do i=1,m%s
          call redBlack(u%RF(i)%f,f%RF(i)%f,r%RF(i)%f,u%RF(i)%s,&
          SOR%p%g(i)%c(1)%dhn,SOR%p%g(i)%c(2)%dhn,SOR%p%g(i)%c(3)%dhn,&
          SOR%d%g(i)%c(1)%dhn,SOR%d%g(i)%c(2)%dhn,SOR%d%g(i)%c(3)%dhn,&
          SOR%omega,SOR%gt,odd)
          ! call apply_stitches(u,m)
        enddo
      end subroutine

      subroutine redBlack(u,f,r,s,dxp,dyp,dzp,dxd,dyd,dzd,omega,gt,odd)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f,r
        integer,dimension(3),intent(in) :: s,odd
        real(cp),dimension(:),intent(in) :: dxp,dyp,dzp,dxd,dyd,dzd
        real(cp),intent(in) :: omega
        integer,dimension(3),intent(in) :: gt
        integer :: i,j,k

#ifdef _PARALLELIZE_SOR_
        !$OMP DO

#endif

        do k=2+odd(3),s(3)-1,2
          do j=2+odd(2),s(2)-1,2
            do i=2+odd(1),s(1)-1,2

              u(i,j,k) = u(i,j,k)*(1.0_cp-omega) + &
                 omega*( u(i-1,j,k)/(dxp(i-1) * dxd(i-1+gt(1))) + &
                         u(i+1,j,k)/(dxp( i ) * dxd(i-1+gt(1))) + &
                         u(i,j-1,k)/(dyp(j-1) * dyd(j-1+gt(2))) + &
                         u(i,j+1,k)/(dyp( j ) * dyd(j-1+gt(2))) + &
                         u(i,j,k-1)/(dzp(k-1) * dzd(k-1+gt(3))) + &
                         u(i,j,k+1)/(dzp( k ) * dzd(k-1+gt(3))) &
                       - f(i,j,k) )/r(i,j,k)

            enddo
          enddo
        enddo

#ifdef _PARALLELIZE_SOR_
        !$OMP END DO

#endif
      end subroutine

      subroutine init_r_SF(r,p,d,gt)
        implicit none
        type(SF),intent(inout) :: r
        type(mesh),intent(in) :: p,d
        integer,dimension(3),intent(in) :: gt
        integer :: i
        do i=1,p%s
          call init_r(r%RF(i)%f,r%RF(i)%s,&
            p%g(i)%c(1)%dhn,p%g(i)%c(2)%dhn,p%g(i)%c(3)%dhn,&
            d%g(i)%c(1)%dhn,d%g(i)%c(2)%dhn,d%g(i)%c(3)%dhn,gt)
        enddo
      end subroutine

      subroutine init_r_RF(r,s,dxp,dyp,dzp,dxd,dyd,dzd,gt)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: r
        integer,dimension(3),intent(in) :: s
        real(cp),dimension(:),intent(in) :: dxp,dyp,dzp,dxd,dyd,dzd
        integer,dimension(3),intent(in) :: gt
        integer :: i,j,k

#ifdef _PARALLELIZE_SOR_
        !$OMP DO

#endif

        do k=2,s(3)-1
          do j=2,s(2)-1
            do i=2,s(1)-1
              r(i,j,k) = 1.0_cp/dxd(i-1+gt(1))*(1.0_cp/dxp(i) + 1.0_cp/dxp(i-1)) + & 
                         1.0_cp/dyd(j-1+gt(2))*(1.0_cp/dyp(j) + 1.0_cp/dyp(j-1)) + & 
                         1.0_cp/dzd(k-1+gt(3))*(1.0_cp/dzp(k) + 1.0_cp/dzp(k-1))
            enddo
          enddo
        enddo

#ifdef _PARALLELIZE_SOR_
        !$OMP END DO

#endif
      end subroutine

!       subroutine redBlackSigma(u,f,sigma,s,dxp,dyp,dzp,dxd,dyd,dzd,omega,gt,odd)
!         ! sigx,sigy,sigz are already interpolated to the correct location here
!         implicit none
!         real(cp),dimension(:,:,:),intent(inout) :: u
!         real(cp),dimension(:,:,:),intent(in) :: f
!         type(VF),intent(in) :: sigma
!         integer,dimension(3) :: s,odd
!         real(cp),dimension(:),intent(in) :: dxp,dyp,dzp,dxd,dyd,dzd
!         real(cp),intent(in) :: omega
!         integer,dimension(3),intent(in) :: gt
!         integer :: i,j,k
!         real(cp) :: r

! #ifdef _PARALLELIZE_SOR_
!         !$OMP DO PRIVATE(r)

! #endif

!         do k=2+odd(3),s(3)-1,2
!           do j=2+odd(2),s(2)-1,2
!             do i=2+odd(1),s(1)-1,2
!                 r = 1.0_cp/dxd(i-1+gt(1))*(sigma%x(i,j,k)/dxp(i) + sigma%x(i-1,j,k)/dxp(i-1)) + & 
!                     1.0_cp/dyd(j-1+gt(2))*(sigma%y(i,j,k)/dyp(j) + sigma%y(i,j-1,k)/dyp(j-1)) + & 
!                     1.0_cp/dzd(k-1+gt(3))*(sigma%z(i,j,k)/dzp(k) + sigma%z(i,j,k-1)/dzp(k-1))

!                 u(i,j,k) = u(i,j,k)*(1.0_cp-omega) + &

!                    omega*( u(i-1,j,k)*(sigma%x(i-1,j,k)/(dxp(i-1) * dxd(i-1+gt(1)))) + &
!                            u(i+1,j,k)*(sigma%x(i, j ,k)/(dxp( i ) * dxd(i-1+gt(1)))) + &
!                            u(i,j-1,k)*(sigma%y(i,j-1,k)/(dyp(j-1) * dyd(j-1+gt(2)))) + &
!                            u(i,j+1,k)*(sigma%y(i, j ,k)/(dyp( j ) * dyd(j-1+gt(2)))) + &
!                            u(i,j,k-1)*(sigma%z(i,j,k-1)/(dzp(k-1) * dzd(k-1+gt(3)))) + &
!                            u(i,j,k+1)*(sigma%z(i,j, k )/(dzp( k ) * dzd(k-1+gt(3)))) &
!                          - f(i,j,k) )/r

!             enddo
!           enddo
!         enddo

! #ifdef _PARALLELIZE_SOR_
!         !$OMP END DO

! #endif
!       end subroutine

      end module