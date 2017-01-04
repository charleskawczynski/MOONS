      module SOR_mod
      ! call SORSolver(SOR,u,f,u_bcs,m,n,norm,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), mesh (m)
      ! and a number of iterations (n) using the iterative Successive Over
      ! Realxation (SOR) method
      !
      ! Note that the variant of Gauss-Seidel/SOR called
      ! "red-black" Gauss-Seidel is used, where the fields are
      ! traversed in a 3D checkerboarding manner.
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to boundary_conditions_mod for more info.
      !     m            = contains mesh information (dhc,dhn)
      !     norm         = Ln norms of residual
      !     displayTF    = print residuals to screen (T,F)
      !
      ! Flags: (_PARALLELIZE_SOR_,_EXPORT_SOR_CONVERGENCE_)
      use current_precision_mod
      use grid_mod
      use mesh_mod
      use apply_BCs_mod
      use boundary_conditions_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use constants_mod

#ifdef _EXPORT_SOR_CONVERGENCE_
      use IO_tools_mod
#endif
      implicit none

      private
      public :: SORSolver,solve
      public :: init,delete

      logical, parameter :: useGaussSeidel = .true.

      type SORSolver
        character(len=5) :: name
        type(mesh) :: p,d ! Primary / Dual grids
        type(SF) :: lapu,res,r ! laplacian, residual, coefficient
        real(cp) :: omega
        type(SF) :: vol
        integer,dimension(3) :: gt,s
        logical :: setCoeff = .false.
      end type

      interface init;        module procedure initSOR;       end interface
      interface delete;      module procedure deleteSOR;     end interface
      interface solve;       module procedure solveSOR;      end interface

      interface init_r;      module procedure init_r_GF;     end interface
      interface init_r;      module procedure init_r_SF;     end interface

      contains

      subroutine initSOR(SOR,u,m)
        implicit none
        type(SORSolver),intent(inout) :: SOR
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        integer,dimension(3) :: s
        integer :: Nx,Ny,Nz,i,t

        s = u%BF(1)%GF%s
        SOR%s = s
        call init(SOR%p,m)
        call init(SOR%d,m)
        call init(SOR%vol,u)
        call volume(SOR%vol,m)

        if (is_CC(u%DL)) then
          do t=1,u%s; do i=1,3
            call init(SOR%p%B(t)%g,m%B(t)%g%c(i)%hc%f,i) ! mesh made from cc --> p%dhn is dhc
            SOR%gt(i) = 1
          enddo; enddo
        elseif(is_Node(u%DL)) then
          do t=1,u%s; do i=1,3
            call init(SOR%p%B(t)%g,m%B(t)%g%c(i)%hc%f,i) ! mesh made from cc --> p%dhn is dhc
              SOR%gt(i) = 0
          enddo; enddo
        elseif (is_Face(u%DL)) then
        elseif (is_Edge(u%DL)) then
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
        call delete(SOR%vol)
      end subroutine

      subroutine solveSOR(SOR,u,f,m,n,norm,displayTF)
        implicit none
        type(SORSolver),intent(inout) :: SOR
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        ! Locals
        integer :: i
#ifdef _EXPORT_SOR_CONVERGENCE_
        integer :: NU
#endif

        ! Boundaries
        call apply_BCs(u) ! Necessary with ghost nodes

#ifdef _EXPORT_SOR_CONVERGENCE_
        NU = new_and_open('out\','norm_SOR')
#endif

        do i = 1,n

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

          call apply_BCs(u)

#ifdef _EXPORT_SOR_CONVERGENCE_
            call lap(SOR%lapu,u,m)
            call subtract(SOR%res,SOR%lapu,f)
            call assign_ghost_XPeriodic(SOR%res,0.0_cp)
            call compute(norm,SOR%res,SOR%vol,m%volume)
            write(NU,*) norm%L1,norm%L2,norm%Linf
#endif

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
          write(*,*) '(Final,max) SOR iterations = ',i-1,n

          call lap(SOR%lapu,u,m)
          call subtract(SOR%res,SOR%lapu,f)
          call assign_ghost_XPeriodic(SOR%res,0.0_cp)
          call compute(norm,SOR%res,SOR%vol,m%volume)
          call print(norm,'SOR Residuals')
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
          call redBlack(u%BF(i)%GF%f,f%BF(i)%GF%f,r%BF(i)%GF%f,u%BF(i)%GF%s,&
          SOR%p%B(i)%g%c(1)%dhn%f,&
          SOR%p%B(i)%g%c(2)%dhn%f,&
          SOR%p%B(i)%g%c(3)%dhn%f,&
          SOR%d%B(i)%g%c(1)%dhn%f,&
          SOR%d%B(i)%g%c(2)%dhn%f,&
          SOR%d%B(i)%g%c(3)%dhn%f,&
          SOR%omega,SOR%gt,odd)
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
          call init_r(r%BF(i)%GF%f,r%BF(i)%GF%s,&
            p%B(i)%g%c(1)%dhn%f,&
            p%B(i)%g%c(2)%dhn%f,&
            p%B(i)%g%c(3)%dhn%f,&
            d%B(i)%g%c(1)%dhn%f,&
            d%B(i)%g%c(2)%dhn%f,&
            d%B(i)%g%c(3)%dhn%f,gt)
        enddo
      end subroutine

      subroutine init_r_GF(r,s,dxp,dyp,dzp,dxd,dyd,dzd,gt)
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