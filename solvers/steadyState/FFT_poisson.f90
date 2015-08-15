      module FFT_poisson_mod
      ! call FFT_poisson(u,f,u_bcs,g,ss,gridType,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), grid (g)
      ! and solver settings (ss) using a Fast Fourier Transform (FFT) 
      ! method.
      ! 
      ! Although grids may have different spacing in each direction,
      ! grid sizes along each direction are assumed uniform.
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     g            = contains grid information (dhc,dhn)
      ! 
      ! Flags: (_PARALLELIZE_FFT_)

      use grid_mod
      use BCs_mod
      use applyBCs_mod
      use norms_mod
      use solversettings_mod
      use SF_mod
      use ops_discrete_mod
      use ops_aux_mod
      use IO_scalarFields_mod
      use dct_mod
      use idct_mod
      implicit none

      private
      public :: FFTSolver,solve
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
       real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

      type FFTSolver
        character(len=5) :: name
        real(cp) :: dx2,dy2,dz2 ! Grid spacing (assumed uniform)
        integer :: Nx,Ny,Nz ! Number of cells
        type(SF) :: f,res
        integer,dimension(3) :: s
      end type
      
      interface init;        module procedure initFFT;       end interface
      interface delete;      module procedure deleteFFT;     end interface
      interface solve;       module procedure solveFFT;      end interface

      contains

      subroutine initFFT(FFT,s,g)
        implicit none
        type(FFTSolver),intent(inout) :: FFT
        integer,dimension(3),intent(in) :: s
        type(grid),intent(in) :: g
        FFT%s = s
        FFT%dx2 = g%c(1)%dhn(1)**2.0_cp
        FFT%dy2 = g%c(2)%dhn(1)**2.0_cp
        FFT%dz2 = g%c(3)%dhn(1)**2.0_cp
        FFT%Nx = g%c(1)%sc-2
        FFT%Ny = g%c(2)%sc-2
        FFT%Nz = g%c(3)%sc-2
        call init(FFT%f,FFT%s)
        call init(FFT%res,FFT%s)
        FFT%name = 'FFT'
      end subroutine

      subroutine deleteFFT(FFT)
        implicit none
        type(FFTSolver),intent(inout) :: FFT
        call delete(FFT%res)
        call delete(FFT%f)
      end subroutine

      subroutine solveFFT(FFT,u,f,u_bcs,g,ss,norm,displayTF,dir)
        implicit none
        type(FFTSolver),intent(inout) :: FFT
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        integer,intent(in) :: dir
        integer :: i,j,k
        real(cp) :: cosi,cosj,cosk
        integer,dimension(3) :: s
        s = shape(f)
        call init(FFT,s,g)

        call assign(FFT%f,f)

        select case (dir)
        case (1); call dct(FFT%f%phi(:,2:s(2)-1,:),2,1)
                  call dct(FFT%f%phi(:,:,2:s(3)-1),3,1)
        case (2); call dct(FFT%f%phi(2:s(1)-1,:,:),1,1)
                  call dct(FFT%f%phi(:,:,2:s(3)-1),3,1)
        case (3); call dct(FFT%f%phi(2:s(1)-1,:,:),1,1)
                  call dct(FFT%f%phi(:,2:s(2)-1,:),2,1)
        case default
        stop 'Error: dir must = 1,2,3 in solveFFT in FFT_poisson.f90'
        end select

        ! THESE FORMULAS ARE ONLY VALID WHEN DX1 = DX2 WHERE DIR = 3
        select case (dir)
        case (1)
          !$OMP PARALLEL DO PRIVATE(cosj,cosk)
          do k=1,FFT%s(3); do j=2,FFT%s(2)-1; do i=2,FFT%s(1)-1
          cosj = cos(PI*real(j-2,cp)/real(FFT%Ny,cp))
          cosk = cos(PI*real(k-2,cp)/real(FFT%Nz,cp))
          if (.not.((i.eq.2).and.(j.eq.2))) then
            u(i,j,k) = 0.5_cp*FFT%f%phi(i,j,k)/(cosj+cosk-2.0_cp)*FFT%dy2
          endif
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO PRIVATE(cosi,cosk)
          do k=2,FFT%s(3)-1; do j=1,FFT%s(2); do i=2,FFT%s(1)-1
          cosi = cos(PI*real(i-2,cp)/real(FFT%Nx,cp))
          cosk = cos(PI*real(k-2,cp)/real(FFT%Nz,cp))
          if (.not.((i.eq.2).and.(k.eq.2))) then
            u(i,j,k) = 0.5_cp*FFT%f%phi(i,j,k)/(cosi+cosk-2.0_cp)*FFT%dx2
          endif
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO PRIVATE(cosi,cosj)
          do k=1,FFT%s(3); do j=2,FFT%s(2)-1; do i=2,FFT%s(1)-1
          cosi = cos(PI*real(i-2,cp)/real(FFT%Nx,cp))
          cosj = cos(PI*real(j-2,cp)/real(FFT%Ny,cp))
          if (.not.((i.eq.2).and.(j.eq.2))) then
            u(i,j,k) = 0.5_cp*FFT%f%phi(i,j,k)/(cosi+cosj-2.0_cp)*FFT%dx2
          endif
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in solveFFT in FFT_poisson.f90'
        end select

        ! 'Pin down pressure'
        select case (dir)
        case (1); u(:,2,2) = 0.0_cp
        case (2); u(2,:,2) = 0.0_cp
        case (3); u(2,2,:) = 0.0_cp
        case default
        stop 'Error: dir must = 1,2,3 in solveFFT in FFT_poisson.f90'
        end select

        select case (dir)
        case (1); call idct(u(:,2:s(2)-1,:),2,1)
                  call idct(u(:,:,2:s(3)-1),3,1)
        case (2); call idct(u(2:s(1)-1,:,:),1,1)
                  call idct(u(:,:,2:s(3)-1),3,1)
        case (3); call idct(u(2:s(1)-1,:,:),1,1)
                  call idct(u(:,2:s(2)-1,:),2,1)
        case default
        stop 'Error: dir must = 1,2,3 in solveFFT in FFT_poisson.f90'
        end select

        call applyAllBCs(u_bcs,u,g)

        if (displayTF) then
          call lap(FFT%res,u,g)
          call subtract(FFT%res,f)
          call zeroGhostPoints(FFT%res)
          call compute(norm,0.0_cp,FFT%res%phi)
          call print(norm,FFT%name//' Residuals for '//trim(adjustl(getName(ss))))
        endif

        call delete(FFT)
      end subroutine

      end module