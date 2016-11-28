      module FFT_poisson_mod
      ! call FFT_poisson(u,f,u_bcs,g,gridType,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), grid (g)
      ! and using a Fast Fourier Transform (FFT) method.
      !
      ! Although grids may have different spacing in each direction,
      ! grid sizes along each direction are assumed uniform.
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to boundary_conditions_mod for more info.
      !     g            = contains grid information (dhc,dhn)
      !
      ! Flags: (_PARALLELIZE_FFT_)
      use current_precision_mod
      use mesh_mod
      use apply_BCs_mod
      use norms_mod
      use SF_mod
      use ops_discrete_mod
      use ops_aux_mod
      use ops_dct_mod
      use ops_idct_mod
      implicit none

      private
      public :: FFTSolver,solve
      private :: init,delete

       real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

      type FFTSolver
        real(cp),dimension(3) :: dh2 ! Grid spacing (assumed uniform)
        integer :: Nx,Ny,Nz ! Number of cells
        type(SF) :: f,res
        type(SF) :: coeff_x,coeff_y,coeff_z
        integer,dimension(3) :: s
      end type

      interface init;        module procedure initFFT;       end interface
      interface delete;      module procedure deleteFFT;     end interface
      interface solve;       module procedure solveFFT_SF;   end interface

      contains

      subroutine initFFT(FFT,u,m)
        implicit none
        type(FFTSolver),intent(inout) :: FFT
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        integer :: i,j,k,t
        real(cp) :: cosi,cosj,cosk
        FFT%s = u%BF(1)%GF%s
        FFT%dh2(1) = m%B(1)%g%c(1)%dhn(1)**2.0_cp
        FFT%dh2(2) = m%B(1)%g%c(2)%dhn(1)**2.0_cp
        FFT%dh2(3) = m%B(1)%g%c(3)%dhn(1)**2.0_cp
        FFT%Nx = m%B(1)%g%c(1)%sc-2
        FFT%Ny = m%B(1)%g%c(2)%sc-2
        FFT%Nz = m%B(1)%g%c(3)%sc-2
        call init(FFT%f,u)
        call init(FFT%coeff_x,u)
        call init(FFT%coeff_y,u)
        call init(FFT%coeff_z,u)
        call init(FFT%res,u)

        ! THESE FORMULAS ARE ONLY VALID WHEN DX1 = DX2 WHERE DIR = 3
        !$OMP PARALLEL DO PRIVATE(cosj,cosk)
        do k=1,FFT%s(3); do j=2,FFT%s(2)-1; do i=2,FFT%s(1)-1
        cosj = cos(PI*real(j-2,cp)/real(FFT%Ny,cp))
        cosk = cos(PI*real(k-2,cp)/real(FFT%Nz,cp))
        if (.not.((i.eq.2).and.(j.eq.2))) then
              FFT%coeff_x%BF(t)%GF%f(i,j,k) = 0.5_cp*1.0_cp/(cosj+cosk-2.0_cp)*FFT%dh2(1)
        else; FFT%coeff_x%BF(t)%GF%f(i,j,k) = 1.0_cp
        endif
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO PRIVATE(cosi,cosk)
        do k=2,FFT%s(3)-1; do j=1,FFT%s(2); do i=2,FFT%s(1)-1
        cosi = cos(PI*real(i-2,cp)/real(FFT%Nx,cp))
        cosk = cos(PI*real(k-2,cp)/real(FFT%Nz,cp))
        if (.not.((i.eq.2).and.(k.eq.2))) then
              FFT%coeff_y%BF(t)%GF%f(i,j,k) = 0.5_cp*1.0_cp/(cosi+cosk-2.0_cp)*FFT%dh2(2)
        else; FFT%coeff_y%BF(t)%GF%f(i,j,k) = 1.0_cp
        endif
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO PRIVATE(cosi,cosj)
        do k=1,FFT%s(3); do j=2,FFT%s(2)-1; do i=2,FFT%s(1)-1
        cosi = cos(PI*real(i-2,cp)/real(FFT%Nx,cp))
        cosj = cos(PI*real(j-2,cp)/real(FFT%Ny,cp))
        if (.not.((i.eq.2).and.(j.eq.2))) then
              FFT%coeff_z%BF(t)%GF%f(i,j,k) = 0.5_cp*1.0_cp/(cosi+cosj-2.0_cp)*FFT%dh2(3)
        else; FFT%coeff_z%BF(t)%GF%f(i,j,k) = 1.0_cp
        endif
        enddo; enddo; enddo
        !$OMP END PARALLEL DO

      end subroutine

      subroutine deleteFFT(FFT)
        implicit none
        type(FFTSolver),intent(inout) :: FFT
        call delete(FFT%res)
        call delete(FFT%f)
        call delete(FFT%coeff_x)
        call delete(FFT%coeff_y)
        call delete(FFT%coeff_z)
      end subroutine

      subroutine solveFFT_SF(FFT,u,f,vol,m,norm,displayTF,dir)
        implicit none
        type(FFTSolver),intent(inout) :: FFT
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f,vol
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        integer,intent(in) :: dir
        integer :: i,j,k,t
        integer,dimension(3) :: s

        call assign(FFT%f,f)
        do t=1,u%s
          s = u%BF(t)%GF%s
          call init(FFT,U,m)

          select case (dir)
          case (1); call dct(FFT%f%BF(t)%GF%f(:,2:s(2)-1,:),2,1)
                    call dct(FFT%f%BF(t)%GF%f(:,:,2:s(3)-1),3,1)
          case (2); call dct(FFT%f%BF(t)%GF%f(2:s(1)-1,:,:),1,1)
                    call dct(FFT%f%BF(t)%GF%f(:,:,2:s(3)-1),3,1)
          case (3); call dct(FFT%f%BF(t)%GF%f(2:s(1)-1,:,:),1,1)
                    call dct(FFT%f%BF(t)%GF%f(:,2:s(2)-1,:),2,1)
          case default
          stop 'Error: dir must = 1,2,3 in solveFFT in FFT_poisson.f90'
          end select

          select case (dir)
          case (1)
            !$OMP PARALLEL DO
            do k=1,s(3); do j=2,s(2)-1; do i=2,s(1)-1
              u%BF(t)%GF%f(i,j,k) = FFT%f%BF(t)%GF%f(i,j,k)*FFT%coeff_x%BF(t)%GF%f(i,j,k)
            enddo; enddo; enddo
            !$OMP END PARALLEL DO
          case (2)
            !$OMP PARALLEL DO
            do k=2,s(3)-1; do j=1,s(2); do i=2,s(1)-1
              u%BF(t)%GF%f(i,j,k) = FFT%f%BF(t)%GF%f(i,j,k)*FFT%coeff_x%BF(t)%GF%f(i,j,k)
            enddo; enddo; enddo
            !$OMP END PARALLEL DO
          case (3)
            !$OMP PARALLEL DO
            do k=1,s(3); do j=2,s(2)-1; do i=2,s(1)-1
              u%BF(t)%GF%f(i,j,k) = FFT%f%BF(t)%GF%f(i,j,k)*FFT%coeff_x%BF(t)%GF%f(i,j,k)
            enddo; enddo; enddo
            !$OMP END PARALLEL DO
          case default
          stop 'Error: dir must = 1,2,3 in solveFFT in FFT_poisson.f90'
          end select

          ! 'Pin down pressure'
          select case (dir)
          case (1); u%BF(t)%GF%f(:,2,2) = 0.0_cp
          case (2); u%BF(t)%GF%f(2,:,2) = 0.0_cp
          case (3); u%BF(t)%GF%f(2,2,:) = 0.0_cp
          case default
          stop 'Error: dir must = 1,2,3 in solveFFT in FFT_poisson.f90'
          end select

          select case (dir)
          case (1); call idct(u%BF(t)%GF%f(:,2:s(2)-1,:),2,1)
                    call idct(u%BF(t)%GF%f(:,:,2:s(3)-1),3,1)
          case (2); call idct(u%BF(t)%GF%f(2:s(1)-1,:,:),1,1)
                    call idct(u%BF(t)%GF%f(:,:,2:s(3)-1),3,1)
          case (3); call idct(u%BF(t)%GF%f(2:s(1)-1,:,:),1,1)
                    call idct(u%BF(t)%GF%f(:,2:s(2)-1,:),2,1)
          case default
          stop 'Error: dir must = 1,2,3 in solveFFT in FFT_poisson.f90'
          end select

          call apply_BCs(u,m)

          if (displayTF) then
            call lap(FFT%res,u,m)
            call subtract(FFT%res,f)
            call zeroGhostPoints(FFT%res)
            call compute(norm,FFT%res,vol)
            call print(norm,'FFT Residuals')
          endif
          call delete(FFT)
        enddo
      end subroutine

      end module