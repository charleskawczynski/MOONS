      module FFT_solver_extend_mod
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
      use mesh_extend_mod
      use FFT_Solver_SF_mod
      use FFT_algorithm_mod
      use apply_BCs_mod
      use norms_extend_mod
      use string_mod
      use SF_extend_mod
      use ops_discrete_mod
      use ops_aux_mod
      use ops_dct_mod
      use ops_idct_mod
      use constants_mod
      implicit none

      private
      public :: FFT_Solver_SF
      public :: init
      public :: solve

      interface init;  module procedure init_FFT;             end interface
      interface solve; module procedure solve_FFT_SF_wrapper; end interface

      contains

      subroutine init_FFT(FFT,u,m,direction,var_name)
        implicit none
        type(FFT_Solver_SF),intent(inout) :: FFT
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        integer,intent(in) :: direction
        character(len=*),intent(in) :: var_name
        integer :: i,j,k,t
        real(cp) :: cosi,cosj,cosk
        real(cp) :: dx,dy,dz
        real(cp) :: Nx,Ny,Nz

        call init(FFT%f,u)
        call init(FFT%var_name,var_name)
        call init(FFT%coeff,u)
        call init(FFT%res,u)
        call init(FFT%vol,u)
        call volume(FFT%vol,m)
        FFT%direction = direction

        ! THESE FORMULAS ARE ONLY VALID WHEN DX1 = DX2 WHERE DIR = 3
        select case(direction)
        case (1)
          do t=1,m%s;
          dy = m%B(t)%g%c(2)%dhn%f(1)
          dz = m%B(t)%g%c(3)%dhn%f(1)
          Ny = real(m%B(t)%g%c(2)%sc-2,cp)
          Nz = real(m%B(t)%g%c(3)%sc-2,cp)
          do k=1,u%BF(t)%GF%s(3); do j=2,u%BF(t)%GF%s(2)-1; do i=2,u%BF(t)%GF%s(1)-1
          cosj = cos(PI*real(j-2,cp)/Ny)
          cosk = cos(PI*real(k-2,cp)/Nz)
          if (.not.((i.eq.2).and.(j.eq.2))) then
                FFT%coeff%BF(t)%GF%f(i,j,k) = 0.5_cp*dy*dz/(cosj+cosk-2.0_cp)
          else; FFT%coeff%BF(t)%GF%f(i,j,k) = 1.0_cp
          endif
          enddo; enddo; enddo
          enddo
        case (2)
          do t=1,m%s;
          dx = m%B(t)%g%c(1)%dhn%f(1)
          dz = m%B(t)%g%c(3)%dhn%f(1)
          Nx = real(m%B(t)%g%c(1)%sc-2,cp)
          Nz = real(m%B(t)%g%c(3)%sc-2,cp)
          do k=2,u%BF(t)%GF%s(3)-1; do j=1,u%BF(t)%GF%s(2); do i=2,u%BF(t)%GF%s(1)-1
          cosi = cos(PI*real(i-2,cp)/Nx)
          cosk = cos(PI*real(k-2,cp)/Nz)
          if (.not.((i.eq.2).and.(k.eq.2))) then
                FFT%coeff%BF(t)%GF%f(i,j,k) = 0.5_cp*dx*dz/(cosi+cosk-2.0_cp)
          else; FFT%coeff%BF(t)%GF%f(i,j,k) = 1.0_cp
          endif
          enddo; enddo; enddo
          enddo
        case(3)
          do t=1,m%s;
          dx = m%B(t)%g%c(1)%dhn%f(1)
          dy = m%B(t)%g%c(2)%dhn%f(1)
          Nx = real(m%B(t)%g%c(1)%sc-2,cp)
          Ny = real(m%B(t)%g%c(2)%sc-2,cp)
          do k=1,u%BF(t)%GF%s(3); do j=2,u%BF(t)%GF%s(2)-1; do i=2,u%BF(t)%GF%s(1)-1
          cosi = cos(PI*real(i-2,cp)/Nx)
          cosj = cos(PI*real(j-2,cp)/Ny)
          if (.not.((i.eq.2).and.(j.eq.2))) then
                FFT%coeff%BF(t)%GF%f(i,j,k) = 0.5_cp*dx*dy/(cosi+cosj-2.0_cp)
          else; FFT%coeff%BF(t)%GF%f(i,j,k) = 1.0_cp
          endif
          enddo; enddo; enddo
          enddo
        end select
      end subroutine

      subroutine solve_FFT_SF_wrapper(FFT,u,f_in,m,compute_norms)
        implicit none
        type(FFT_Solver_SF),intent(inout) :: FFT
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f_in
        type(mesh),intent(in) :: m
        logical,intent(in) :: compute_norms
        call solve_FFT_SF(u,f_in,FFT%vol,FFT%coeff,m,FFT%norm,&
        compute_norms,FFT%f,FFT%res,FFT%direction,str(FFT%var_name))
      end subroutine

      end module