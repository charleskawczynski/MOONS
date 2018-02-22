      module FFT_algorithm_mod
      ! Flags: (_PARALLELIZE_FFT_)
      use current_precision_mod
      use mesh_extend_mod
      use apply_BCs_mod
      use norms_extend_mod
      use SF_extend_mod
      use ops_discrete_mod
      use ops_aux_mod
      use ops_dct_mod
      use ops_idct_mod
      use constants_mod
      implicit none

      private
      public :: solve_FFT_SF

      ! interface solve;  module procedure solve_FFT_SF;   end interface

      contains

      subroutine solve_FFT_SF(u,f_in,vol,coeff,m,norm,compute_norms,f,res,dir,name)
        implicit none
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f_in,vol,coeff
        type(SF),intent(inout) :: f,res
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        logical,intent(in) :: compute_norms
        character(len=*),intent(in) :: name
        integer,intent(in) :: dir
        select case (dir)
        case (1); call solve_FFT_SF_x(u,f_in,vol,coeff,m,norm,compute_norms,f,res,name)
        case (2); call solve_FFT_SF_y(u,f_in,vol,coeff,m,norm,compute_norms,f,res,name)
        case (3); call solve_FFT_SF_z(u,f_in,vol,coeff,m,norm,compute_norms,f,res,name)
        case default
        stop 'Error: dir must = 1,2,3 in solve_FFT_SF in FFT_algorithm.f90'
        end select
      end subroutine

      subroutine solve_FFT_SF_x(u,f_in,vol,coeff,m,norm,compute_norms,f,res,name)
        implicit none
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f_in,vol,coeff
        type(SF),intent(inout) :: f,res
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        logical,intent(in) :: compute_norms
        character(len=*),intent(in) :: name
        integer :: i,j,k,t
        integer,dimension(3) :: s

        call assign(f,f_in)

        do t=1,u%s
          s = u%BF(t)%GF%s
          call dct(f%BF(t)%GF%f(:,2:s(2)-1,:),2,1)
          call dct(f%BF(t)%GF%f(:,:,2:s(3)-1),3,1)
        enddo

        do t=1,u%s
          !$OMP PARALLEL DO
          do k=1,s(3); do j=2,s(2)-1; do i=2,s(1)-1
            u%BF(t)%GF%f(i,j,k) = f%BF(t)%GF%f(i,j,k)*coeff%BF(t)%GF%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
        enddo

        ! 'Pin down pressure'
        do t=1,u%s
          u%BF(t)%GF%f(:,2,2) = 0.0_cp
        enddo

        do t=1,u%s
          call idct(u%BF(t)%GF%f(:,2:s(2)-1,:),2,1)
          call idct(u%BF(t)%GF%f(:,:,2:s(3)-1),3,1)
        enddo

        call apply_BCs(u)

        if (.true.) then
          call lap(res,u,m)
          call subtract(res,f)
          call assign_ghost_XPeriodic(res,0.0_cp)
          call compute(norm,res,vol,m%MP%volume)
          call print(norm,'FFT_x_'//name)
        endif
      end subroutine

      subroutine solve_FFT_SF_y(u,f_in,vol,coeff,m,norm,compute_norms,f,res,name)
        implicit none
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f_in,vol,coeff
        type(SF),intent(inout) :: f,res
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        logical,intent(in) :: compute_norms
        character(len=*),intent(in) :: name
        integer :: i,j,k,t
        integer,dimension(3) :: s

        call assign(f,f_in)

        do t=1,u%s
          s = u%BF(t)%GF%s
          call dct(f%BF(t)%GF%f(2:s(1)-1,:,:),1,1)
          call dct(f%BF(t)%GF%f(:,:,2:s(3)-1),3,1)
        enddo

        do t=1,u%s
          !$OMP PARALLEL DO
          do k=2,s(3)-1; do j=1,s(2); do i=2,s(1)-1
            u%BF(t)%GF%f(i,j,k) = f%BF(t)%GF%f(i,j,k)*coeff%BF(t)%GF%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
        enddo

        ! 'Pin down pressure'
        do t=1,u%s
          u%BF(t)%GF%f(2,:,2) = 0.0_cp
        enddo

        do t=1,u%s
          call idct(u%BF(t)%GF%f(2:s(1)-1,:,:),1,1)
          call idct(u%BF(t)%GF%f(:,:,2:s(3)-1),3,1)
        enddo

        call apply_BCs(u)

        if (compute_norms) then
          call lap(res,u,m)
          call subtract(res,f)
          call assign_ghost_XPeriodic(res,0.0_cp)
          call compute(norm,res,vol,m%MP%volume)
          call print(norm,'FFT_y_'//name)
        endif
      end subroutine

      subroutine solve_FFT_SF_z(u,f_in,vol,coeff,m,norm,compute_norms,f,res,name)
        implicit none
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f_in,vol,coeff
        type(SF),intent(inout) :: f,res
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        logical,intent(in) :: compute_norms
        character(len=*),intent(in) :: name
        integer :: i,j,k,t
        integer,dimension(3) :: s

        call assign(f,f_in)

        do t=1,u%s
          s = u%BF(t)%GF%s
          call dct(f%BF(t)%GF%f(2:s(1)-1,:,:),1,1)
          call dct(f%BF(t)%GF%f(:,2:s(2)-1,:),2,1)
        enddo

        do t=1,u%s
          !$OMP PARALLEL DO
          do k=1,s(3); do j=2,s(2)-1; do i=2,s(1)-1
            u%BF(t)%GF%f(i,j,k) = f%BF(t)%GF%f(i,j,k)*coeff%BF(t)%GF%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
        enddo

        ! 'Pin down pressure'
        do t=1,u%s
          u%BF(t)%GF%f(2,2,:) = 0.0_cp
        enddo

        do t=1,u%s
          call idct(u%BF(t)%GF%f(2:s(1)-1,:,:),1,1)
          call idct(u%BF(t)%GF%f(:,2:s(2)-1,:),2,1)
        enddo

        call apply_BCs(u)

        if (compute_norms) then
          call lap(res,u,m)
          call subtract(res,f)
          call assign_ghost_XPeriodic(res,0.0_cp)
          call compute(norm,res,vol,m%MP%volume)
          call print(norm,'FFT_z_'//name)
        endif
      end subroutine

      end module