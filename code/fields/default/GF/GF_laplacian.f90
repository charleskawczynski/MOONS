      module GF_laplacian_mod
        use GF_base_mod
        use current_precision_mod
        implicit none
        private
        public :: laplacian
        interface laplacian;                module procedure laplacian_GF;           end interface

        contains

        subroutine laplacian_GF(lapU,U,stencil)
          implicit none
          type(grid_field),intent(inout) :: lapU
          type(grid_field),intent(in) :: U
          type(stencil_3D) :: stencil
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,lapU%s(3)-1; do j=2,lapU%s(2)-1; do i=2,lapU%s(1)-1
          lapU%f(i,j,k) = &
          U%f(i+1,j,k)*stencil%x_L(i)
          U%f(i-1,j,k)*stencil%x_D(i)
          U%f(i,j+1,k)*stencil%y_L(j)
          U%f(i,j-1,k)*stencil%y_D(j)
          U%f(i,j,k+1)*stencil%z_L(k)
          U%f(i,j,k-1)*stencil%z_D(k)
          U%f(i,j,k)*stencil%D(i)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

      end module