      module GF_add_product_mod
        use GF_base_mod
        use current_precision_mod
        implicit none
        private
        public :: add_product
        interface add_product;  module procedure add_product_GF_GF_S;    end interface
        interface add_product;  module procedure add_product_GF_GF_GF;   end interface

        contains

        subroutine add_product_GF_GF_S(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          real(cp),intent(in) :: c
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = a%f(i,j,k) + b%f(i,j,k)*c
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f + b%f*c
#endif
        end subroutine

        subroutine add_product_GF_GF_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = a%f(i,j,k) + b%f(i,j,k)*c%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1) ! No intrinsic matrix-matrix mult.
          a%f(i,j,k) = a%f(i,j,k) + b%f(i,j,k)*c%f(i,j,k)
          enddo; enddo; enddo
#endif
        end subroutine

      end module