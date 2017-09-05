      module GF_product_add_mod
        use grid_field_mod
        use grid_field_extend_mod
        use current_precision_mod
        implicit none
        private
        public :: product_add

        interface product_add;              module procedure product_add_GF_GF_S;    end interface
        interface product_add;              module procedure product_add_GF_GF_GF;   end interface

      contains

        subroutine product_add_GF_GF_S(a,c,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: c
          type(grid_field),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'product_add_GF_GF_S (1)')
#endif
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = a%f(i,j,k)*c + b%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'product_add_GF_GF_S (2)')
#endif
          a%f = a%f*c + b%f
#endif
        end subroutine

        subroutine product_add_GF_GF_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c
          integer :: i,j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'product_add_GF_GF_GF')
#endif
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = a%f(i,j,k)*c%f(i,j,k) + b%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1) ! No intrinsic matrix-matrix mult.
          a%f(i,j,k) = a%f(i,j,k)*c%f(i,j,k) + b%f(i,j,k)
          enddo; enddo; enddo
#endif
        end subroutine

      end module