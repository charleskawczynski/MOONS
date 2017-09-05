      module GF_square_mod
        use grid_field_mod
        use current_precision_mod
        implicit none
        private

        public :: square
        interface square;  module procedure square_GF;  end interface

        contains

        subroutine square_GF(a)
          implicit none
          type(grid_field),intent(inout) :: a
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = a%f(i,j,k) * a%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f*a%f
#endif
        end subroutine

      end module