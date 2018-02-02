      module GF_magnitude_mod
        use grid_field_mod
        use grid_field_extend_mod
        use current_precision_mod
        implicit none
        private
        public :: magnitude

        interface magnitude;     module procedure magnitude_GF_GF_GF_GF;  end interface

      contains

        subroutine magnitude_GF_GF_GF_GF(a,b,c,d)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c,d
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'magnitude_GF_GF_GF_GF (1)')
          call insist_shape_match(a,c,'magnitude_GF_GF_GF_GF (2)')
          call insist_shape_match(a,d,'magnitude_GF_GF_GF_GF (3)')
#endif
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = sqrt(b%f(i,j,k)**2.0_cp + c%f(i,j,k)**2.0_cp + d%f(i,j,k)**2.0_cp)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'magnitude_GF_GF_GF_GF (4)')
          call insist_shape_match(a,c,'magnitude_GF_GF_GF_GF (5)')
          call insist_shape_match(a,d,'magnitude_GF_GF_GF_GF (6)')
#endif
          a%f = sqrt(b%f**2.0_cp + c%f**2.0_cp + d%f**2.0_cp)
#endif
        end subroutine

      end module