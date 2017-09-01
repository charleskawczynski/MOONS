      module GF_assign_mod
        use grid_field_mod
        use grid_field_extend_mod
        use current_precision_mod
        implicit none
        private
        public :: assign,assign_negative

        interface assign;                   module procedure assign_GF_S;            end interface
        interface assign;                   module procedure assign_GF_GF;           end interface
        interface assign;                   module procedure assign_GF_R;            end interface
        interface assign_negative;          module procedure assign_negative_GF_GF;  end interface

        contains

        subroutine assign_GF_GF(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'assign_GF_GF (1)')
#endif
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = b%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'assign_GF_GF (2)')
#endif
          a%f = b%f
#endif
        end subroutine

        subroutine assign_GF_R(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),dimension(:,:,:),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = b(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = b
#endif
        end subroutine

        subroutine assign_GF_S(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = b
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = b
#endif
        end subroutine

        subroutine assign_negative_GF_GF(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'assign_negative_GF_GF (1)')
#endif
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = -b%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'assign_negative_GF_GF (2)')
#endif
          a%f = -b%f
#endif
        end subroutine

      end module