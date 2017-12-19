      module GF_multiply_mod
        use grid_field_mod
        use grid_field_extend_mod
        use current_precision_mod
        implicit none
        private
        public :: multiply

        interface multiply;                 module procedure multiply_GF_GF;         end interface
        interface multiply;                 module procedure multiply_GF_GF_GF;      end interface
        interface multiply;                 module procedure multiply_GF_GF_S;       end interface
        interface multiply;                 module procedure multiply_GF_S;          end interface
        interface multiply;                 module procedure multiply_S_GF;          end interface

      contains

        subroutine multiply_GF_GF(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'multiply_GF_GF (1)')
#endif
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) * b%f(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'multiply_GF_GF (2)')
#endif
          a%f = a%f * b%f
#endif
        end subroutine

        subroutine multiply_GF_GF_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'multiply_GF_GF_GF (1)')
          call insist_shape_match(a,c,'multiply_GF_GF_GF (2)')
#endif
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b%f(i,j,k) * c%f(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'multiply_GF_GF_GF (3)')
          call insist_shape_match(a,c,'multiply_GF_GF_GF (4)')
#endif
          a%f = b%f * c%f
#endif
        end subroutine

        subroutine multiply_GF_GF_S(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          real(cp),intent(in) :: c
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'multiply_GF_GF_S (1)')
#endif
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b%f(i,j,k) * c
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'multiply_GF_GF_S (2)')
#endif
          a%f = b%f * c
#endif
        end subroutine

        subroutine multiply_GF_S(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) * b
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f * b
#endif
        end subroutine

        subroutine multiply_S_GF(g2,a)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) * g2
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f * g2
#endif
        end subroutine

      end module