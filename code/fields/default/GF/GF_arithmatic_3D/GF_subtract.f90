      module GF_subtract_mod
        use GF_base_mod
        use current_precision_mod
        implicit none
        private
        public :: subtract

        interface subtract;                 module procedure subtract_GF_GF;         end interface
        interface subtract;                 module procedure subtract_GF_GF_GF;      end interface
        interface subtract;                 module procedure subtract_GF_R_R;        end interface
        interface subtract;                 module procedure subtract_GF_R;          end interface
        interface subtract;                 module procedure subtract_GF_S;          end interface
        interface subtract;                 module procedure subtract_S_GF;          end interface

      contains

        subroutine subtract_GF_GF(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'subtract_GF_GF (1)')
#endif
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) - b%f(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'subtract_GF_GF (2)')
#endif
          a%f = a%f - b%f
#endif
        end subroutine

        subroutine subtract_GF_GF_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'subtract_GF_GF_GF (1)')
#endif
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b%f(i,j,k) - c%f(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,'subtract_GF_GF_GF (2)')
#endif
          a%f = b%f - c%f
#endif
        end subroutine

        subroutine subtract_GF_R_R(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),dimension(:,:,:),intent(in) :: b,c
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b(i,j,k) - c(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          a%f = b - c
#endif
        end subroutine

        subroutine subtract_GF_S(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) - b
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f - b
#endif
        end subroutine

        subroutine subtract_GF_R(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),dimension(:,:,:),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) - b(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f - b
#endif
        end subroutine

        subroutine subtract_S_GF(g2,a)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = g2 - a%f(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          a%f = g2 - a%f
#endif
        end subroutine

      end module