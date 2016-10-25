      module GF_divide_mod
        use GF_base_mod
        use current_precision_mod
        implicit none
        private

        public :: divide
        interface divide;                   module procedure divide_GF_GF;           end interface
        interface divide;                   module procedure divide_GF_GF_GF;        end interface
        interface divide;                   module procedure divide_GF_S_GF;         end interface
        interface divide;                   module procedure divide_GF_S;            end interface
        interface divide;                   module procedure divide_S_GF;            end interface

      contains

        subroutine divide_GF_GF(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) / b%f(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f / b%f
#endif
        end subroutine

        subroutine divide_GF_GF_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b%f(i,j,k) / c%f(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          a%f = b%f / c%f
#endif
        end subroutine

        subroutine divide_GF_S_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: c
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b / c%f(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          a%f = b / c%f
#endif
        end subroutine

        subroutine divide_GF_S(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) / b
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f / b
#endif
        end subroutine

        subroutine divide_S_GF(g2,a)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = g2 / a%f(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          a%f = g2 / a%f
#endif
        end subroutine

      end module