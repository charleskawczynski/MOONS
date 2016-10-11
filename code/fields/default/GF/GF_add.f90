      module GF_add_mod
        use GF_base_mod
        use current_precision_mod
        implicit none
        private
        public :: add,add_product

        interface add;                      module procedure add_GF_GF;              end interface
        interface add;                      module procedure add_GF_GF_GF;           end interface
        interface add;                      module procedure add_GF_GF_GF_GF;        end interface
        interface add;                      module procedure add_GF_R;               end interface
        interface add;                      module procedure add_GF_S;               end interface
        interface add;                      module procedure add_S_GF;               end interface
        interface add;                      module procedure add_GF_GF9;             end interface

        interface add_product;              module procedure add_product_GF_GF_S;    end interface
        interface add_product;              module procedure add_product_GF_GF_GF;   end interface

      contains

      ! ------------------- ADD ------------------------

        subroutine add_GF_GF(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = a%f(i,j,k) + b%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f + b%f
#endif
        end subroutine

        subroutine add_GF_GF_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = b%f(i,j,k) + c%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = b%f + c%f
#endif
        end subroutine

        subroutine add_GF_GF_GF_GF(a,b,c,d)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c,d
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = b%f(i,j,k) + c%f(i,j,k) + d%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = b%f + c%f + d%f
#endif
        end subroutine

        subroutine add_GF_R(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),dimension(:,:,:),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = a%f(i,j,k) + b(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f + b
#endif
        end subroutine

        subroutine add_GF_S(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = a%f(i,j,k) + b
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f + b
#endif
        end subroutine

        subroutine add_S_GF(g2,a)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = a%f(i,j,k) + g2
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f + g2
#endif
        end subroutine

        subroutine add_GF_GF9(A,B1,B2,B3,B4,B5,B6,B7,B8,B9)
          implicit none
          type(grid_field),intent(inout) :: A
          type(grid_field),intent(in) :: B1,B2,B3,B4,B5,B6,B7,B8,B9
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,A%s(3); do j=1,A%s(2); do i=1,A%s(1)
          A%f(i,j,k) = B1%f(i,j,k)+B2%f(i,j,k)+B3%f(i,j,k)+&
                       B4%f(i,j,k)+B5%f(i,j,k)+B6%f(i,j,k)+&
                       B7%f(i,j,k)+B8%f(i,j,k)+B9%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          A%f = B1%f+B2%f+B3%f+&
                B4%f+B5%f+B6%f+&
                B7%f+B8%f+B9%f
#endif
        end subroutine

      ! ------------------- ADD PRODUCT ------------------------

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