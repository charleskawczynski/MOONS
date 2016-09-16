      module RF_ops_mod
        use RF_base_mod
        use current_precision_mod
        use grid_mod
        use BCs_mod
        implicit none
        private

        ! Operators
        public :: assign,assign_negative
        public :: add,subtract
        public :: multiply,divide
        public :: add_product,swap

        interface assign;                   module procedure assign_RF_S;            end interface
        interface assign;                   module procedure assign_RF_RF;           end interface
        interface assign;                   module procedure assign_RF_R;            end interface
        interface assign_negative;          module procedure assign_negative_RF_RF;  end interface

        interface add;                      module procedure add_RF_RF;              end interface
        interface add;                      module procedure add_RF_RF_RF;           end interface
        interface add;                      module procedure add_RF_RF_RF_RF;        end interface
        interface add;                      module procedure add_RF_R;               end interface
        interface add;                      module procedure add_RF_S;               end interface
        interface add;                      module procedure add_S_RF;               end interface
        interface add;                      module procedure add_RF_RF9;             end interface

        interface add_product;              module procedure add_product_RF_RF_S;    end interface
        interface add_product;              module procedure add_product_RF_RF_RF;   end interface

        interface multiply;                 module procedure multiply_RF_RF;         end interface
        interface multiply;                 module procedure multiply_RF_RF_RF;      end interface
        interface multiply;                 module procedure multiply_RF_RF_S;       end interface
        interface multiply;                 module procedure multiply_RF_S;          end interface
        interface multiply;                 module procedure multiply_S_RF;          end interface

        interface subtract;                 module procedure subtract_RF_RF;         end interface
        interface subtract;                 module procedure subtract_RF_RF_RF;      end interface
        interface subtract;                 module procedure subtract_RF_R_R;        end interface
        interface subtract;                 module procedure subtract_RF_R;          end interface
        interface subtract;                 module procedure subtract_RF_S;          end interface
        interface subtract;                 module procedure subtract_S_RF;          end interface

        interface divide;                   module procedure divide_RF_RF;           end interface
        interface divide;                   module procedure divide_RF_RF_RF;        end interface
        interface divide;                   module procedure divide_RF_S_RF;         end interface
        interface divide;                   module procedure divide_RF_S;            end interface
        interface divide;                   module procedure divide_S_RF;            end interface

        interface swap;                     module procedure swap_RF;                end interface

      contains

        subroutine assign_RF_RF(a,b)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = b%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = b%f
#endif
        end subroutine

        subroutine assign_RF_R(a,b)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),dimension(:,:,:),intent(in) :: b
#ifdef _PARALLELIZE_RF_
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

        subroutine assign_RF_S(a,b)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_RF_
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

        subroutine assign_negative_RF_RF(a,b)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = -b%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = -b%f
#endif
        end subroutine

      ! ------------------- ADD ------------------------

        subroutine add_RF_RF(a,b)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b
#ifdef _PARALLELIZE_RF_
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

        subroutine add_RF_RF_RF(a,b,c)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b,c
#ifdef _PARALLELIZE_RF_
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

        subroutine add_RF_RF_RF_RF(a,b,c,d)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b,c,d
#ifdef _PARALLELIZE_RF_
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

        subroutine add_RF_R(a,b)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),dimension(:,:,:),intent(in) :: b
#ifdef _PARALLELIZE_RF_
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

        subroutine add_RF_S(a,b)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_RF_
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

        subroutine add_S_RF(g2,a)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_RF_
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

        subroutine add_RF_RF9(A,B1,B2,B3,B4,B5,B6,B7,B8,B9)
          implicit none
          type(realField),intent(inout) :: A
          type(realField),intent(in) :: B1,B2,B3,B4,B5,B6,B7,B8,B9
#ifdef _PARALLELIZE_RF_
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

        subroutine add_product_RF_RF_S(a,b,c)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b
          real(cp),intent(in) :: c
#ifdef _PARALLELIZE_RF_
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

        subroutine add_product_RF_RF_RF(a,b,c)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b,c
          integer :: i,j,k
#ifdef _PARALLELIZE_RF_
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

      ! ------------------- SUBTRACT ------------------------

        subroutine subtract_RF_RF(a,b)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = a%f(i,j,k) - b%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f - b%f
#endif
        end subroutine

        subroutine subtract_RF_RF_RF(a,b,c)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b,c
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = b%f(i,j,k) - c%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = b%f - c%f
#endif
        end subroutine

        subroutine subtract_RF_R_R(a,b,c)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),dimension(:,:,:),intent(in) :: b,c
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = b(i,j,k) - c(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = b - c
#endif
        end subroutine

        subroutine subtract_RF_S(a,b)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = a%f(i,j,k) - b
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f - b
#endif
        end subroutine

        subroutine subtract_RF_R(a,b)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),dimension(:,:,:),intent(in) :: b
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = a%f(i,j,k) - b(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f - b
#endif
        end subroutine

        subroutine subtract_S_RF(g2,a)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = g2 - a%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = g2 - a%f
#endif
        end subroutine

      ! ------------------- MULTIPLY ------------------------

        subroutine multiply_RF_RF(a,b)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = a%f(i,j,k) * b%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f * b%f
#endif
        end subroutine

        subroutine multiply_RF_RF_RF(a,b,c)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b,c
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = b%f(i,j,k) * c%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = b%f * c%f
#endif
        end subroutine

        subroutine multiply_RF_RF_S(a,b,c)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b
          real(cp),intent(in) :: c
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = b%f(i,j,k) * c
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = b%f * c
#endif
        end subroutine

        subroutine multiply_RF_S(a,b)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = a%f(i,j,k) * b
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f * b
#endif
        end subroutine

        subroutine multiply_S_RF(g2,a)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = a%f(i,j,k) * g2
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f * g2
#endif
        end subroutine

      ! ------------------- DIVIDE ------------------------

        subroutine divide_RF_RF(a,b)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = a%f(i,j,k) / b%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f / b%f
#endif
        end subroutine

        subroutine divide_RF_RF_RF(a,b,c)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b,c
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = b%f(i,j,k) / c%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = b%f / c%f
#endif
        end subroutine

        subroutine divide_RF_S_RF(a,b,c)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: c
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = b / c%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = b / c%f
#endif
        end subroutine

        subroutine divide_RF_S(a,b)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = a%f(i,j,k) / b
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = a%f / b
#endif
        end subroutine

        subroutine divide_S_RF(g2,a)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            a%f(i,j,k) = g2 / a%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = g2 / a%f
#endif
        end subroutine

      ! ------------------- OTHER ------------------------

        subroutine square_RF(a)
          implicit none
          type(realField),intent(inout) :: a
#ifdef _PARALLELIZE_RF_
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

        subroutine swap_RF(a,b,c)
          implicit none
          type(realField),intent(inout) :: a,b,c
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            c%f(i,j,k) = a%f(i,j,k)
            a%f(i,j,k) = b%f(i,j,k)
            b%f(i,j,k) = c%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          c%f = a%f
          a%f = b%f
          b%f = c%f
#endif
        end subroutine

      end module