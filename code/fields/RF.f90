      module RF_mod
        ! Pre-processor directives: (_DEBUG_RF_,_PARALLELIZE_RF_)
        ! 
        ! Naming convention: name = operation_type1_type2
        ! 
        !      RF = type(realField)
        !      R  = real(cp),dimension(:,:,:)
        !      S  = real(cp)
        ! 
        ! Example(1): Adding a scalar to RF
        !             name = add_RF_S
        ! Example(2): Subtracting a real field from RF
        !             name = subtract_RF_R
        ! Example(3): Subtracting a RF from a real field
        !             name = subtract_R_RF
        ! 
        ! NOTES: RF stands for 'real field'
        ! 
        ! Rules:
        ! a = a + b => call add(a,b)
        ! a = a - b => call subtract(a,b)
        ! a = a * b => call multiply(a,b)
        ! a = a / b => call divide(a,b)
        ! a = b / a => call divide(b,a)
        ! OR
        ! c = a + b => call add(c,a,b)
        ! c = a - b => call subtract(c,a,b)
        ! c = a * b => call multiply(c,a,b)
        ! c = a / b => call divide(c,a,b)
        ! c = b / a => call divide(c,b,a)

        !         
        use current_precision_mod
        use grid_mod
        use BCs_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: realField
        public :: init,delete
        ! Grid initialization
        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        ! BC initialization
        public :: init_BCs

        ! Monitoring
        public :: print,print_physical

        ! Operators
        public :: assign,assign_negative
        public :: add,subtract
        public :: multiply,divide
        public :: add_product,swap
        ! Auxiliary
        public :: square,min,max,maxabs
        public :: maxabsdiff,mean,sum
        public :: size



        type realField
          integer,dimension(3) :: s                  ! Dimension
          real(cp),dimension(:,:,:),allocatable :: f ! field
          type(BCs) :: b
        end type

        interface init;                     module procedure init_RF_size;           end interface
        interface init;                     module procedure init_RF_copy;           end interface

        interface init_CC;                  module procedure init_RF_CC;             end interface
        interface init_Face;                module procedure init_RF_Face;           end interface
        interface init_Edge;                module procedure init_RF_Edge;           end interface
        interface init_Node;                module procedure init_RF_Node;           end interface

        interface init_BCs;                 module procedure init_BC_val;            end interface
        interface init_BCs;                 module procedure init_BC_vals;           end interface

        interface delete;                   module procedure delete_RF;              end interface

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

        interface square;                   module procedure square_RF;              end interface
        interface swap;                     module procedure swap_RF;                end interface
        interface print;                    module procedure print_RF;               end interface
        interface print_physical;           module procedure print_physical_RF;      end interface
        interface min;                      module procedure min_RF;                 end interface
        interface max;                      module procedure max_RF;                 end interface
        interface min;                      module procedure min_pad_RF;             end interface
        interface max;                      module procedure max_pad_RF;             end interface
        interface maxabs;                   module procedure maxabs_RF;              end interface
        interface maxabsdiff;               module procedure maxabsdiff_RF;          end interface
        interface mean;                     module procedure mean_RF;                end interface
        interface sum;                      module procedure sum_RF;                 end interface
        interface sum;                      module procedure sum_RF_pad;             end interface
        interface size;                     module procedure size_RF;                end interface

      contains

        subroutine assign_RF_RF(a,b)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b%f(i,j,k)
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b(i,j,k)
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = -b%f(i,j,k)
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) + b%f(i,j,k)
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b%f(i,j,k) + c%f(i,j,k)
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b%f(i,j,k) + c%f(i,j,k) + d%f(i,j,k)
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) + b(i,j,k)
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) + b
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) + g2
              enddo
            enddo
          enddo
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
          do k=1,A%s(3)
            do j=1,A%s(2)
              do i=1,A%s(1)
                A%f(i,j,k) = B1%f(i,j,k)+B2%f(i,j,k)+B3%f(i,j,k)+&
                             B4%f(i,j,k)+B5%f(i,j,k)+B6%f(i,j,k)+&
                             B7%f(i,j,k)+B8%f(i,j,k)+B9%f(i,j,k)
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) + b%f(i,j,k)*c
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) + b%f(i,j,k)*c%f(i,j,k)
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) - b%f(i,j,k)
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b%f(i,j,k) - c%f(i,j,k)
              enddo
            enddo
          enddo
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

        subroutine subtract_RF_S(a,b)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_RF_
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

        subroutine subtract_RF_R(a,b)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),dimension(:,:,:),intent(in) :: b
#ifdef _PARALLELIZE_RF_
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

        subroutine subtract_S_RF(g2,a)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_RF_
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

      ! ------------------- MULTIPLY ------------------------

        subroutine multiply_RF_RF(a,b)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b%f(i,j,k) * c%f(i,j,k)
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = b%f(i,j,k) * c
              enddo
            enddo
          enddo
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

        subroutine multiply_S_RF(g2,a)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_RF_
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

      ! ------------------- DIVIDE ------------------------

        subroutine divide_RF_RF(a,b)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b
#ifdef _PARALLELIZE_RF_
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

        subroutine divide_RF_RF_RF(a,b,c)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: b,c
#ifdef _PARALLELIZE_RF_
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

        subroutine divide_RF_S_RF(a,b,c)
          implicit none
          type(realField),intent(inout) :: a
          type(realField),intent(in) :: c
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_RF_
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

        subroutine divide_RF_S(a,b)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_RF_
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

        subroutine divide_S_RF(g2,a)
          implicit none
          type(realField),intent(inout) :: a
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_RF_
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

      ! ------------------- OTHER ------------------------

        subroutine square_RF(a)
          implicit none
          type(realField),intent(inout) :: a
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                a%f(i,j,k) = a%f(i,j,k) * a%f(i,j,k)
              enddo
            enddo
          enddo
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
          do k=1,a%s(3)
            do j=1,a%s(2)
              do i=1,a%s(1)
                c%f(i,j,k) = a%f(i,j,k)
                a%f(i,j,k) = b%f(i,j,k)
                b%f(i,j,k) = c%f(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          c%f = a%f
          a%f = b%f
          b%f = c%f
#endif
        end subroutine

        function min_RF(a) result(m)
          implicit none
          type(realField),intent(in) :: a
          real(cp) :: m
          m = minval(a%f)
        end function

        function max_RF(a) result(m)
          implicit none
          type(realField),intent(in) :: a
          real(cp) :: m
          m = maxval(a%f)
        end function

        function min_pad_RF(a,pad) result(m)
          implicit none
          type(realField),intent(in) :: a
          integer,intent(in) :: pad
          real(cp) :: m
          m = minval(a%f(1+pad:a%s(1)-pad,1+pad:a%s(2)-pad,1+pad:a%s(3)-pad))
        end function

        function max_pad_RF(a,pad) result(m)
          implicit none
          type(realField),intent(in) :: a
          integer,intent(in) :: pad
          real(cp) :: m
          m = maxval(a%f(1+pad:a%s(1)-pad,1+pad:a%s(2)-pad,1+pad:a%s(3)-pad))
        end function

        function maxabs_RF(a) result(m)
          implicit none
          type(realField),intent(in) :: a
          real(cp) :: m
          m = maxval(abs(a%f))
        end function

        function maxabsdiff_RF(a,b) result(m)
          implicit none
          type(realField),intent(in) :: a,b
          real(cp) :: m
          m = maxval(abs(a%f-b%f))
        end function

        function mean_RF(a) result(m)
          implicit none
          type(realField),intent(in) :: a
          real(cp) :: m
          m = sum(a)/(max(1,size(a%f)))
        end function

        function sum_RF(a) result(m)
          implicit none
          type(realField),intent(in) :: a
          real(cp) :: m
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          real(cp) :: mTemp
          mTemp = 0.0_cp
          !$OMP PARALLEL DO REDUCTION(+:mTemp)
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            mTemp = mTemp + a%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
          m = mTemp
#else
          m = sum(a%f)
#endif
        end function

        function sum_RF_pad(a,pad) result(m)
          implicit none
          type(realField),intent(in) :: a
          integer,intent(in) :: pad
          real(cp) :: m
#ifdef _PARALLELIZE_RF_
          integer :: i,j,k
          real(cp) :: mTemp
          mTemp = 0.0_cp
          !$OMP PARALLEL DO REDUCTION(+:mTemp)
          do k=1+pad,a%s(3)-pad; do j=1+pad,a%s(2)-pad; do i=1+pad,a%s(1)-pad
            mTemp = mTemp + a%f(i,j,k)
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
          m = mTemp
#else
          m = sum(a%f(1+pad:a%s(1)-1,1+pad:a%s(2)-1,1+pad:a%s(3)-1))
#endif
        end function

        function size_RF(a) result(s)
          implicit none
          type(realField),intent(in) :: a
          integer :: s
          s = a%s(1)*a%s(2)*a%s(3)
        end function

      ! ------------------- ALLOCATE / DEALLOCATE --------------------

        subroutine init_RF_size(a,Nx,Ny,Nz)
          implicit none
          type(realField),intent(inout) :: a
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(a%f)) deallocate(a%f)
          allocate(a%f(Nx,Ny,Nz))
          a%s = shape(a%f)
        end subroutine

        subroutine init_RF_copy(f1,f2)
          implicit none
          type(realField),intent(inout) :: f1
          type(realField),intent(in) :: f2
          integer,dimension(3) :: s
          if (.not.allocated(f2%f)) stop 'Error: trying to copy unallocated RF in RF.f90'
          s = shape(f2%f)
          if (allocated(f1%f)) deallocate(f1%f)
          allocate(f1%f(s(1),s(2),s(3)))
          f1%s = shape(f1%f)
          if (f2%b%defined) call init(f1%b,f2%b)
        end subroutine

      ! ------------------- LOCATION-BASED ALLOCATE / DEALLOCATE --------------------

        subroutine init_RF_CC(a,g)
          implicit none
          type(realField),intent(inout) :: a
          type(grid),intent(in) :: g
          call init(a,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
        end subroutine

        subroutine init_RF_Face(a,g,dir)
          implicit none
          type(realField),intent(inout) :: a
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init(a,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
          case (2); call init(a,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
          case (3); call init(a,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)
          case default; stop 'Error: dir must = 1,2,3 in init_RF_Face in RF.f90'
          end select
        end subroutine

        subroutine init_RF_Edge(a,g,dir)
          implicit none
          type(realField),intent(inout) :: a
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init(a,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
          case (2); call init(a,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
          case (3); call init(a,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)
          case default; stop 'Error: dir must = 1,2,3 in init_RF_Face in RF.f90'
          end select
        end subroutine

        subroutine init_RF_Node(a,g)
          implicit none
          type(realField),intent(inout) :: a
          type(grid),intent(in) :: g
          call init(a,g%c(1)%sn,g%c(2)%sn,g%c(3)%sn)
        end subroutine

        subroutine init_BC_val(f,val)
          implicit none
          type(realField),intent(inout) :: f
          real(cp),intent(in) :: val
          call init(f%b,val)
        end subroutine

        subroutine init_BC_vals(f,is_CC,is_Node)
          implicit none
          type(realField),intent(inout) :: f
          logical,intent(in) :: is_CC,is_Node
          logical,dimension(2) :: TF
          TF = (/is_CC,is_Node/)
          if (count(TF).gt.1) then
            stop 'Error: more than one datatype in init_BC_vals in RF.f90'
          endif
          if (is_Node) then
            call init(f%b,f%f(2,:,:),1)
            call init(f%b,f%f(:,2,:),3)
            call init(f%b,f%f(:,:,2),5)
            call init(f%b,f%f(f%s(1)-1,:,:),2)
            call init(f%b,f%f(:,f%s(2)-1,:),4)
            call init(f%b,f%f(:,:,f%s(3)-1),6)
          elseif (is_CC) then
            call init(f%b,0.5_cp*(f%f(1,:,:)+f%f(2,:,:)),1)
            call init(f%b,0.5_cp*(f%f(:,1,:)+f%f(:,2,:)),3)
            call init(f%b,0.5_cp*(f%f(:,:,1)+f%f(:,:,2)),5)
            call init(f%b,0.5_cp*(f%f(f%s(1),:,:)+f%f(f%s(1)-1,:,:)),2)
            call init(f%b,0.5_cp*(f%f(:,f%s(2),:)+f%f(:,f%s(2)-1,:)),4)
            call init(f%b,0.5_cp*(f%f(:,:,f%s(3))+f%f(:,:,f%s(3)-1)),6)
          else
            stop 'Error: field-based BC init is only available for N / CC data.'
          endif
        end subroutine

        subroutine delete_RF(a)
          implicit none
          type(realField),intent(inout) :: a
          if (allocated(a%f)) deallocate(a%f)
          call delete(a%b)
          a%s = 0
        end subroutine

        subroutine print_RF(a)
          implicit none
          type(realField),intent(in) :: a
          integer :: i,j,k
          if (allocated(a%f)) then
            write(*,*) 'shape(f) = ',a%s
            do i=1,a%s(1)
              do j=1,a%s(2)
                do k=1,a%s(3)
                  write(*,'(A4,I1,A,I1,A,I1,A4,1F15.6)') 'f(',i,',',j,',',k,') = ',a%f(i,j,k)
                enddo
              enddo
            enddo
          endif
        end subroutine

        subroutine print_physical_RF(a)
          implicit none
          type(realField),intent(in) :: a
          integer :: i,j,k
          if (allocated(a%f)) then
            write(*,*) 'shape(f) = ',a%s
            do i=2,a%s(1)-1
              do j=2,a%s(2)-1
                do k=2,a%s(3)-1
                  write(*,'(A4,I1,A,I1,A,I1,A4,1F20.10)') 'f(',i,',',j,',',k,') = ',a%f(i,j,k)
                enddo
              enddo
            enddo
          endif
        end subroutine

      end module