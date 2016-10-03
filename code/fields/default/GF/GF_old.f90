      module GF_mod
        ! Pre-processor directives: (_DEBUG_GF_,_PARALLELIZE_GF_)
        ! 
        ! Naming convention: name = operation_type1_type2
        ! 
        !      GF = type(grid_field)
        !      R  = real(cp),dimension(:,:,:)
        !      S  = real(cp)
        ! 
        ! Example(1): Adding a scalar to GF
        !             name = add_GF_S
        ! Example(2): Subtracting a real field from GF
        !             name = subtract_GF_R
        ! Example(3): Subtracting a GF from a real field
        !             name = subtract_R_GF
        ! 
        ! NOTES: GF stands for 'real field'
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
        public :: grid_field
        public :: init,delete,display,print,export,import ! Essentials

        ! Grid initialization
        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        ! BC initialization
        public :: init_BCs

        ! Monitoring

        ! Operators
        public :: assign,assign_negative
        public :: add,subtract
        public :: multiply,divide
        public :: add_product,swap
        ! Auxiliary
        public :: square,min,max,minabs,maxabs
        public :: maxabsdiff,mean,sum,size

        public :: zero_ghost_xmin_xmax
        public :: zero_ghost_ymin_ymax
        public :: zero_ghost_zmin_zmax

        type grid_field
          integer :: s_1D                            ! size
          integer,dimension(3) :: s                  ! Dimension
          real(cp),dimension(:,:,:),allocatable :: f ! field
          type(BCs) :: b
        end type

        interface init;                     module procedure init_GF_size;           end interface
        interface init;                     module procedure init_GF_copy;           end interface
        interface delete;                   module procedure delete_GF;              end interface
        interface display;                  module procedure display_GF;             end interface
        interface print;                    module procedure print_GF;               end interface
        interface export;                   module procedure export_GF;              end interface
        interface import;                   module procedure import_GF;              end interface

        interface init_CC;                  module procedure init_GF_CC;             end interface
        interface init_Face;                module procedure init_GF_Face;           end interface
        interface init_Edge;                module procedure init_GF_Edge;           end interface
        interface init_Node;                module procedure init_GF_Node;           end interface

        interface init_BCs;                 module procedure init_BC_val;            end interface
        interface init_BCs;                 module procedure init_BC_vals;           end interface

        interface assign;                   module procedure assign_GF_S;            end interface
        interface assign;                   module procedure assign_GF_GF;           end interface
        interface assign;                   module procedure assign_GF_R;            end interface
        interface assign_negative;          module procedure assign_negative_GF_GF;  end interface

        interface add;                      module procedure add_GF_GF;              end interface
        interface add;                      module procedure add_GF_GF_GF;           end interface
        interface add;                      module procedure add_GF_GF_GF_GF;        end interface
        interface add;                      module procedure add_GF_R;               end interface
        interface add;                      module procedure add_GF_S;               end interface
        interface add;                      module procedure add_S_GF;               end interface
        interface add;                      module procedure add_GF_GF9;             end interface

        interface add_product;              module procedure add_product_GF_GF_S;    end interface
        interface add_product;              module procedure add_product_GF_GF_GF;   end interface

        interface multiply;                 module procedure multiply_GF_GF;         end interface
        interface multiply;                 module procedure multiply_GF_GF_GF;      end interface
        interface multiply;                 module procedure multiply_GF_GF_S;       end interface
        interface multiply;                 module procedure multiply_GF_S;          end interface
        interface multiply;                 module procedure multiply_S_GF;          end interface

        interface subtract;                 module procedure subtract_GF_GF;         end interface
        interface subtract;                 module procedure subtract_GF_GF_GF;      end interface
        interface subtract;                 module procedure subtract_GF_R_R;        end interface
        interface subtract;                 module procedure subtract_GF_R;          end interface
        interface subtract;                 module procedure subtract_GF_S;          end interface
        interface subtract;                 module procedure subtract_S_GF;          end interface

        interface divide;                   module procedure divide_GF_GF;           end interface
        interface divide;                   module procedure divide_GF_GF_GF;        end interface
        interface divide;                   module procedure divide_GF_S_GF;         end interface
        interface divide;                   module procedure divide_GF_S;            end interface
        interface divide;                   module procedure divide_S_GF;            end interface

        interface square;                   module procedure square_GF;              end interface
        interface swap;                     module procedure swap_GF;                end interface
        interface min;                      module procedure min_GF;                 end interface
        interface max;                      module procedure max_GF;                 end interface
        interface min;                      module procedure min_pad_GF;             end interface
        interface max;                      module procedure max_pad_GF;             end interface
        interface minabs;                   module procedure minabs_GF;              end interface
        interface maxabs;                   module procedure maxabs_GF;              end interface
        interface maxabsdiff;               module procedure maxabsdiff_GF;          end interface
        interface mean;                     module procedure mean_GF;                end interface
        interface sum;                      module procedure sum_GF;                 end interface
        interface sum;                      module procedure sum_GF_pad;             end interface
        interface size;                     module procedure size_GF;                end interface

        interface zero_ghost_xmin_xmax;     module procedure zero_ghost_xmin_xmax_GF;end interface
        interface zero_ghost_ymin_ymax;     module procedure zero_ghost_ymin_ymax_GF;end interface
        interface zero_ghost_zmin_zmax;     module procedure zero_ghost_zmin_zmax_GF;end interface

      contains

        ! **********************************************************
        ! ********************* ESSENTIALS *************************
        ! **********************************************************

        subroutine init_GF_size(a,Nx,Ny,Nz)
          implicit none
          type(grid_field),intent(inout) :: a
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(a%f)) deallocate(a%f)
          allocate(a%f(Nx,Ny,Nz))
          a%s = shape(a%f)
          a%s_1D = a%s(1)*a%s(2)*a%s(3)
        end subroutine

        subroutine init_GF_copy(f1,f2)
          implicit none
          type(grid_field),intent(inout) :: f1
          type(grid_field),intent(in) :: f2
          integer,dimension(3) :: s
          if (.not.allocated(f2%f)) stop 'Error: trying to copy unallocated GF in GF.f90'
          s = shape(f2%f)
          if (allocated(f1%f)) deallocate(f1%f)
          allocate(f1%f(s(1),s(2),s(3)))
          f1%s = shape(f1%f)
          if (f2%b%defined) call init(f1%b,f2%b)
          f1%s_1D = f2%s_1D
        end subroutine

        subroutine delete_GF(a)
          implicit none
          type(grid_field),intent(inout) :: a
          if (allocated(a%f)) deallocate(a%f)
          call delete(a%b)
          a%s = 0
          a%s_1D = 0
        end subroutine

        subroutine display_GF(a,un)
          implicit none
          type(grid_field),intent(in) :: a
          integer,intent(in) :: un
          integer :: i,j,k
          if (allocated(a%f)) then
            write(*,*) 'shape(f) = ',a%s
            do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
              write(un,'(A4,I1,A,I1,A,I1,A4,1F15.6)') 'f(',i,',',j,',',k,') = ',a%f(i,j,k)
            enddo; enddo; enddo
          endif
        end subroutine

        subroutine print_GF(a)
          implicit none
          type(grid_field),intent(in) :: a
          call display(a,6)
        end subroutine

        subroutine export_GF(a,un)
          implicit none
          type(grid_field),intent(in) :: a
          integer,intent(in) :: un
          integer :: i,j,k
          if (allocated(a%f)) then
          write(un,*) 'shape(f) = '
          write(un,*) a%s
          write(un,*) 'size(f) = '
          write(un,*) a%s_1D
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            write(un,*) a%f(i,j,k)
          enddo; enddo; enddo
          else; stop 'Error: trying to export unallocated GF in export_GF in GF.f90'
          endif
          call export(a%b,un)
        end subroutine

        subroutine import_GF(a,un)
          implicit none
          type(grid_field),intent(inout) :: a
          integer,intent(in) :: un
          integer :: i,j,k
          call delete(a)
          read(un,*) 
          read(un,*) a%s
          read(un,*) 
          read(un,*) a%s_1D
          allocate(a%f(a%s(1),a%s(2),a%s(3)))
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            read(un,*) a%f(i,j,k)
          enddo; enddo; enddo
          call import(a%b,un)
        end subroutine

        ! **********************************************************
        ! **********************************************************
        ! **********************************************************

        subroutine assign_GF_GF(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
#ifdef _PARALLELIZE_GF_
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

        subroutine assign_GF_R(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),dimension(:,:,:),intent(in) :: b
#ifdef _PARALLELIZE_GF_
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

        subroutine assign_GF_S(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_GF_
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

        subroutine assign_negative_GF_GF(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
#ifdef _PARALLELIZE_GF_
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

        subroutine add_GF_GF(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
#ifdef _PARALLELIZE_GF_
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

        subroutine add_GF_GF_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c
#ifdef _PARALLELIZE_GF_
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

        subroutine add_GF_GF_GF_GF(a,b,c,d)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c,d
#ifdef _PARALLELIZE_GF_
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

        subroutine add_GF_R(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),dimension(:,:,:),intent(in) :: b
#ifdef _PARALLELIZE_GF_
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

        subroutine add_GF_S(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
#ifdef _PARALLELIZE_GF_
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

        subroutine add_S_GF(g2,a)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_GF_
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

        subroutine add_GF_GF9(A,B1,B2,B3,B4,B5,B6,B7,B8,B9)
          implicit none
          type(grid_field),intent(inout) :: A
          type(grid_field),intent(in) :: B1,B2,B3,B4,B5,B6,B7,B8,B9
#ifdef _PARALLELIZE_GF_
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

        subroutine add_product_GF_GF_S(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          real(cp),intent(in) :: c
#ifdef _PARALLELIZE_GF_
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

        subroutine add_product_GF_GF_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
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

        subroutine subtract_GF_GF(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
#ifdef _PARALLELIZE_GF_
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

        subroutine subtract_GF_GF_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c
#ifdef _PARALLELIZE_GF_
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

      ! ------------------- MULTIPLY ------------------------

        subroutine multiply_GF_GF(a,b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
#ifdef _PARALLELIZE_GF_
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

        subroutine multiply_GF_GF_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b,c
#ifdef _PARALLELIZE_GF_
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

        subroutine multiply_GF_GF_S(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          real(cp),intent(in) :: c
#ifdef _PARALLELIZE_GF_
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

      ! ------------------- DIVIDE ------------------------

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

      ! ------------------- OTHER ------------------------

        subroutine square_GF(a)
          implicit none
          type(grid_field),intent(inout) :: a
#ifdef _PARALLELIZE_GF_
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

        subroutine swap_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a,b,c
#ifdef _PARALLELIZE_GF_
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

        function min_GF(a) result(m)
          implicit none
          type(grid_field),intent(in) :: a
          real(cp) :: m
          m = minval(a%f)
        end function

        function max_GF(a) result(m)
          implicit none
          type(grid_field),intent(in) :: a
          real(cp) :: m
          m = maxval(a%f)
        end function

        function min_pad_GF(a,pad) result(m)
          implicit none
          type(grid_field),intent(in) :: a
          integer,intent(in) :: pad
          real(cp) :: m
          m = minval(a%f(1+pad:a%s(1)-pad,1+pad:a%s(2)-pad,1+pad:a%s(3)-pad))
        end function

        function max_pad_GF(a,pad) result(m)
          implicit none
          type(grid_field),intent(in) :: a
          integer,intent(in) :: pad
          real(cp) :: m
          m = maxval(a%f(1+pad:a%s(1)-pad,1+pad:a%s(2)-pad,1+pad:a%s(3)-pad))
        end function

        function minabs_GF(a) result(m)
          implicit none
          type(grid_field),intent(in) :: a
          real(cp) :: m
          m = minval(abs(a%f))
        end function

        function maxabs_GF(a) result(m)
          implicit none
          type(grid_field),intent(in) :: a
          real(cp) :: m
          m = maxval(abs(a%f))
        end function

        function maxabsdiff_GF(a,b) result(m)
          implicit none
          type(grid_field),intent(in) :: a,b
          real(cp) :: m
          m = maxval(abs(a%f-b%f))
        end function

        function mean_GF(a) result(m)
          implicit none
          type(grid_field),intent(in) :: a
          real(cp) :: m
          m = sum(a)/(max(1,size(a%f)))
        end function

        function sum_GF(a) result(m)
          implicit none
          type(grid_field),intent(in) :: a
          real(cp) :: m
#ifdef _PARALLELIZE_GF_
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

        function sum_GF_pad(a,pad) result(m)
          implicit none
          type(grid_field),intent(in) :: a
          integer,intent(in) :: pad
          real(cp) :: m
#ifdef _PARALLELIZE_GF_
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

        subroutine zero_ghost_xmin_xmax_GF(f)
          implicit none
          type(grid_field),intent(inout) :: f
          integer :: j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=1,f%s(3); do j=1,f%s(2)
            f%f(1,j,k) = 0.0_cp
            f%f(f%s(1),j,k) = 0.0_cp
          enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine zero_ghost_ymin_ymax_GF(f)
          implicit none
          type(grid_field),intent(inout) :: f
          integer :: i,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=1,f%s(3); do i=1,f%s(1)
            f%f(i,1,k) = 0.0_cp
            f%f(i,f%s(2),k) = 0.0_cp
          enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine zero_ghost_zmin_zmax_GF(f)
          implicit none
          type(grid_field),intent(inout) :: f
          integer :: i,j
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do j=1,f%s(2); do i=1,f%s(1)
            f%f(i,j,1) = 0.0_cp
            f%f(i,j,f%s(3)) = 0.0_cp
          enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        function size_GF(a) result(s)
          implicit none
          type(grid_field),intent(in) :: a
          integer :: s
          s = a%s_1D
        end function

      ! ------------------- LOCATION-BASED ALLOCATE / DEALLOCATE --------------------

        subroutine init_GF_CC(a,g)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid),intent(in) :: g
          call init(a,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
        end subroutine

        subroutine init_GF_Face(a,g,dir)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init(a,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
          case (2); call init(a,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
          case (3); call init(a,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)
          case default; stop 'Error: dir must = 1,2,3 in init_GF_Face in GF.f90'
          end select
        end subroutine

        subroutine init_GF_Edge(a,g,dir)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init(a,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
          case (2); call init(a,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
          case (3); call init(a,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)
          case default; stop 'Error: dir must = 1,2,3 in init_GF_Face in GF.f90'
          end select
        end subroutine

        subroutine init_GF_Node(a,g)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid),intent(in) :: g
          call init(a,g%c(1)%sn,g%c(2)%sn,g%c(3)%sn)
        end subroutine

        subroutine init_BC_val(f,val)
          implicit none
          type(grid_field),intent(inout) :: f
          real(cp),intent(in) :: val
          call init(f%b,val)
        end subroutine

        subroutine init_BC_vals(f,is_CC,is_Node)
          implicit none
          type(grid_field),intent(inout) :: f
          logical,intent(in) :: is_CC,is_Node
          logical,dimension(2) :: L
          L = (/is_CC,is_Node/)
          if (count(L).gt.1) then
            stop 'Error: more than one datatype in init_BC_vals in GF.f90'
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

      end module