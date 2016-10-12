      module GF_aux_mod
        use GF_base_mod
        use GF_assign_mod
        use grid_mod
        use data_location_mod
        use current_precision_mod
        implicit none
        private

        public :: square,swap
        public :: min,max,minabs,maxabs
        public :: maxabsdiff,mean,sum,size

        public :: zero_ghost_xmin_xmax
        public :: zero_ghost_ymin_ymax
        public :: zero_ghost_zmin_zmax

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

        subroutine swap_GF(a,b,c)
          implicit none
          type(grid_field),intent(inout) :: a,b,c
#ifdef _PARALLELIZE_GF_
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

      end module