      module GF_aux_mod
        use GF_base_mod
        use GF_assign_mod
        use GF_subtract_mod
        use grid_mod
        use data_location_mod
        use current_precision_mod
        implicit none
        private

        public :: swap
        public :: abs
        public :: min,max,amin,amax
        public :: mean,sum,size

        public :: insist_amax_lt_tol

        interface swap;                     module procedure swap_GF;                end interface
        interface abs;                      module procedure abs_GF;                 end interface
        interface min;                      module procedure min_GF;                 end interface
        interface max;                      module procedure max_GF;                 end interface
        interface min;                      module procedure min_pad_GF;             end interface
        interface max;                      module procedure max_pad_GF;             end interface
        interface amin;                     module procedure amin_GF;                end interface
        interface amax;                     module procedure amax_GF;                end interface
        interface mean;                     module procedure mean_GF;                end interface
        interface sum;                      module procedure sum_GF;                 end interface
        interface sum;                      module procedure sum_GF_pad;             end interface
        interface size;                     module procedure size_GF;                end interface
        interface insist_amax_lt_tol;       module procedure insist_amax_lt_tol_GF;  end interface
        interface insist_amax_lt_tol;       module procedure insist_amax_lt_tol_GF_2;end interface

        contains

        subroutine abs_GF(a)
          implicit none
          type(grid_field),intent(inout) :: a
#ifdef _PARALLELIZE_GF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,k) = abs(a%f(i,j,k))
          enddo; enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f = abs(a%f)
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

        function amin_GF(a) result(m)
          implicit none
          type(grid_field),intent(in) :: a
          real(cp) :: m
          m = minval(abs(a%f))
        end function

        function amax_GF(a) result(m)
          implicit none
          type(grid_field),intent(in) :: a
          real(cp) :: m
          m = maxval(abs(a%f))
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

        function size_GF(a) result(s)
          implicit none
          type(grid_field),intent(in) :: a
          integer :: s
          s = a%s_1D
        end function

        subroutine insist_amax_lt_tol_GF(U,caller)
          implicit none
          type(grid_field),intent(in) :: U
          character(len=*),intent(in) :: caller
          real(cp) :: tol,amx_U
          tol = 10.0_cp**(-10.0_cp)
          amx_U = amax(U)
          if (.not.(amx_U.lt.tol)) then
            write(*,*) 'Error: tol > amax(U) in ',caller,' in insist_above_tol_GF in GF_aux.f90'
            write(*,*) 'tol = ',tol
            write(*,*) 'amax(U) = ',amx_U
            stop 'Done'
          endif
        end subroutine

        subroutine insist_amax_lt_tol_GF_2(A,B,caller)
          implicit none
          type(grid_field),intent(in) :: A,B
          character(len=*),intent(in) :: caller
          type(grid_field) :: C
          real(cp) :: tol,amx_C
          tol = 10.0_cp**(-10.0_cp)
          call init(C,A)
          call subtract(C,A,B)
          amx_C = amax(C)
          if (.not.(amx_C.lt.tol)) then
            write(*,*) '-------------------------------------------- A'; call print_physical(A)
            write(*,*) '-------------------------------------------- B'; call print_physical(B)
            write(*,*) '--------------------------------------------'
            write(*,*) 'Error: tol > amax(C) in ',caller,' in insist_above_tol_GF in GF_aux.f90'
            write(*,*) 'tol = ',tol
            write(*,*) 'amax(C) = ',amx_C
            stop 'Done'
          endif
          call delete(C)
        end subroutine

      end module