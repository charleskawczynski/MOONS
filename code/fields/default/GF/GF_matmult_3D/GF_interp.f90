      module GF_interp_mod
        use GF_base_mod
        use current_precision_mod
        implicit none
        private
        public :: interp_x
        public :: interp_y
        public :: interp_z
        interface interp_x;   module procedure interp_x_GF;  end interface
        interface interp_y;   module procedure interp_y_GF;  end interface
        interface interp_z;   module procedure interp_z_GF;  end interface

        contains

        subroutine interp_x_GF(G,F,U,D)
          implicit none
          type(grid_field),intent(inout) :: G
          type(grid_field),intent(in) :: F,U,D
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,G%s(3)-1; do j=2,G%s(2)-1; do i=2,G%s(1)-1
             G%f( i , j , k ) = &
             F%f(i+1, j , k )*U%f(i,j,k)+&
             F%f( i , j , k )*D%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine interp_y_GF(G,F,U,D)
          implicit none
          type(grid_field),intent(inout) :: G
          type(grid_field),intent(in) :: F,U,D
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,G%s(3)-1; do j=2,G%s(2)-1; do i=2,G%s(1)-1
             G%f( i , j , k ) = &
             F%f( i ,j+1, k )*U%f(i,j,k)+&
             F%f( i , j , k )*D%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine interp_z_GF(G,F,U,D)
          implicit none
          type(grid_field),intent(inout) :: G
          type(grid_field),intent(in) :: F,U,D
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,G%s(3)-1; do j=2,G%s(2)-1; do i=2,G%s(1)-1
             G%f( i , j , k ) = &
             F%f( i , j ,k+1)*U%f(i,j,k)+&
             F%f( i , j , k )*D%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

      end module