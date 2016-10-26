      module GF_curl_curl_mod
        use GF_base_mod
        use current_precision_mod
        implicit none
        private
        public :: curl_curl_x
        public :: curl_curl_y
        public :: curl_curl_z
        interface curl_curl_x;   module procedure curl_curl_x_GF;  end interface
        interface curl_curl_y;   module procedure curl_curl_y_GF;  end interface
        interface curl_curl_z;   module procedure curl_curl_z_GF;  end interface

        contains

        subroutine curl_curl_x_GF(C,X,Y,Z,L,D,U)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,D
          type(grid_field),dimension(3),intent(in) :: L,U
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          X%f( i ,j+1, k )*U(1)%f(i,j,k)+&
          X%f( i ,j-1, k )*L(1)%f(i,j,k)+&
          X%f( i , j ,k+1)*U(1)%f(i,j,k)+&
          X%f( i , j ,k-1)*L(1)%f(i,j,k)+&
          X%f( i , j , k )*D%f(i,j,k)+&
          Y%f(i+1, j , k )*U(2)%f(i,j,k)+&
          Y%f(i-1, j , k )*L(2)%f(i,j,k)+&
          Y%f( i ,j+1, k )*U(2)%f(i,j,k)+&
          Y%f( i ,j-1, k )*L(2)%f(i,j,k)+&
          Z%f(i+1, j , k )*U(3)%f(i,j,k)+&
          Z%f(i-1, j , k )*L(3)%f(i,j,k)+&
          Z%f( i , j ,k+1)*U(3)%f(i,j,k)+&
          Z%f( i , j ,k-1)*L(3)%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine curl_curl_y_GF(C,X,Y,Z,L,D,U)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,D
          type(grid_field),dimension(3),intent(in) :: L,U
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          X%f(i+1, j , k )*U(1)%f(i,j,k)+&
          X%f(i-1, j , k )*L(1)%f(i,j,k)+&
          X%f( i ,j+1, k )*U(1)%f(i,j,k)+&
          X%f( i ,j-1, k )*L(1)%f(i,j,k)+&
          Y%f(i+1, j , k )*U(2)%f(i,j,k)+&
          Y%f(i-1, j , k )*L(2)%f(i,j,k)+&
          Y%f( i , j ,k+1)*U(2)%f(i,j,k)+&
          Y%f( i , j ,k-1)*L(2)%f(i,j,k)+&
          Y%f( i , j , k )*D%f(i,j,k)+&
          Z%f( i ,j+1, k )*U(3)%f(i,j,k)+&
          Z%f( i ,j-1, k )*L(3)%f(i,j,k)+&
          Z%f( i , j ,k+1)*U(3)%f(i,j,k)+&
          Z%f( i , j ,k-1)*L(3)%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine curl_curl_z_GF(C,X,Y,Z,L,D,U)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,D
          type(grid_field),dimension(3),intent(in) :: L,U
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          X%f(i+1, j , k )*U(1)%f(i,j,k)+&
          X%f(i-1, j , k )*L(1)%f(i,j,k)+&
          X%f( i , j ,k+1)*U(1)%f(i,j,k)+&
          X%f( i , j ,k-1)*L(1)%f(i,j,k)+&
          Y%f( i ,j+1, k )*U(2)%f(i,j,k)+&
          Y%f( i ,j-1, k )*L(2)%f(i,j,k)+&
          Y%f( i , j ,k+1)*U(2)%f(i,j,k)+&
          Y%f( i , j ,k-1)*L(2)%f(i,j,k)+&
          Z%f(i+1, j , k )*U(3)%f(i,j,k)+&
          Z%f(i-1, j , k )*L(3)%f(i,j,k)+&
          Z%f( i ,j+1, k )*U(3)%f(i,j,k)+&
          Z%f( i ,j-1, k )*L(3)%f(i,j,k)+&
          Z%f( i , j , k )*D%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

      end module