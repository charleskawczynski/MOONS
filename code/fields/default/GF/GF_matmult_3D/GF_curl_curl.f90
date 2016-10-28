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

        subroutine curl_curl_x_GF(C,X,Y,Z,D,LX,LY,LZ,UX,UY,UZ)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,D
          type(grid_field),dimension(3),intent(in) :: LX,LY,LZ,UX,UY,UZ
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          X%f( i ,j+1, k )*UX(2)%f(i,j,k)+&
          X%f( i ,j-1, k )*LX(2)%f(i,j,k)+&
          X%f( i , j ,k+1)*UX(3)%f(i,j,k)+&
          X%f( i , j ,k-1)*LX(3)%f(i,j,k)+&
          X%f( i , j , k )*D%f(i,j,k)+&
          Y%f(i+1, j , k )*UY(1)%f(i,j,k)+&
          Y%f(i-1, j , k )*LY(1)%f(i,j,k)+&
          Y%f( i ,j+1, k )*UY(2)%f(i,j,k)+&
          Y%f( i ,j-1, k )*LY(2)%f(i,j,k)+&
          Z%f(i+1, j , k )*UZ(1)%f(i,j,k)+&
          Z%f(i-1, j , k )*LZ(1)%f(i,j,k)+&
          Z%f( i , j ,k+1)*UZ(3)%f(i,j,k)+&
          Z%f( i , j ,k-1)*LZ(3)%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine curl_curl_y_GF(C,X,Y,Z,D,LX,LY,LZ,UX,UY,UZ)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,D
          type(grid_field),dimension(3),intent(in) :: LX,LY,LZ,UX,UY,UZ
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          X%f(i+1, j , k )*UX(1)%f(i,j,k)+&
          X%f(i-1, j , k )*LX(1)%f(i,j,k)+&
          X%f( i ,j+1, k )*UX(2)%f(i,j,k)+&
          X%f( i ,j-1, k )*LX(2)%f(i,j,k)+&
          Y%f(i+1, j , k )*UY(1)%f(i,j,k)+&
          Y%f(i-1, j , k )*LY(1)%f(i,j,k)+&
          Y%f( i , j ,k+1)*UY(3)%f(i,j,k)+&
          Y%f( i , j ,k-1)*LY(3)%f(i,j,k)+&
          Y%f( i , j , k )*D%f(i,j,k)+&
          Z%f( i ,j+1, k )*UZ(2)%f(i,j,k)+&
          Z%f( i ,j-1, k )*LZ(2)%f(i,j,k)+&
          Z%f( i , j ,k+1)*UZ(3)%f(i,j,k)+&
          Z%f( i , j ,k-1)*LZ(3)%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine curl_curl_z_GF(C,X,Y,Z,D,LX,LY,LZ,UX,UY,UZ)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,D
          type(grid_field),dimension(3),intent(in) :: LX,LY,LZ,UX,UY,UZ
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          X%f(i+1, j , k )*UX(1)%f(i,j,k)+&
          X%f(i-1, j , k )*LX(1)%f(i,j,k)+&
          X%f( i , j ,k+1)*UX(3)%f(i,j,k)+&
          X%f( i , j ,k-1)*LX(3)%f(i,j,k)+&
          Y%f( i ,j+1, k )*UY(2)%f(i,j,k)+&
          Y%f( i ,j-1, k )*LY(2)%f(i,j,k)+&
          Y%f( i , j ,k+1)*UY(3)%f(i,j,k)+&
          Y%f( i , j ,k-1)*LY(3)%f(i,j,k)+&
          Z%f(i+1, j , k )*UZ(1)%f(i,j,k)+&
          Z%f(i-1, j , k )*LZ(1)%f(i,j,k)+&
          Z%f( i ,j+1, k )*UZ(2)%f(i,j,k)+&
          Z%f( i ,j-1, k )*LZ(2)%f(i,j,k)+&
          Z%f( i , j , k )*D%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

      end module