      module GF_curl_curl_mod
        use GF_base_mod
        use GF_aux_mod
        use GF_multiply_mod
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

        subroutine curl_curl_x_GF(C,X,Y,Z,D,X_L,X_U,Y_D,Y_U,Z_D,Z_U)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,D
          type(grid_field),dimension(3),intent(in) :: X_L,X_U,Y_D,Y_U,Z_D,Z_U
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          X%f( i ,j+1, k )*X_U(2)%f(i,j,k)+&
          X%f( i ,j-1, k )*X_L(2)%f(i,j,k)+&
          X%f( i , j ,k+1)*X_U(3)%f(i,j,k)+&
          X%f( i , j ,k-1)*X_L(3)%f(i,j,k)+&
          Y%f( i , j , k )*Y_U(1)%f(i,j,k)+&
          Y%f(i-1, j , k )*Y_D(1)%f(i,j,k)+&
          Y%f( i ,j+1, k )*Y_U(2)%f(i,j,k)+&
          Y%f(i-1,j+1, k )*Y_D(2)%f(i,j,k)+&
          Z%f( i , j , k )*Z_U(1)%f(i,j,k)+&
          Z%f(i-1, j , k )*Z_D(1)%f(i,j,k)+&
          Z%f( i , j ,k+1)*Z_U(3)%f(i,j,k)+&
          Z%f(i-1, j ,k+1)*Z_D(3)%f(i,j,k)+&
          X%f( i , j , k )*D%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine curl_curl_y_GF(C,X,Y,Z,D,X_D,X_U,Y_L,Y_U,Z_D,Z_U)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,D
          type(grid_field),dimension(3),intent(in) :: X_D,X_U,Y_L,Y_U,Z_D,Z_U
          integer :: i,j,k

#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          ! X%f( i , j , k )*X_U(1)%f(i,j,k)+&
          ! X%f(i+1, j , k )*X_D(1)%f(i,j,k)+&
          ! X%f( i ,j-1, k )*X_U(2)%f(i,j,k)+&
          ! X%f(i+1,j-1, k )*X_D(2)%f(i,j,k)+&
          Y%f(i+1, j , k )*Y_U(1)%f(i,j,k)+&
          Y%f(i-1, j , k )*Y_L(1)%f(i,j,k)+&
          Y%f( i ,j+1, k )*Y_U(2)%f(i,j,k)+&
          Y%f( i ,j-1, k )*Y_L(2)%f(i,j,k)+&
          Y%f( i , j ,k+1)*Y_U(3)%f(i,j,k)+&
          Y%f( i , j ,k-1)*Y_L(3)%f(i,j,k)+&
          ! Z%f( i , j , k )*Z_U(2)%f(i,j,k)+&
          ! Z%f( i ,j-1, k )*Z_D(2)%f(i,j,k)+&
          ! Z%f( i , j ,k+1)*Z_U(3)%f(i,j,k)+&
          ! Z%f( i ,j-1,k+1)*Z_D(3)%f(i,j,k)+&
          Y%f( i , j , k )*D%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine curl_curl_z_GF(C,X,Y,Z,D,X_D,X_U,Y_D,Y_U,Z_L,Z_U)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,D
          type(grid_field),dimension(3),intent(in) :: X_D,X_U,Y_D,Y_U,Z_L,Z_U
          integer :: i,j,k

#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          ! X%f( i , j , k )*X_U(1)%f(i,j,k)+&
          ! X%f(i+1, j , k )*X_D(1)%f(i,j,k)+&
          ! X%f( i , j ,k-1)*X_U(3)%f(i,j,k)+&
          ! X%f(i+1, j ,k-1)*X_D(3)%f(i,j,k)+&
          ! Y%f( i , j , k )*Y_U(2)%f(i,j,k)+&
          ! Y%f( i ,j+1, k )*Y_D(2)%f(i,j,k)+&
          ! Y%f( i , j ,k-1)*Y_U(3)%f(i,j,k)+&
          ! Y%f( i ,j+1,k-1)*Y_D(3)%f(i,j,k)+&
          Z%f(i+1, j , k )*Z_U(1)%f(i,j,k)+&
          Z%f(i-1, j , k )*Z_L(1)%f(i,j,k)+&
          Z%f( i ,j+1, k )*Z_U(2)%f(i,j,k)+&
          Z%f( i ,j-1, k )*Z_L(2)%f(i,j,k)+&
          Z%f( i , j ,k+1)*Z_U(3)%f(i,j,k)+&
          Z%f( i , j ,k-1)*Z_L(3)%f(i,j,k)+&
          Z%f( i , j , k )*D%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

      end module