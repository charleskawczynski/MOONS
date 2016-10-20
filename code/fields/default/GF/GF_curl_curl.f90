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

        subroutine curl_curl_x_GF(C,X,Y,Z,sig_inv,stencil)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,sig_inv
          type(stencil_3D) :: stencil
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          U%f(i+1, j , k )*stencil%x_L(i)
          U%f(i-1, j , k )*stencil%x_D(i)
          U%f( i ,j+1, k )*stencil%y_L(j)
          U%f( i ,j-1, k )*stencil%y_D(j)
          U%f( i , j ,k+1)*stencil%z_L(k)
          U%f( i , j ,k-1)*stencil%z_D(k)
          U%f( i , j , k )*stencil%D(i)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine curl_curl_y_GF(C,X,Y,Z,sig_inv,stencil)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,sig_inv
          type(stencil_3D) :: stencil
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          U%f(i+1, j , k )*stencil%x_L(i)
          U%f(i-1, j , k )*stencil%x_D(i)
          U%f( i ,j+1, k )*stencil%y_L(j)
          U%f( i ,j-1, k )*stencil%y_D(j)
          U%f( i , j ,k+1)*stencil%z_L(k)
          U%f( i , j ,k-1)*stencil%z_D(k)
          U%f( i , j , k )*stencil%D(i)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine curl_curl_z_GF(C,X,Y,Z,stencil)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z
          type(stencil_3D) :: stencil
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          U%f(i+1, j , k )*stencil%x_L(i)
          U%f(i-1, j , k )*stencil%x_D(i)
          U%f( i ,j+1, k )*stencil%y_L(j)
          U%f( i ,j-1, k )*stencil%y_D(j)
          U%f( i , j ,k+1)*stencil%z_L(k)
          U%f( i , j ,k-1)*stencil%z_D(k)
          U%f( i , j , k )*stencil%D(i)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

      end module