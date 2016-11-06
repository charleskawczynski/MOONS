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

        subroutine curl_curl_x_GF(C,X,Y,Z,D,X_L,X_U,&
          Y_D1_D2,Z_D1_D2,&
          Y_D1_U2,Z_D1_U2,&
          Y_U1_D2,Z_U1_D2,&
          Y_U1_U2,Z_U1_U2)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,D
          type(grid_field),dimension(3),intent(in) :: X_L,X_U
          type(grid_field),intent(in) :: Y_D1_D2,Z_D1_D2
          type(grid_field),intent(in) :: Y_D1_U2,Z_D1_U2
          type(grid_field),intent(in) :: Y_U1_D2,Z_U1_D2
          type(grid_field),intent(in) :: Y_U1_U2,Z_U1_U2
          integer :: i,j,k
          ! write(*,*) 'sum(Y_D1_D2)=',sum(Y_D1_D2)
          ! write(*,*) 'sum(Y_D1_U2)=',sum(Y_D1_U2)
          ! write(*,*) 'sum(Y_U1_D2)=',sum(Y_U1_D2)
          ! write(*,*) 'sum(Y_U1_U2)=',sum(Y_U1_U2)
          ! write(*,*) 'sum(Z_D1_D2)=',sum(Z_D1_D2)
          ! write(*,*) 'sum(Z_D1_U2)=',sum(Z_D1_U2)
          ! write(*,*) 'sum(Z_U1_D2)=',sum(Z_U1_D2)
          ! write(*,*) 'sum(Z_U1_U2)=',sum(Z_U1_U2)
          ! stop 'Done'

#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          X%f( i ,j+1, k )*X_U(2)%f(i,j,k)+&
          X%f( i ,j-1, k )*X_L(2)%f(i,j,k)+&
          X%f( i , j ,k+1)*X_U(3)%f(i,j,k)+&
          X%f( i , j ,k-1)*X_L(3)%f(i,j,k)+&
          ! 
          Y%f(i-1, j , k )*Y_D1_D2%f(i,j-1,k-1)+&
          Y%f(i-1,j+1, k )*Y_D1_U2%f(i,j-1,k-1)+&
          Y%f( i , j , k )*Y_U1_D2%f(i,j-1,k-1)+&
          Y%f( i ,j+1, k )*Y_U1_U2%f(i,j-1,k-1)+&
          ! 
          Z%f(i-1, j , k )*Z_D1_D2%f(i,j-1,k-1)+&
          Z%f(i-1, j ,k+1)*Z_D1_U2%f(i,j-1,k-1)+&
          Z%f( i , j , k )*Z_U1_D2%f(i,j-1,k-1)+&
          Z%f( i , j ,k+1)*Z_U1_U2%f(i,j-1,k-1)+&
          ! 
          X%f( i , j , k )*D%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine curl_curl_y_GF(C,X,Y,Z,D,Y_L,Y_U,&
          X_D1_D2,Z_D1_D2,&
          X_D1_U2,Z_D1_U2,&
          X_U1_D2,Z_U1_D2,&
          X_U1_U2,Z_U1_U2)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,D
          type(grid_field),dimension(3),intent(in) :: Y_L,Y_U
          type(grid_field),intent(in) :: X_D1_D2,Z_D1_D2
          type(grid_field),intent(in) :: X_D1_U2,Z_D1_U2
          type(grid_field),intent(in) :: X_U1_D2,Z_U1_D2
          type(grid_field),intent(in) :: X_U1_U2,Z_U1_U2
          integer :: i,j,k

#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          ! 
          X%f( i ,j-1, k )*X_D1_D2%f(i-1,j,k-1)+&
          X%f(i+1,j-1, k )*X_D1_U2%f(i-1,j,k-1)+&
          X%f( i , j , k )*X_U1_D2%f(i-1,j,k-1)+&
          X%f(i+1, j , k )*X_U1_U2%f(i-1,j,k-1)+&
          ! 
          Y%f(i+1, j , k )*Y_U(1)%f(i,j,k)+&
          Y%f(i-1, j , k )*Y_L(1)%f(i,j,k)+&
          Y%f( i , j ,k+1)*Y_U(3)%f(i,j,k)+&
          Y%f( i , j ,k-1)*Y_L(3)%f(i,j,k)+&
          ! 
          Z%f( i ,j-1, k )*Z_D1_D2%f(i-1,j,k-1)+&
          Z%f( i ,j-1,k+1)*Z_D1_U2%f(i-1,j,k-1)+&
          Z%f( i , j , k )*Z_U1_D2%f(i-1,j,k-1)+&
          Z%f( i , j ,k+1)*Z_U1_U2%f(i-1,j,k-1)+&
          ! 
          Y%f( i , j , k )*D%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine curl_curl_z_GF(C,X,Y,Z,D,Z_L,Z_U,&
          X_D1_D2,Y_D1_D2,&
          X_D1_U2,Y_D1_U2,&
          X_U1_D2,Y_U1_D2,&
          X_U1_U2,Y_U1_U2)
          implicit none
          type(grid_field),intent(inout) :: C
          type(grid_field),intent(in) :: X,Y,Z,D
          type(grid_field),dimension(3),intent(in) :: Z_L,Z_U
          type(grid_field),intent(in) :: X_D1_D2,Y_D1_D2
          type(grid_field),intent(in) :: X_D1_U2,Y_D1_U2
          type(grid_field),intent(in) :: X_U1_D2,Y_U1_D2
          type(grid_field),intent(in) :: X_U1_U2,Y_U1_U2
          integer :: i,j,k

#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,C%s(3)-1; do j=2,C%s(2)-1; do i=2,C%s(1)-1
          C%f( i , j , k ) = &
          ! 
          X%f( i , j ,k-1)*X_D1_D2%f(i-1,j-1,k)+&
          X%f(i+1, j ,k-1)*X_D1_U2%f(i-1,j-1,k)+&
          X%f( i , j , k )*X_U1_D2%f(i-1,j-1,k)+&
          X%f(i+1, j , k )*X_U1_U2%f(i-1,j-1,k)+&
          ! 
          Y%f( i , j ,k-1)*Y_D1_D2%f(i-1,j-1,k)+&
          Y%f( i ,j+1,k-1)*Y_D1_U2%f(i-1,j-1,k)+&
          Y%f( i , j , k )*Y_U1_D2%f(i-1,j-1,k)+&
          Y%f( i ,j+1, k )*Y_U1_U2%f(i-1,j-1,k)+&
          ! 
          Z%f(i+1, j , k )*Z_U(1)%f(i,j,k)+&
          Z%f(i-1, j , k )*Z_L(1)%f(i,j,k)+&
          Z%f( i ,j+1, k )*Z_U(2)%f(i,j,k)+&
          Z%f( i ,j-1, k )*Z_L(2)%f(i,j,k)+&
          ! 
          Z%f( i , j , k )*D%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

      end module