      module GF_laplacian_mod
        use GF_base_mod
        use current_precision_mod
        implicit none
        private
        public :: laplacian
        interface laplacian;   module procedure laplacian_GF;  end interface

        contains

        subroutine laplacian_GF(lapX,X,L,D,U)
          implicit none
          type(grid_field),intent(inout) :: lapX
          type(grid_field),dimension(3),intent(in) :: L,U
          type(grid_field),intent(in) :: D,X
          integer :: i,j,k
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO

#endif
          do k=2,lapX%s(3)-1; do j=2,lapX%s(2)-1; do i=2,lapX%s(1)-1
          lapX%f( i , j , k ) = &
             X%f(i+1, j , k )*U(1)%f(i,j,k)+&
             X%f(i-1, j , k )*L(1)%f(i,j,k)+&
             X%f( i ,j+1, k )*U(2)%f(i,j,k)+&
             X%f( i ,j-1, k )*L(2)%f(i,j,k)+&
             X%f( i , j ,k+1)*U(3)%f(i,j,k)+&
             X%f( i , j ,k-1)*L(3)%f(i,j,k)+&
             X%f( i , j , k )*D%f(i,j,k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

      end module