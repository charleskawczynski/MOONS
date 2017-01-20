      module GF_cross_product_mod
        ! Compiler flags: (_PARALLELIZE_CROSS_PRODUCT_GF_)
        use GF_base_mod
        use GF_assign_mod
        implicit none
        private

        public :: cross_product_x
        public :: cross_product_y
        public :: cross_product_z
        interface cross_product_x;  module procedure cross_product_x_GF;   end interface
        interface cross_product_y;  module procedure cross_product_y_GF;   end interface
        interface cross_product_z;  module procedure cross_product_z_GF;   end interface

        contains

       subroutine cross_product_x_GF(AcrossB,Ay,Az,By,Bz)
         implicit none
         type(grid_field),intent(inout) :: AcrossB
         type(grid_field),intent(in) :: Ay,Az,By,Bz
         integer :: i,j,k
#ifdef _PARALLELIZE_CROSS_PRODUCT_GF_
         !$OMP PARALLEL DO

#endif
         do k=1,AcrossB%s(3); do j=1,AcrossB%s(2); do i=1,AcrossB%s(1)
           AcrossB%f(i,j,k) = Ay%f(i,j,k)*Bz%f(i,j,k) - Az%f(i,j,k)*By%f(i,j,k)
         enddo; enddo; enddo
#ifdef _PARALLELIZE_CROSS_PRODUCT_GF_
         !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine cross_product_y_GF(AcrossB,Ax,Az,Bx,Bz)
         implicit none
         type(grid_field),intent(inout) :: AcrossB
         type(grid_field),intent(in) :: Ax,Az,Bx,Bz
         integer :: i,j,k
#ifdef _PARALLELIZE_CROSS_PRODUCT_GF_
         !$OMP PARALLEL DO

#endif
         do k=1,AcrossB%s(3); do j=1,AcrossB%s(2); do i=1,AcrossB%s(1)
           AcrossB%f(i,j,k) = -(Ax%f(i,j,k)*Bz%f(i,j,k) - Az%f(i,j,k)*Bx%f(i,j,k))
         enddo; enddo; enddo
#ifdef _PARALLELIZE_CROSS_PRODUCT_GF_
         !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine cross_product_z_GF(AcrossB,Ax,Ay,Bx,By)
         implicit none
         type(grid_field),intent(inout) :: AcrossB
         type(grid_field),intent(in) :: Ax,Ay,Bx,By
         integer :: i,j,k
#ifdef _PARALLELIZE_CROSS_PRODUCT_GF_
         !$OMP PARALLEL DO

#endif
         do k=1,AcrossB%s(3); do j=1,AcrossB%s(2); do i=1,AcrossB%s(1)
           AcrossB%f(i,j,k) = Ax%f(i,j,k)*By%f(i,j,k) - Ay%f(i,j,k)*Bx%f(i,j,k)
         enddo; enddo; enddo
#ifdef _PARALLELIZE_CROSS_PRODUCT_GF_
         !$OMP END PARALLEL DO

#endif
       end subroutine

      end module