      module GF_plane_sum_mod
        ! Compiler flags: (_PARALLELIZE_GF_PLANE_SUM_)
        use current_precision_mod
        use GF_base_mod
        use grid_mod
        implicit none

        private
        public :: plane_sum_x
        public :: plane_sum_y
        public :: plane_sum_z
        interface plane_sum_x;    module procedure plane_sum_x_GF; end interface
        interface plane_sum_y;    module procedure plane_sum_y_GF; end interface
        interface plane_sum_z;    module procedure plane_sum_z_GF; end interface

        contains

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        function plane_sum_x_GF(U,g,p) result(F)
          implicit none
          type(grid_field),intent(in) :: U
          type(grid),intent(in) :: g
          integer,intent(in) :: p
          real(cp) :: F
          integer :: j,k
#ifdef _PARALLELIZE_GF_PLANE_SUM_
          real(cp) :: temp
          temp = 0.0_cp
          !$OMP PARALLEL DO SHARED(g), REDUCTION(+:temp)
          do k=2,U%s(3)-1; do j=2,U%s(2)-1
            temp = temp + U%f(p,j,k)*g%c(3)%dhn(k)*g%c(2)%dhn(j)
          enddo; enddo
          !$OMP END PARALLEL DO
          F = temp
#else
          F = 0.0_cp
          do k=2,U%s(3)-1; do j=2,U%s(2)-1
            F = F + U%f(p,j,k)*g%c(3)%dhn(k)*g%c(2)%dhn(j)
          enddo; enddo
#endif
        end function

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        function plane_sum_y_GF(U,g,p) result(F)
          implicit none
          type(grid_field),intent(in) :: U
          type(grid),intent(in) :: g
          integer,intent(in) :: p
          real(cp) :: F
          integer :: i,k
#ifdef _PARALLELIZE_GF_PLANE_SUM_
          real(cp) :: temp
          temp = 0.0_cp
          !$OMP PARALLEL DO SHARED(g), REDUCTION(+:temp)
          do k=2,U%s(3)-1; do i=2,U%s(1)-1
            temp = temp + U%f(i,p,k)*g%c(3)%dhn(k)*g%c(1)%dhn(i)
          enddo; enddo
          !$OMP END PARALLEL DO
          F = temp
#else
          F = 0.0_cp
          do k=2,U%s(3)-1; do i=2,U%s(1)-1
            F = F + U%f(i,p,k)*g%c(3)%dhn(k)*g%c(1)%dhn(i)
          enddo; enddo
#endif
        end function

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        function plane_sum_z_GF(U,g,p) result(F)
          implicit none
          type(grid_field),intent(in) :: U
          type(grid),intent(in) :: g
          integer,intent(in) :: p
          real(cp) :: F
          integer :: i,j
#ifdef _PARALLELIZE_GF_PLANE_SUM_
          real(cp) :: temp
          temp = 0.0_cp
          !$OMP PARALLEL DO SHARED(g), REDUCTION(+:temp)
          do j=2,U%s(2)-1; do i=2,U%s(1)-1
            temp = temp + U%f(i,j,p)*g%c(2)%dhn(j)*g%c(1)%dhn(i)
          enddo; enddo
          !$OMP END PARALLEL DO
          F = temp
#else
          F = 0.0_cp
          do j=2,U%s(2)-1; do i=2,U%s(1)-1
            F = F + U%f(i,j,p)*g%c(2)%dhn(j)*g%c(1)%dhn(i)
          enddo; enddo
#endif
        end function

      end module