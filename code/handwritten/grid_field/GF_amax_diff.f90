      module GF_amax_diff_mod
        use grid_field_mod
        use grid_field_extend_mod
        use grid_mod
        use GF_assign_plane_mod
        use GF_plane_mean_mod
        use current_precision_mod
        implicit none
        private

        public :: amax_diff
        interface amax_diff;      module procedure amax_diff_GF;       end interface
        interface amax_diff;      module procedure amax_diff_dir_GF;   end interface

        contains

        function amax_diff_GF(u) result(amax_diff)
          implicit none
          type(grid_field),intent(in) :: u
          real(cp),dimension(3) :: amax_diff
          amax_diff(1) = amax_diff_x_GF(u)
          amax_diff(2) = amax_diff_y_GF(u)
          amax_diff(3) = amax_diff_z_GF(u)
        end function

        function amax_diff_dir_GF(u,dir) result(amax_diff)
          implicit none
          type(grid_field),intent(in) :: u
          integer,intent(in) :: dir
          real(cp) :: amax_diff
          select case(dir)
          case (1); amax_diff = amax_diff_x_GF(u)
          case (2); amax_diff = amax_diff_y_GF(u)
          case (3); amax_diff = amax_diff_z_GF(u)
          case default
          stop 'Error: bad dir in amax_diff_dir_GF in GF_amax_diff.f90'
          end select
        end function

        function amax_diff_func(u,n) result(amax_diff)
          implicit none
          integer,intent(in) :: n
          real(cp),dimension(n),intent(in) :: u
          real(cp) :: amax_diff
          amax_diff = maxval(abs(u - u(1)))
        end function

        function amax_diff_x_GF(u) result(amax_diff)
          implicit none
          type(grid_field),intent(in) :: u
          integer :: j,k
          real(cp) :: amax_diff
          amax_diff = 0.0_cp
          do k=1,u%s(3)
          do j=1,u%s(2)
            amax_diff = maxval((/amax_diff,amax_diff_func(u%f(1:u%s(1),j,k),u%s(1))/))
          enddo
          enddo
        end function

        function amax_diff_y_GF(u) result(amax_diff)
          implicit none
          type(grid_field),intent(in) :: u
          integer :: i,k
          real(cp) :: amax_diff
          amax_diff = 0.0_cp
          do k=1,u%s(3)
          do i=1,u%s(1)
            amax_diff = maxval((/amax_diff,amax_diff_func(u%f(i,1:u%s(2),k),u%s(2))/))
          enddo
          enddo
        end function

        function amax_diff_z_GF(u) result(amax_diff)
          implicit none
          type(grid_field),intent(in) :: u
          integer :: i,j
          real(cp) :: amax_diff
          amax_diff = 0.0_cp
          do j=1,u%s(2)
          do i=1,u%s(1)
            amax_diff = maxval((/amax_diff,amax_diff_func(u%f(i,j,1:u%s(3)),u%s(3))/))
          enddo
          enddo
        end function

      end module