      module GF_mirror_about_plane_mod
        use current_precision_mod
        use GF_base_mod
        use GF_add_mod
        use GF_aux_mod
        use GF_multiply_mod
        use GF_assign_mod
        use GF_assign_plane_mod
        use grid_mod
        implicit none

        private
        public :: mirror_about_xmin,mirror_about_xmax
        public :: mirror_about_ymin,mirror_about_ymax
        public :: mirror_about_zmin,mirror_about_zmax

        interface mirror_about_xmin;  module procedure mirror_about_xmin_GF; end interface
        interface mirror_about_ymin;  module procedure mirror_about_ymin_GF; end interface
        interface mirror_about_zmin;  module procedure mirror_about_zmin_GF; end interface

        interface mirror_about_xmax;  module procedure mirror_about_xmax_GF; end interface
        interface mirror_about_ymax;  module procedure mirror_about_ymax_GF; end interface
        interface mirror_about_zmax;  module procedure mirror_about_zmax_GF; end interface

        contains

        subroutine mirror_about_xmin_GF(U,mirror_sign)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: mirror_sign
          type(grid_field) :: temp
          integer :: i
          call init(temp,U)
          call assign(temp,U)
          do i=1,U%s(1)/2
            call assign_plane_x(U,temp,i,U%s(1)-i+1)
            call multiply_plane_x(U,i,mirror_sign)
          enddo
          call delete(temp)
        end subroutine

        subroutine mirror_about_ymin_GF(U,mirror_sign)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: mirror_sign
          type(grid_field) :: temp
          integer :: i
          call init(temp,U)
          call assign(temp,U)
          do i=1,U%s(2)/2
            call assign_plane_y(U,temp,i,U%s(2)-i+1)
            call multiply_plane_y(U,i,mirror_sign)
          enddo
          call delete(temp)
        end subroutine

        subroutine mirror_about_zmin_GF(U,mirror_sign)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: mirror_sign
          type(grid_field) :: temp
          integer :: i
          call init(temp,U)
          call assign(temp,U)
          do i=1,U%s(3)/2
            call assign_plane_z(U,temp,i,U%s(3)-i+1)
            call multiply_plane_z(U,i,mirror_sign)
          enddo
          call delete(temp)
        end subroutine

        subroutine mirror_about_xmax_GF(U,mirror_sign)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: mirror_sign
          type(grid_field) :: temp
          integer :: i
          call init(temp,U)
          call assign(temp,U)
          do i=U%s(1),U%s(1)/2,-1
            call assign_plane_x(U,temp,i,i-U%s(1)+1)
            call multiply_plane_x(U,i,mirror_sign)
          enddo
          call delete(temp)
        end subroutine

        subroutine mirror_about_ymax_GF(U,mirror_sign)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: mirror_sign
          type(grid_field) :: temp
          integer :: i
          call init(temp,U)
          call assign(temp,U)
          do i=U%s(2),U%s(2)/2,-1
            call assign_plane_y(U,temp,i,i-U%s(2)+1)
            call multiply_plane_y(U,i,mirror_sign)
          enddo
          call delete(temp)
        end subroutine

        subroutine mirror_about_zmax_GF(U,mirror_sign)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: mirror_sign
          type(grid_field) :: temp
          integer :: i
          call init(temp,U)
          call assign(temp,U)
          do i=U%s(3),U%s(3)/2,-1
            call assign_plane_z(U,temp,i,i-U%s(3)+1)
            call multiply_plane_z(U,i,mirror_sign)
          enddo
          call delete(temp)
        end subroutine

      end module