      module GF_symmetry_error_mod
        use current_precision_mod
        use even_odd_mod
        use grid_field_mod
        use GF_add_mod
        use GF_aux_mod
        use GF_multiply_mod
        use GF_assign_mod
        use GF_assign_plane_mod
        use GF_plane_sum_mod
        use grid_mod
        implicit none

        private
        public :: symmetry_error_x
        public :: symmetry_error_y
        public :: symmetry_error_z
        public :: symmetry_local_x
        public :: symmetry_local_y
        public :: symmetry_local_z

        interface symmetry_error_x;  module procedure symmetry_error_x_GF; end interface
        interface symmetry_error_y;  module procedure symmetry_error_y_GF; end interface
        interface symmetry_error_z;  module procedure symmetry_error_z_GF; end interface

        interface symmetry_local_x;  module procedure symmetry_local_x_GF; end interface
        interface symmetry_local_y;  module procedure symmetry_local_y_GF; end interface
        interface symmetry_local_z;  module procedure symmetry_local_z_GF; end interface

        contains

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        function symmetry_error_x_GF(U) result(F)
          implicit none
          type(grid_field),intent(in) :: U
          type(grid_field) :: temp
          real(cp) :: F
          call init(temp,U)
          call assign(temp,U)
          call symmetry_local_x(temp)
          F = amax(temp)
          call delete(temp)
        end function

        function symmetry_error_y_GF(U) result(F)
          implicit none
          type(grid_field),intent(in) :: U
          type(grid_field) :: temp
          real(cp) :: F
          call init(temp,U)
          call assign(temp,U)
          call symmetry_local_y(temp)
          F = amax(temp)
          call delete(temp)
        end function

        function symmetry_error_z_GF(U) result(F)
          implicit none
          type(grid_field),intent(in) :: U
          type(grid_field) :: temp
          real(cp) :: F
          call init(temp,U)
          call assign(temp,U)
          call symmetry_local_z(temp)
          F = amax(temp)
          call delete(temp)
        end function

        ! ********************************************************************
        ! ********************************************************************
        ! ********************************************************************

        subroutine symmetry_local_x_GF(U)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid_field) :: temp
          integer :: i
          call init(temp,U)
          do i=1,U%s(1); call assign_plane_x(temp,U,i,U%s(1)-i+1); enddo
          call multiply(temp,-1.0_cp)
          call add(U,temp)
          call delete(temp)
        end subroutine

        subroutine symmetry_local_y_GF(U)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid_field) :: temp
          integer :: i
          call init(temp,U)
          do i=1,U%s(2); call assign_plane_y(temp,U,i,U%s(2)-i+1); enddo
          call multiply(temp,-1.0_cp)
          call add(U,temp)
          call delete(temp)
        end subroutine

        subroutine symmetry_local_z_GF(U)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid_field) :: temp
          integer :: i
          call init(temp,U)
          do i=1,U%s(3); call assign_plane_z(temp,U,i,U%s(3)-i+1); enddo
          call multiply(temp,-1.0_cp)
          call add(U,temp)
          call delete(temp)
        end subroutine

      end module