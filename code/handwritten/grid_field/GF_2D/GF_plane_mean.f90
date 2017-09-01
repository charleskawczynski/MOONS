      module GF_plane_mean_mod
        use current_precision_mod
        use grid_field_mod
        use GF_plane_sum_mod
        use grid_mod
        implicit none

        private
        public :: plane_mean_x
        public :: plane_mean_y
        public :: plane_mean_z
        interface plane_mean_x;    module procedure plane_mean_x_GF;            end interface
        interface plane_mean_y;    module procedure plane_mean_y_GF;            end interface
        interface plane_mean_z;    module procedure plane_mean_z_GF;            end interface
        interface plane_mean_x;    module procedure plane_mean_x_no_weights_GF; end interface
        interface plane_mean_y;    module procedure plane_mean_y_no_weights_GF; end interface
        interface plane_mean_z;    module procedure plane_mean_z_no_weights_GF; end interface

        contains

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        function plane_mean_x_GF(U,g,p) result(F)
          implicit none
          type(grid_field),intent(in) :: U
          type(grid),intent(in) :: g
          integer,intent(in) :: p
          real(cp) :: F
          F = plane_sum_x(U,g,p,1.0_cp)/g%c(1)%maxRange
        end function

        function plane_mean_x_no_weights_GF(U,p) result(F)
          implicit none
          type(grid_field),intent(in) :: U
          integer,intent(in) :: p
          real(cp) :: F
          F = plane_sum_x(U,p,1.0_cp)/real(U%s(1),cp)
        end function

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        function plane_mean_y_GF(U,g,p) result(F)
          implicit none
          type(grid_field),intent(in) :: U
          type(grid),intent(in) :: g
          integer,intent(in) :: p
          real(cp) :: F
          F = plane_sum_y(U,g,p,1.0_cp)/g%c(2)%maxRange
        end function

        function plane_mean_y_no_weights_GF(U,p) result(F)
          implicit none
          type(grid_field),intent(in) :: U
          integer,intent(in) :: p
          real(cp) :: F
          F = plane_sum_y(U,p,1.0_cp)/real(U%s(2),cp)
        end function

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        function plane_mean_z_GF(U,g,p) result(F)
          implicit none
          type(grid_field),intent(in) :: U
          type(grid),intent(in) :: g
          integer,intent(in) :: p
          real(cp) :: F
          F = plane_sum_z(U,g,p,1.0_cp)/g%c(3)%maxRange
        end function

        function plane_mean_z_no_weights_GF(U,p) result(F)
          implicit none
          type(grid_field),intent(in) :: U
          integer,intent(in) :: p
          real(cp) :: F
          F = plane_sum_z(U,p,1.0_cp)/real(U%s(3),cp)
        end function

      end module