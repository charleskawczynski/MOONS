      module GF_assign_ghost_mod
        use current_precision_mod
        use GF_base_mod
        use GF_assign_plane_mod
        implicit none

        private
        public :: assign_ghost
        public :: assign_ghost_xmin
        public :: assign_ghost_ymin
        public :: assign_ghost_zmin
        public :: assign_ghost_xmax
        public :: assign_ghost_ymax
        public :: assign_ghost_zmax

        interface assign_ghost;       module procedure assign_ghost_GF;      end interface
        interface assign_ghost_xmin;  module procedure assign_ghost_xmin_GF; end interface
        interface assign_ghost_ymin;  module procedure assign_ghost_ymin_GF; end interface
        interface assign_ghost_zmin;  module procedure assign_ghost_zmin_GF; end interface
        interface assign_ghost_xmax;  module procedure assign_ghost_xmax_GF; end interface
        interface assign_ghost_ymax;  module procedure assign_ghost_ymax_GF; end interface
        interface assign_ghost_zmax;  module procedure assign_ghost_zmax_GF; end interface

        contains

        subroutine assign_ghost_xmin_GF(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_x(U,val,1)
        end subroutine

        subroutine assign_ghost_xmax_GF(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_x(U,val,U%s(1))
        end subroutine

        subroutine assign_ghost_ymin_GF(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_y(U,val,1)
        end subroutine

        subroutine assign_ghost_ymax_GF(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_y(U,val,U%s(2))
        end subroutine

        subroutine assign_ghost_zmin_GF(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_z(U,val,1)
        end subroutine

        subroutine assign_ghost_zmax_GF(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_z(U,val,U%s(3))
        end subroutine

        subroutine assign_ghost_GF(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_ghost_xmin(U,val)
          call assign_ghost_xmax(U,val)
          call assign_ghost_ymin(U,val)
          call assign_ghost_ymax(U,val)
          call assign_ghost_zmin(U,val)
          call assign_ghost_zmax(U,val)
        end subroutine

      end module