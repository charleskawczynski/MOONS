      module GF_assign_ghost_mod
        use current_precision_mod
        use GF_base_mod
        use GF_assign_plane_mod
        implicit none

        abstract interface
          subroutine plane_op(GF,val)
            import grid_field,cp
            implicit none
            type(grid_field),intent(inout) :: GF
            real(cp),intent(in) :: val
          end subroutine
        end interface

        private
        public :: plane_op
        public :: assign_ghost_all
        public :: assign_ghost_xmin
        public :: assign_ghost_ymin
        public :: assign_ghost_zmin
        public :: assign_ghost_xmax
        public :: assign_ghost_ymax
        public :: assign_ghost_zmax

        public :: assign_ghost_xmin_xmax
        public :: assign_ghost_ymin_ymax
        public :: assign_ghost_zmin_zmax

        interface assign_ghost_xmin_xmax; module procedure assign_ghost_xmin_xmax_GF; end interface
        interface assign_ghost_ymin_ymax; module procedure assign_ghost_ymin_ymax_GF; end interface
        interface assign_ghost_zmin_zmax; module procedure assign_ghost_zmin_zmax_GF; end interface

        contains

        subroutine assign_ghost_all(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_x(U,val,1)
          call assign_plane_x(U,val,U%s(1))
          call assign_plane_y(U,val,1)
          call assign_plane_y(U,val,U%s(2))
          call assign_plane_z(U,val,1)
          call assign_plane_z(U,val,U%s(3))
        end subroutine

        subroutine assign_ghost_xmin(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_x(U,val,1)
        end subroutine
        subroutine assign_ghost_xmax(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_x(U,val,U%s(1))
        end subroutine
        subroutine assign_ghost_xmin_xmax_GF(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_2_planes_x(U,val,1,U%s(1))
        end subroutine

        subroutine assign_ghost_ymin(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_y(U,val,1)
        end subroutine
        subroutine assign_ghost_ymax(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_y(U,val,U%s(2))
        end subroutine
        subroutine assign_ghost_ymin_ymax_GF(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_2_planes_y(U,val,1,U%s(2))
        end subroutine

        subroutine assign_ghost_zmin(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_z(U,val,1)
        end subroutine
        subroutine assign_ghost_zmax(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_z(U,val,U%s(3))
        end subroutine
        subroutine assign_ghost_zmin_zmax_GF(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_2_planes_z(U,val,1,U%s(3))
        end subroutine

      end module