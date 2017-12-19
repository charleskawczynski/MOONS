      module GF_assign_ghost_periodic_mod
        use current_precision_mod
        use grid_field_mod
        use GF_assign_plane_mod
        implicit none

        abstract interface
          subroutine plane_op(GF,val)
            import grid_field
            implicit none
            type(grid_field),intent(inout) :: GF
            type(grid_field),intent(in) :: val
          end subroutine
        end interface

        private
        public :: plane_op
        public :: assign_ghost_periodic_all
        public :: assign_ghost_periodic_xmin
        public :: assign_ghost_periodic_ymin
        public :: assign_ghost_periodic_zmin
        public :: assign_ghost_periodic_xmax
        public :: assign_ghost_periodic_ymax
        public :: assign_ghost_periodic_zmax

        contains

        subroutine assign_ghost_periodic_all(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid_field),intent(in) :: val
          call assign_plane_x(U,val,1,1)
          call assign_plane_x(U,val,U%s(1),val%s(1))
          call assign_plane_y(U,val,1,1)
          call assign_plane_y(U,val,U%s(2),val%s(2))
          call assign_plane_z(U,val,1,1)
          call assign_plane_z(U,val,U%s(3),val%s(3))
        end subroutine

        subroutine assign_ghost_periodic_xmin(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid_field),intent(in) :: val
          call assign_plane_x(U,val,1,1)
        end subroutine
        subroutine assign_ghost_periodic_xmax(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid_field),intent(in) :: val
          call assign_plane_x(U,val,U%s(1),val%s(1))
        end subroutine

        subroutine assign_ghost_periodic_ymin(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid_field),intent(in) :: val
          call assign_plane_y(U,val,1,1)
        end subroutine
        subroutine assign_ghost_periodic_ymax(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid_field),intent(in) :: val
          call assign_plane_y(U,val,U%s(2),val%s(2))
        end subroutine

        subroutine assign_ghost_periodic_zmin(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid_field),intent(in) :: val
          call assign_plane_z(U,val,1,1)
        end subroutine
        subroutine assign_ghost_periodic_zmax(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid_field),intent(in) :: val
          call assign_plane_z(U,val,U%s(3),val%s(3))
        end subroutine

      end module