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
        public :: assign_ghost_xmin
        public :: assign_ghost_ymin
        public :: assign_ghost_zmin
        public :: assign_ghost_xmax
        public :: assign_ghost_ymax
        public :: assign_ghost_zmax

        contains

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

      end module