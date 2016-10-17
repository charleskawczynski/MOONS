      module GF_assign_wall_mod
        use current_precision_mod
        use GF_base_mod
        use GF_assign_plane_mod
        implicit none

        abstract interface
          subroutine assign_plane_op(GF,val)
            import grid_field,cp
            implicit none
            type(grid_field),intent(inout) :: GF
            real(cp),intent(in) :: val
          end subroutine
        end interface

        private
        public :: assign_plane_op
        public :: assign_wall_xmin
        public :: assign_wall_ymin
        public :: assign_wall_zmin
        public :: assign_wall_xmax
        public :: assign_wall_ymax
        public :: assign_wall_zmax

        contains

        subroutine assign_wall_xmin(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_x(U,val,2)
        end subroutine

        subroutine assign_wall_xmax(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_x(U,val,U%s(1)-1)
        end subroutine

        subroutine assign_wall_ymin(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_y(U,val,2)
        end subroutine

        subroutine assign_wall_ymax(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_y(U,val,U%s(2)-1)
        end subroutine

        subroutine assign_wall_zmin(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_z(U,val,2)
        end subroutine

        subroutine assign_wall_zmax(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call assign_plane_z(U,val,U%s(3)-1)
        end subroutine

      end module