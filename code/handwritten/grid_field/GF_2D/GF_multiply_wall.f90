      module GF_multiply_wall_mod
        use current_precision_mod
        use grid_field_mod
        use GF_multiply_plane_mod
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
        public :: multiply_wall_xmin
        public :: multiply_wall_ymin
        public :: multiply_wall_zmin
        public :: multiply_wall_xmax
        public :: multiply_wall_ymax
        public :: multiply_wall_zmax

        contains

        subroutine multiply_wall_xmin(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call multiply_plane_x(U,val,2)
        end subroutine

        subroutine multiply_wall_xmax(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call multiply_plane_x(U,val,U%s(1)-1)
        end subroutine

        subroutine multiply_wall_ymin(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call multiply_plane_y(U,val,2)
        end subroutine

        subroutine multiply_wall_ymax(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call multiply_plane_y(U,val,U%s(2)-1)
        end subroutine

        subroutine multiply_wall_zmin(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call multiply_plane_z(U,val,2)
        end subroutine

        subroutine multiply_wall_zmax(U,val)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: val
          call multiply_plane_z(U,val,U%s(3)-1)
        end subroutine

      end module