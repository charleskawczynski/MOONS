      module GF_mirror_about_plane_mod
        use current_precision_mod
        use grid_field_mod
        use grid_field_extend_mod
        use GF_add_mod
        use GF_aux_mod
        use GF_multiply_mod
        use GF_assign_mod
        use GF_assign_plane_mod
        use GF_multiply_plane_mod
        use grid_mod

        implicit none
        private

        public :: mirror_about_hmin,mirror_about_hmax
        interface mirror_about_hmin;  module procedure mirror_about_hmin_GF; end interface
        interface mirror_about_hmax;  module procedure mirror_about_hmax_GF; end interface

        interface mirror_about_xmin;  module procedure mirror_about_xmin_GF; end interface
        interface mirror_about_ymin;  module procedure mirror_about_ymin_GF; end interface
        interface mirror_about_zmin;  module procedure mirror_about_zmin_GF; end interface

        interface mirror_about_xmax;  module procedure mirror_about_xmax_GF; end interface
        interface mirror_about_ymax;  module procedure mirror_about_ymax_GF; end interface
        interface mirror_about_zmax;  module procedure mirror_about_zmax_GF; end interface

        contains

        subroutine mirror_about_hmin_GF(U,dir,mirror_sign,N_along_dir)
          implicit none
          type(grid_field),intent(inout) :: U
          integer,intent(in) :: dir,N_along_dir
          real(cp),intent(in) :: mirror_sign
          select case (dir)
          case (1); call mirror_about_xmin(U,mirror_sign,N_along_dir)
          case (2); call mirror_about_ymin(U,mirror_sign,N_along_dir)
          case (3); call mirror_about_zmin(U,mirror_sign,N_along_dir)
          case default
          write(*,*) 'Error: bad dir in mirror_about_hmin_GF in GF_mirror_about_plane.f90'
          stop 'Done'
          end select
        end subroutine

        subroutine mirror_about_hmax_GF(U,dir,mirror_sign,N_along_dir)
          implicit none
          type(grid_field),intent(inout) :: U
          integer,intent(in) :: dir,N_along_dir
          real(cp),intent(in) :: mirror_sign
          select case (dir)
          case (1); call mirror_about_xmax(U,mirror_sign,N_along_dir)
          case (2); call mirror_about_ymax(U,mirror_sign,N_along_dir)
          case (3); call mirror_about_zmax(U,mirror_sign,N_along_dir)
          case default
          write(*,*) 'Error: bad dir in mirror_about_hmin_GF in GF_mirror_about_plane.f90'
          stop 'Done'
          end select
        end subroutine

        ! *************************************************************
        ! *************************************************************
        ! *************************************************************

        subroutine mirror_about_xmin_GF(U,mirror_sign,N_along_dir)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: mirror_sign
          integer,intent(in) :: N_along_dir
          type(grid_field) :: temp
          integer :: i,i_reverse,i_temp
          call init(temp,(/2*U%s(1)-2-N_along_dir,U%s(2),U%s(3)/))
          do i=1,U%s(1)
            i_reverse = U%s(1)-i+1
            i_temp = temp%s(1)-i+1
            call assign_plane_x(temp,U,i_temp,i_reverse)
          enddo
          do i=1+N_along_dir,U%s(1)
            i_reverse = U%s(1)-i+1
            call assign_plane_x(temp,U,i,i_reverse)
            call multiply_plane_x(temp,mirror_sign,i)
          enddo
          call init(U,temp)
          call assign(U,temp)
        end subroutine

        subroutine mirror_about_ymin_GF(U,mirror_sign,N_along_dir)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: mirror_sign
          integer,intent(in) :: N_along_dir
          type(grid_field) :: temp
          integer :: i,i_reverse,i_temp
          call init(temp,(/U%s(1),2*U%s(2)-2-N_along_dir,U%s(3)/))
          do i=1,U%s(2)
            i_reverse = U%s(2)-i+1
            i_temp = temp%s(2)-i+1
            call assign_plane_y(temp,U,i_temp,i_reverse)
          enddo
          do i=1+N_along_dir,U%s(2)
            i_reverse = U%s(2)-i+1
            call assign_plane_y(temp,U,i,i_reverse)
            call multiply_plane_y(temp,mirror_sign,i)
          enddo
          call init(U,temp)
          call assign(U,temp)
        end subroutine

        subroutine mirror_about_zmin_GF(U,mirror_sign,N_along_dir)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: mirror_sign
          integer,intent(in) :: N_along_dir
          type(grid_field) :: temp
          integer :: i,i_reverse,i_temp
          call init(temp,(/U%s(1),U%s(2),2*U%s(3)-2-N_along_dir/))
          do i=1,U%s(3)
            i_reverse = U%s(3)-i+1
            i_temp = temp%s(3)-i+1
            call assign_plane_z(temp,U,i_temp,i_reverse)
          enddo
          do i=1+N_along_dir,U%s(3)
            i_reverse = U%s(3)-i+1
            call assign_plane_z(temp,U,i,i_reverse)
            call multiply_plane_z(temp,mirror_sign,i)
          enddo
          call init(U,temp)
          call assign(U,temp)
        end subroutine

        subroutine mirror_about_xmax_GF(U,mirror_sign,N_along_dir)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: mirror_sign
          integer,intent(in) :: N_along_dir
          type(grid_field) :: temp
          integer :: i,i_reverse,i_temp
          call init(temp,(/2*U%s(1)-2-N_along_dir,U%s(2),U%s(3)/))
          do i=1,U%s(1)
            call assign_plane_x(temp,U,i,i)
          enddo
          do i=1,U%s(1)-N_along_dir
            i_reverse = U%s(1)-i+1
            i_temp = temp%s(1)-i+1
            call assign_plane_x(temp,U,i_temp,i)
            call multiply_plane_x(temp,mirror_sign,i_temp)
          enddo
          call init(U,temp)
          call assign(U,temp)
        end subroutine

        subroutine mirror_about_ymax_GF(U,mirror_sign,N_along_dir)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: mirror_sign
          integer,intent(in) :: N_along_dir
          type(grid_field) :: temp
          integer :: i,i_reverse,i_temp
          call init(temp,(/U%s(1),2*U%s(2)-2-N_along_dir,U%s(3)/))
          do i=1,U%s(2)
            call assign_plane_y(temp,U,i,i)
          enddo
          do i=1,U%s(2)-N_along_dir
            i_reverse = U%s(2)-i+1
            i_temp = temp%s(2)-i+1
            call assign_plane_y(temp,U,i_temp,i)
            call multiply_plane_y(temp,mirror_sign,i_temp)
          enddo
          call init(U,temp)
          call assign(U,temp)
        end subroutine

        subroutine mirror_about_zmax_GF(U,mirror_sign,N_along_dir)
          implicit none
          type(grid_field),intent(inout) :: U
          real(cp),intent(in) :: mirror_sign
          integer,intent(in) :: N_along_dir
          type(grid_field) :: temp
          integer :: i,i_reverse,i_temp
          call init(temp,(/U%s(1),U%s(2),2*U%s(3)-2-N_along_dir/))
          do i=1,U%s(3)
            call assign_plane_z(temp,U,i,i)
          enddo
          do i=1,U%s(3)-N_along_dir
            i_reverse = U%s(3)-i+1
            i_temp = temp%s(3)-i+1
            call assign_plane_z(temp,U,i_temp,i)
            call multiply_plane_z(temp,mirror_sign,i_temp)
          enddo
          call init(U,temp)
          call assign(U,temp)
        end subroutine

      end module