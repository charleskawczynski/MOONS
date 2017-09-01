      module GF_assign_plane_ave_mod
        use grid_field_mod
        use grid_field_extend_mod
        use current_precision_mod
        implicit none

        private
        public :: assign_plane_ave
        interface assign_plane_ave;   module procedure assign_plane_ave_GF; end interface

        public :: assign_plane_ave_x
        public :: assign_plane_ave_y
        public :: assign_plane_ave_z
        interface assign_plane_ave_x;  module procedure assign_plane_ave_x_GF; end interface
        interface assign_plane_ave_y;  module procedure assign_plane_ave_y_GF; end interface
        interface assign_plane_ave_z;  module procedure assign_plane_ave_z_GF; end interface

        contains

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine assign_plane_ave_GF(a,b,p_a,p_b1,p_b2,dir)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b1,p_b2,dir
          select case(dir)
          case (1); call assign_plane_ave_x(a,b,p_a,p_b1,p_b2)
          case (2); call assign_plane_ave_y(a,b,p_a,p_b1,p_b2)
          case (3); call assign_plane_ave_z(a,b,p_a,p_b1,p_b2)
          case default
          write(*,*) 'Error: dir must = 1:3 in assign_plane_GF_GF in GF_assign_plane.f90'
          write(*,*) 'dir = ',dir
          stop 'Done'
          end select
        end subroutine

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine assign_plane_ave_x_GF(a,b,p_a,p_b1,p_b2)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b1,p_b2
          integer :: j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,1,'assign_plane_x_GF_GF')
#endif
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3); do j=1,a%s(2)
          a%f(p_a,j,k) = 0.5_cp*(b%f(p_b1,j,k)+b%f(p_b2,j,k))
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine assign_plane_ave_y_GF(a,b,p_a,p_b1,p_b2)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b1,p_b2
          integer :: i,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,2,'assign_plane_y_GF_GF')
#endif
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3); do i=1,a%s(1)
          a%f(i,p_a,k) = 0.5_cp*(b%f(i,p_b1,k)+b%f(i,p_b2,k))
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine assign_plane_ave_z_GF(a,b,p_a,p_b1,p_b2)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b1,p_b2
          integer :: i,j
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,3,'assign_plane_z_GF_GF')
#endif
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,p_a) = 0.5_cp*(b%f(i,j,p_b1)+b%f(i,j,p_b2))
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine

      end module