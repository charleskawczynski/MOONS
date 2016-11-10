      module GF_mean_along_dir_mod
        use GF_base_mod
        use grid_mod
        use GF_assign_plane_mod
        use GF_plane_mean_mod
        use current_precision_mod
        implicit none
        private

        public :: mean_along_x
        public :: mean_along_y
        public :: mean_along_z

        interface mean_along_x;    module procedure mean_along_x_GF;     end interface
        interface mean_along_y;    module procedure mean_along_y_GF;     end interface
        interface mean_along_z;    module procedure mean_along_z_GF;     end interface

        contains

        subroutine mean_along_x_GF(u_mean,u,g)
          implicit none
          type(grid_field),intent(inout) :: u_mean
          type(grid_field),intent(in) :: u
          type(grid),intent(in) :: g
          integer :: i
          real(cp) :: temp
#ifdef _DEBUG_GF_
          call insist_shape_match(u,u_mean,'mean_along_x_GF')
#endif
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g) PRIVATE(temp)

#endif
          do i=1,u%s(1)
            temp = plane_mean_x(u,g,i)
            call assign_plane_x(u_mean,temp,i)
          enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine mean_along_y_GF(u_mean,u,g)
          implicit none
          type(grid_field),intent(inout) :: u_mean
          type(grid_field),intent(in) :: u
          type(grid),intent(in) :: g
          integer :: j
          real(cp) :: temp
#ifdef _DEBUG_GF_
          call insist_shape_match(u,u_mean,'mean_along_y_GF')
#endif
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g) PRIVATE(temp)

#endif
          do j=1,u%s(2)
            temp = plane_mean_y(u,g,j)
            call assign_plane_y(u_mean,temp,j)
          enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine mean_along_z_GF(u_mean,u,g)
          implicit none
          type(grid_field),intent(inout) :: u_mean
          type(grid_field),intent(in) :: u
          type(grid),intent(in) :: g
          integer :: k
          real(cp) :: temp
#ifdef _DEBUG_GF_
          call insist_shape_match(u,u_mean,'mean_along_z_GF')
#endif
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g) PRIVATE(temp)

#endif
          do k=1,u%s(3)
            temp = plane_mean_z(u,g,k)
            call assign_plane_z(u_mean,temp,k)
          enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

      end module