      module GF_add_plane_mod
        use GF_base_mod
        use current_precision_mod
        implicit none

        private
        public :: add_plane
        interface add_plane;      module procedure add_plane_GF_S;     end interface
        interface add_plane;      module procedure add_plane_GF_GF;    end interface

        public :: add_plane_x
        public :: add_plane_y
        public :: add_plane_z
        interface add_plane_x;    module procedure add_plane_x_GF_S;   end interface
        interface add_plane_x;    module procedure add_plane_x_GF_GF;  end interface
        interface add_plane_y;    module procedure add_plane_y_GF_S;   end interface
        interface add_plane_y;    module procedure add_plane_y_GF_GF;  end interface
        interface add_plane_z;    module procedure add_plane_z_GF_S;   end interface
        interface add_plane_z;    module procedure add_plane_z_GF_GF;  end interface

        public :: add_2_planes_x
        public :: add_2_planes_y
        public :: add_2_planes_z
        interface add_2_planes_x; module procedure add_2_planes_x_S;   end interface
        interface add_2_planes_y; module procedure add_2_planes_y_S;   end interface
        interface add_2_planes_z; module procedure add_2_planes_z_S;   end interface

        contains

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine add_plane_GF_S(a,b,p,dir)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p,dir
          select case(dir)
          case (1); call add_plane_x(a,b,p)
          case (2); call add_plane_y(a,b,p)
          case (3); call add_plane_z(a,b,p)
          case default
          write(*,*) 'Error: dir must = 1:3 in add_plane_GF_S in GF_add_plane.f90'
          write(*,*) 'dir = ',dir
          stop 'Done'
          end select
        end subroutine

        subroutine add_plane_GF_GF(a,b,p_a,p_b,dir)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b,dir
          select case(dir)
          case (1); call add_plane_x(a,b,p_a,p_b)
          case (2); call add_plane_y(a,b,p_a,p_b)
          case (3); call add_plane_z(a,b,p_a,p_b)
          case default
          write(*,*) 'Error: dir must = 1:3 in add_plane_GF_GF in GF_add_plane.f90'
          write(*,*) 'dir = ',dir
          stop 'Done'
          end select
        end subroutine

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine add_plane_x_GF_S(a,b,p)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p
          integer :: j,k
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3); do j=1,a%s(2)
          a%f(p,j,k) = a%f(p,j,k) + b
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine
        subroutine add_plane_x_GF_GF(a,b,p_a,p_b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b
          integer :: j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,1,'add_plane_x_GF_GF')
#endif
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3); do j=1,a%s(2)
          a%f(p_a,j,k) = a%f(p_a,j,k) + b%f(p_b,j,k)
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine
        subroutine add_2_planes_x_S(a,b,p_1,p_2)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p_1,p_2
          integer :: j,k
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3); do j=1,a%s(2)
          a%f(p_1,j,k) = a%f(p_1,j,k) + b
          a%f(p_2,j,k) = a%f(p_2,j,k) + b
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine add_plane_y_GF_S(a,b,p)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p
          integer :: i,k
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3); do i=1,a%s(1)
          a%f(i,p,k) = a%f(i,p,k) + b
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine
        subroutine add_plane_y_GF_GF(a,b,p_a,p_b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b
          integer :: i,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,2,'add_plane_y_GF_GF')
#endif
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3); do i=1,a%s(1)
          a%f(i,p_a,k) = a%f(i,p_a,k) + b%f(i,p_b,k)
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine
        subroutine add_2_planes_y_S(a,b,p_1,p_2)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p_1,p_2
          integer :: i,k
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3); do i=1,a%s(1)
          a%f(i,p_1,k) = a%f(i,p_1,k) + b
          a%f(i,p_2,k) = a%f(i,p_2,k) + b
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine add_plane_z_GF_S(a,b,p)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p
          integer :: i,j
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,p) = a%f(i,j,p) + b
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine
        subroutine add_plane_z_GF_GF(a,b,p_a,p_b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b
          integer :: i,j
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,3,'add_plane_z_GF_GF')
#endif
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,p_a) = a%f(i,j,p_a) + b%f(i,j,p_b)
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine
        subroutine add_2_planes_z_S(a,b,p_1,p_2)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p_1,p_2
          integer :: i,j
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,p_1) = a%f(i,j,p_1) + b
          a%f(i,j,p_2) = a%f(i,j,p_2) + b
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine

      end module