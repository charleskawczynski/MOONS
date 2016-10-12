      module GF_assign_plane_mod
        use GF_base_mod
        use current_precision_mod
        implicit none
        private

        public :: assign_plane_x
        public :: assign_plane_y
        public :: assign_plane_z
        interface assign_plane_x;    module procedure assign_plane_x_GF_S;   end interface
        interface assign_plane_x;    module procedure assign_plane_x_GF_GF;  end interface
        interface assign_plane_y;    module procedure assign_plane_y_GF_S;   end interface
        interface assign_plane_y;    module procedure assign_plane_y_GF_GF;  end interface
        interface assign_plane_z;    module procedure assign_plane_z_GF_S;   end interface
        interface assign_plane_z;    module procedure assign_plane_z_GF_GF;  end interface

        contains

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine assign_plane_x_GF_S(a,b,p)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p
#ifdef _PARALLELIZE_GF_SURFACE_
          integer :: j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2)
          a%f(p,j,k) = b
          enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f(p,:,:) = b
#endif
        end subroutine
        subroutine assign_plane_x_GF_GF(a,b,p_a,p_b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b
#ifdef _PARALLELIZE_GF_SURFACE_
          integer :: j,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do j=1,a%s(2)
          a%f(p_a,j,k) = b%f(p_b,j,k)
          enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f(p_a,:,:) = b%f(p_b,:,:)
#endif
        end subroutine

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine assign_plane_y_GF_S(a,b,p)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p
#ifdef _PARALLELIZE_GF_SURFACE_
          integer :: i,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do i=1,a%s(1)
          a%f(i,p,k) = b
          enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f(:,p,:) = b
#endif
        end subroutine
        subroutine assign_plane_y_GF_GF(a,b,p_a,p_b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b
#ifdef _PARALLELIZE_GF_SURFACE_
          integer :: i,k
          !$OMP PARALLEL DO
          do k=1,a%s(3); do i=1,a%s(1)
          a%f(i,p_a,k) = b%f(i,p_b,k)
          enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f(:,p_a,:) = b%f(:,p_b,:)
#endif
        end subroutine

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine assign_plane_z_GF_S(a,b,p)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p
#ifdef _PARALLELIZE_GF_SURFACE_
          integer :: i,j
          !$OMP PARALLEL DO
          do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,p) = b
          enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f(:,:,p) = b
#endif
        end subroutine
        subroutine assign_plane_z_GF_GF(a,b,p_a,p_b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b
#ifdef _PARALLELIZE_GF_SURFACE_
          integer :: i,j
          !$OMP PARALLEL DO
          do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,p_a) = b%f(i,j,p_b)
          enddo; enddo
          !$OMP END PARALLEL DO
#else
          a%f(:,:,p_a) = b%f(:,:,p_b)
#endif
        end subroutine

      end module