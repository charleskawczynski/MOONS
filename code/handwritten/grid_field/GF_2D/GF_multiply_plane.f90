      module GF_multiply_plane_mod
        use grid_field_mod
        use grid_field_extend_mod
        use current_precision_mod
        implicit none

        private
        public :: multiply_plane
        interface multiply_plane;      module procedure multiply_plane_GF_S;            end interface
        interface multiply_plane;      module procedure multiply_plane_GF_GF;           end interface
        interface multiply_plane;      module procedure multiply_plane_GF_GF_shift;     end interface

        public :: multiply_plane_x
        public :: multiply_plane_y
        public :: multiply_plane_z
        interface multiply_plane_x;    module procedure multiply_plane_x_GF_S;          end interface
        interface multiply_plane_x;    module procedure multiply_plane_x_GF_GF;         end interface
        interface multiply_plane_x;    module procedure multiply_plane_x_GF_GF_shift;   end interface
        interface multiply_plane_y;    module procedure multiply_plane_y_GF_S;          end interface
        interface multiply_plane_y;    module procedure multiply_plane_y_GF_GF;         end interface
        interface multiply_plane_y;    module procedure multiply_plane_y_GF_GF_shift;   end interface
        interface multiply_plane_z;    module procedure multiply_plane_z_GF_S;          end interface
        interface multiply_plane_z;    module procedure multiply_plane_z_GF_GF;         end interface
        interface multiply_plane_z;    module procedure multiply_plane_z_GF_GF_shift;   end interface

        contains

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine multiply_plane_GF_S(a,b,p,dir)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p,dir
          select case(dir)
          case (1); call multiply_plane_x(a,b,p)
          case (2); call multiply_plane_y(a,b,p)
          case (3); call multiply_plane_z(a,b,p)
          case default
          write(*,*) 'Error: dir must = 1:3 in multiply_plane_GF_S in GF_multiply_plane.f90'
          write(*,*) 'dir = ',dir
          stop 'Done'
          end select
        end subroutine

        subroutine multiply_plane_GF_GF(a,b,p_a,p_b,dir)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b,dir
          select case(dir)
          case (1); call multiply_plane_x(a,b,p_a,p_b)
          case (2); call multiply_plane_y(a,b,p_a,p_b)
          case (3); call multiply_plane_z(a,b,p_a,p_b)
          case default
          write(*,*) 'Error: dir must = 1:3 in multiply_plane_GF_GF in GF_multiply_plane.f90'
          write(*,*) 'dir = ',dir
          stop 'Done'
          end select
        end subroutine

        subroutine multiply_plane_GF_GF_shift(a,b,p_a,p_b,dir,s)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b,dir
          integer,dimension(3),intent(in) :: s
          select case(dir)
          case (1); call multiply_plane_x(a,b,p_a,p_b,s)
          case (2); call multiply_plane_y(a,b,p_a,p_b,s)
          case (3); call multiply_plane_z(a,b,p_a,p_b,s)
          case default
          write(*,*) 'Error: dir must = 1:3 in multiply_plane_GF_GF_shift in GF_multiply_plane.f90'
          write(*,*) 'dir = ',dir
          stop 'Done'
          end select
        end subroutine


        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine multiply_plane_x_GF_S(a,b,p)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p
          integer :: j,k
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3); do j=1,a%s(2)
          a%f(p,j,k) = a%f(p,j,k)*b
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine
        subroutine multiply_plane_x_GF_GF(a,b,p_a,p_b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b
          integer :: j,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,1,'multiply_plane_x_GF_GF')
#endif
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3); do j=1,a%s(2)
          a%f(p_a,j,k) = a%f(p_a,j,k)*b%f(p_b,j,k)
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine
        subroutine multiply_plane_x_GF_GF_shift(a,b,p_a,p_b,s)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b
          integer,dimension(3),intent(in) :: s
          integer :: j,k
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3)-s(3); do j=1,a%s(2)-s(2)
          a%f(p_a,j,k) = a%f(p_a,j,k)*b%f(p_b,j+s(2),k+s(3))
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine multiply_plane_y_GF_S(a,b,p)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p
          integer :: i,k
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3); do i=1,a%s(1)
          a%f(i,p,k) = a%f(i,p,k)*b
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine
        subroutine multiply_plane_y_GF_GF(a,b,p_a,p_b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b
          integer :: i,k
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,2,'multiply_plane_y_GF_GF')
#endif
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3); do i=1,a%s(1)
          a%f(i,p_a,k) = a%f(i,p_a,k)*b%f(i,p_b,k)
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine
        subroutine multiply_plane_y_GF_GF_shift(a,b,p_a,p_b,s)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b
          integer,dimension(3),intent(in) :: s
          integer :: i,k
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do k=1,a%s(3)-s(3); do i=1,a%s(1)-s(1)
          a%f(i,p_a,k) = a%f(i,p_a,k)*b%f(i+s(1),p_b,k+s(3))
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        ! *******************************************************************
        ! *******************************************************************
        ! *******************************************************************

        subroutine multiply_plane_z_GF_S(a,b,p)
          implicit none
          type(grid_field),intent(inout) :: a
          real(cp),intent(in) :: b
          integer,intent(in) :: p
          integer :: i,j
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,p) = a%f(i,j,p)*b
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine
        subroutine multiply_plane_z_GF_GF(a,b,p_a,p_b)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b
          integer :: i,j
#ifdef _DEBUG_GF_
          call insist_shape_match(a,b,3,'multiply_plane_z_GF_GF')
#endif
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do j=1,a%s(2); do i=1,a%s(1)
          a%f(i,j,p_a) = a%f(i,j,p_a)*b%f(i,j,p_b)
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine
        subroutine multiply_plane_z_GF_GF_shift(a,b,p_a,p_b,s)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid_field),intent(in) :: b
          integer,intent(in) :: p_a,p_b
          integer,dimension(3),intent(in) :: s
          integer :: i,j
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP PARALLEL DO

#endif
          do j=1,a%s(2)-s(2); do i=1,a%s(1)-s(1)
          a%f(i,j,p_a) = a%f(i,j,p_a)*b%f(i+s(1),j+s(2),p_b)
          enddo; enddo
#ifdef _PARALLELIZE_GF_PLANE_
          !$OMP END PARALLEL DO

#endif
        end subroutine

      end module