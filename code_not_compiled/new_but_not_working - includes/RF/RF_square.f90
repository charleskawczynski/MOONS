      module RF_square_mod
      use RF_mod
      implicit none
      private

      public :: square
      public :: square_face
      public :: square_edge

#ifdef _SINGLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
      integer,parameter :: cp = selected_real_kind(32)
#endif

      interface square;       module procedure square_RF_SF;             end interface
      interface square;       module procedure square_RF_VF_collocated;  end interface
      interface square_face;  module procedure square_RF_VF_face;        end interface
      interface square_edge;  module procedure square_RF_VF_edge;        end interface

      contains

      subroutine square_RF_SF(A,s1,s2)
        implicit none
        type(realField),intent(inout) :: A
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        A%f(i,j,k) = A%f(i,j,k) * A%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine square_RF_VF_collocated(x,y,z,s1,s2)
        implicit none
        type(realField),intent(inout) :: x,y,z
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        x%f(i,j,k) = x%f(i,j,k) * x%f(i,j,k)
        y%f(i,j,k) = y%f(i,j,k) * y%f(i,j,k)
        z%f(i,j,k) = z%f(i,j,k) * z%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine square_RF_VF_face(x,y,z,s1,s2)
        implicit none
        type(realField),intent(inout) :: x,y,z
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        x%f(i,j,k) = x%f(i,j,k) * x%f(i,j,k)
        y%f(i,j,k) = y%f(i,j,k) * y%f(i,j,k)
        z%f(i,j,k) = z%f(i,j,k) * z%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        x%f(x%s(1),j,k) = x%f(x%s(1),j,k) * x%f(x%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        y%f(i,y%s(2),k) = y%f(i,y%s(2),k) * y%f(i,y%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        z%f(i,j,z%s(3)) = z%f(i,j,z%s(3)) * z%f(i,j,z%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine square_RF_VF_edge(x,y,z,s1,s2)
        implicit none
        type(realField),intent(inout) :: x,y,z
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        x%f(i,j,k) = x%f(i,j,k) * x%f(i,j,k)
        y%f(i,j,k) = y%f(i,j,k) * y%f(i,j,k)
        z%f(i,j,k) = z%f(i,j,k) * z%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        y%f(y%s(1),j,k) = y%f(y%s(1),j,k) * y%f(y%s(1),j,k)
        z%f(z%s(1),j,k) = z%f(z%s(1),j,k) * z%f(z%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        x%f(i,x%s(2),k) = x%f(i,x%s(2),k) * x%f(i,x%s(2),k)
        z%f(i,z%s(2),k) = z%f(i,z%s(2),k) * z%f(i,z%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        x%f(i,j,x%s(3)) = x%f(i,j,x%s(3)) * x%f(i,j,x%s(3))
        y%f(i,j,y%s(3)) = y%f(i,j,y%s(3)) * y%f(i,j,y%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do i=s1(1),s2(1)
        x%f(i,x%s(2),x%s(3)) = x%f(i,x%s(2),x%s(3)) * x%f(i,x%s(2),x%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2)
        y%f(y%s(1),j,y%s(3)) = y%f(y%s(1),j,y%s(3)) * y%f(y%s(1),j,y%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3)
        z%f(z%s(1),z%s(2),k) = z%f(z%s(1),z%s(2),k) * z%f(z%s(1),z%s(2),k)
        enddo
        !$OMP END PARALLEL DO
      end subroutine

      end module