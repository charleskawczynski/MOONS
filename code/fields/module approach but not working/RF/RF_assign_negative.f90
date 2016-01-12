      module RF_assign_negative_mod
      use RF_mod
      implicit none

#ifdef _SINGLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
      integer,parameter :: cp = selected_real_kind(32)
#endif

      private
      public :: assign_negative
      public :: assign_negative_face
      public :: assign_negative_edge

      interface assign_negative;        module procedure SF_SF;           end interface
      interface assign_negative;        module procedure SF_S;            end interface

      interface assign_negative;        module procedure VF_VF;           end interface
      interface assign_negative;        module procedure VF_SF;           end interface
      interface assign_negative;        module procedure VF_S;            end interface

      interface assign_negative_face;   module procedure face_VF_VF;      end interface
      interface assign_negative_face;   module procedure face_VF_SF;      end interface
      interface assign_negative_face;   module procedure face_VF_S;       end interface

      interface assign_negative_edge;   module procedure edge_VF_VF;      end interface
      interface assign_negative_edge;   module procedure edge_VF_SF;      end interface
      interface assign_negative_edge;   module procedure edge_VF_S;       end interface

      contains

      subroutine SF_SF(A,B,s1,s2)
        implicit none
        type(realField),intent(inout) :: A
        type(realField),intent(in) :: B
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        A%f(i,j,k) = -B%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine SF_S(A,B,s1,s2)
        implicit none
        type(realField),intent(inout) :: A
        real(cp),intent(in) :: B
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        A%f(i,j,k) = -B
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      ! ********************************************************************
      ! ************************** COLLOCATED VF ***************************
      ! ********************************************************************

      subroutine VF_VF(Ax,Ay,Az,Bx,By,Bz,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: Bx,By,Bz
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = -Bx%f(i,j,k)
        Ay%f(i,j,k) = -By%f(i,j,k)
        Az%f(i,j,k) = -Bz%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine VF_SF(Ax,Ay,Az,B,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: B
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = -B%f(i,j,k)
        Ay%f(i,j,k) = -B%f(i,j,k)
        Az%f(i,j,k) = -B%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine VF_S(Ax,Ay,Az,B,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        real(cp),intent(in) :: B
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = -B
        Ay%f(i,j,k) = -B
        Az%f(i,j,k) = -B
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      ! ********************************************************************
      ! ******************************* FACE *******************************
      ! ********************************************************************

      subroutine face_VF_VF(Ax,Ay,Az,Bx,By,Bz,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: Bx,By,Bz
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = -Bx%f(i,j,k)
        Ay%f(i,j,k) = -By%f(i,j,k)
        Az%f(i,j,k) = -Bz%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ax%f(Ax%s(1),j,k) = -Bx%f(Ax%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ay%f(i,Ay%s(2),k) = -By%f(i,Ay%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Az%f(i,j,Az%s(3)) = -Bz%f(i,j,Az%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine face_VF_SF(Ax,Ay,Az,B,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: B
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = -B%f(i,j,k)
        Ay%f(i,j,k) = -B%f(i,j,k)
        Az%f(i,j,k) = -B%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ax%f(Ax%s(1),j,k) = -B%f(Ax%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ay%f(i,Ay%s(2),k) = -B%f(i,Ay%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Az%f(i,j,Az%s(3)) = -B%f(i,j,Az%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine face_VF_S(Ax,Ay,Az,B,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        real(cp),intent(in) :: B
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = -B
        Ay%f(i,j,k) = -B
        Az%f(i,j,k) = -B
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ax%f(Ax%s(1),j,k) = -B
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ay%f(i,Ay%s(2),k) = -B
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Az%f(i,j,Az%s(3)) = -B
        enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      ! ********************************************************************
      ! ******************************* EDGE *******************************
      ! ********************************************************************

      subroutine edge_VF_VF(Ax,Ay,Az,Bx,By,Bz,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: Bx,By,Bz
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = -Bx%f(i,j,k)
        Ay%f(i,j,k) = -By%f(i,j,k)
        Az%f(i,j,k) = -Bz%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,k) = -By%f(Ay%s(1),j,k)
        Az%f(Az%s(1),j,k) = -Bz%f(Az%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),k) = -Bx%f(i,Ax%s(2),k)
        Az%f(i,Az%s(2),k) = -Bz%f(i,Az%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,Ax%s(3)) = -Bx%f(i,j,Ax%s(3))
        Ay%f(i,j,Ay%s(3)) = -By%f(i,j,Ay%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),Ax%s(3)) = -Bx%f(i,Ax%s(2),Ax%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,Ay%s(3)) = -By%f(Ay%s(1),j,Ay%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3)
        Az%f(Az%s(1),Az%s(2),k) = -Bz%f(Az%s(1),Az%s(2),k)
        enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine edge_VF_SF(Ax,Ay,Az,B,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: B
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = -B%f(i,j,k)
        Ay%f(i,j,k) = -B%f(i,j,k)
        Az%f(i,j,k) = -B%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,k) = -B%f(Ay%s(1),j,k)
        Az%f(Az%s(1),j,k) = -B%f(Az%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),k) = -B%f(i,Ax%s(2),k)
        Az%f(i,Az%s(2),k) = -B%f(i,Az%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,Ax%s(3)) = -B%f(i,j,Ax%s(3))
        Ay%f(i,j,Ay%s(3)) = -B%f(i,j,Ay%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),Ax%s(3)) = -B%f(i,Ax%s(2),Ax%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,Ay%s(3)) = -B%f(Ay%s(1),j,Ay%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3)
        Az%f(Az%s(1),Az%s(2),k) = -B%f(Az%s(1),Az%s(2),k)
        enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine edge_VF_S(Ax,Ay,Az,B,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        real(cp),intent(in) :: B
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = -B
        Ay%f(i,j,k) = -B
        Az%f(i,j,k) = -B
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,k) = -B
        Az%f(Az%s(1),j,k) = -B
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),k) = -B
        Az%f(i,Az%s(2),k) = -B
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,Ax%s(3)) = -B
        Ay%f(i,j,Ay%s(3)) = -B
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),Ax%s(3)) = -B
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,Ay%s(3)) = -B
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3)
        Az%f(Az%s(1),Az%s(2),k) = -B
        enddo
        !$OMP END PARALLEL DO
      end subroutine

      end module