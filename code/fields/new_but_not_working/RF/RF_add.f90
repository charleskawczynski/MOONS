      module RF_add_mod
      use RF_mod
      implicit none
      private

      public :: add
      public :: add_face
      public :: add_edge

#ifdef _SINGLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
      integer,parameter :: cp = selected_real_kind(32)
#endif

      interface add;        module procedure SF_SF;           end interface
      interface add;        module procedure SF_SF_SF;        end interface
      interface add;        module procedure SF_S_SF;         end interface
      interface add;        module procedure SF_S;            end interface
      interface add;        module procedure S_SF;            end interface

      interface add;        module procedure VF_VF;           end interface
      interface add;        module procedure VF_SF;           end interface
      interface add;        module procedure VF_VF_VF;        end interface
      interface add;        module procedure VF_VF_SF;        end interface
      interface add;        module procedure VF_S_VF;         end interface
      interface add;        module procedure VF_S;            end interface
      interface add;        module procedure S_VF;            end interface

      interface add_face;   module procedure face_VF_VF;      end interface
      interface add_face;   module procedure face_VF_SF;      end interface
      interface add_face;   module procedure face_VF_VF_VF;   end interface
      interface add_face;   module procedure face_VF_VF_SF;   end interface
      interface add_face;   module procedure face_VF_S_VF;    end interface
      interface add_face;   module procedure face_VF_S;       end interface
      interface add_face;   module procedure face_S_VF;       end interface

      interface add_edge;   module procedure edge_VF_VF;      end interface
      interface add_edge;   module procedure edge_VF_SF;      end interface
      interface add_edge;   module procedure edge_VF_VF_VF;   end interface
      interface add_edge;   module procedure edge_VF_VF_SF;   end interface
      interface add_edge;   module procedure edge_VF_S_VF;    end interface
      interface add_edge;   module procedure edge_VF_S;       end interface
      interface add_edge;   module procedure edge_S_VF;       end interface

      contains

      subroutine SF_SF(A,B,s1,s2)
        implicit none
        type(realField),intent(inout) :: A
        type(realField),intent(in) :: B
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        A%f(i,j,k) = A%f(i,j,k) + B%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine SF_SF_SF(A,B,C,s1,s2)
        implicit none
        type(realField),intent(inout) :: A
        type(realField),intent(in) :: B,C
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        A%f(i,j,k) = B%f(i,j,k) + C%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine SF_S_SF(A,B,C,s1,s2)
        implicit none
        type(realField),intent(inout) :: A
        real(cp),intent(in) :: B
        type(realField),intent(in) :: C
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        A%f(i,j,k) = B + C%f(i,j,k)
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
        A%f(i,j,k) = A%f(i,j,k) + B
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine S_SF(c2,A,s1,s2)
        implicit none
        real(cp),intent(in) :: c2
        type(realField),intent(inout) :: A
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        A%f(i,j,k) = c2 + A%f(i,j,k)
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
        Ax%f(i,j,k) = Ax%f(i,j,k) + Bx%f(i,j,k)
        Ay%f(i,j,k) = Ay%f(i,j,k) + By%f(i,j,k)
        Az%f(i,j,k) = Az%f(i,j,k) + Bz%f(i,j,k)
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
        Ax%f(i,j,k) = Ax%f(i,j,k) + B%f(i,j,k)
        Ay%f(i,j,k) = Ay%f(i,j,k) + B%f(i,j,k)
        Az%f(i,j,k) = Az%f(i,j,k) + B%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine VF_VF_VF(Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: Bx,By,Bz,Cx,Cy,Cz
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = Bx%f(i,j,k) + Cx%f(i,j,k)
        Ay%f(i,j,k) = By%f(i,j,k) + Cy%f(i,j,k)
        Az%f(i,j,k) = Bz%f(i,j,k) + Cz%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine VF_VF_SF(Ax,Ay,Az,Bx,By,Bz,C,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: Bx,By,Bz,C
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = Bx%f(i,j,k) + C%f(i,j,k)
        Ay%f(i,j,k) = By%f(i,j,k) + C%f(i,j,k)
        Az%f(i,j,k) = Bz%f(i,j,k) + C%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine VF_S_VF(Ax,Ay,Az,B,Cx,Cy,Cz,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: Cx,Cy,Cz
        real(cp),intent(in) :: B
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = B + Cx%f(i,j,k)
        Ay%f(i,j,k) = B + Cy%f(i,j,k)
        Az%f(i,j,k) = B + Cz%f(i,j,k)
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
        Ax%f(i,j,k) = Ax%f(i,j,k) + B
        Ay%f(i,j,k) = Ay%f(i,j,k) + B
        Az%f(i,j,k) = Az%f(i,j,k) + B
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine S_VF(c2,Ax,Ay,Az,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        real(cp),intent(in) :: c2
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = c2 + Ax%f(i,j,k)
        Ay%f(i,j,k) = c2 + Ay%f(i,j,k)
        Az%f(i,j,k) = c2 + Az%f(i,j,k)
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
        Ax%f(i,j,k) = Ax%f(i,j,k) + Bx%f(i,j,k)
        Ay%f(i,j,k) = Ay%f(i,j,k) + By%f(i,j,k)
        Az%f(i,j,k) = Az%f(i,j,k) + Bz%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ax%f(Ax%s(1),j,k) = Ax%f(Ax%s(1),j,k) + Bx%f(Ax%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ay%f(i,Ay%s(2),k) = Ay%f(i,Ay%s(2),k) + By%f(i,Ay%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Az%f(i,j,Az%s(3)) = Az%f(i,j,Az%s(3)) + Bz%f(i,j,Az%s(3))
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
        Ax%f(i,j,k) = Ax%f(i,j,k) + B%f(i,j,k)
        Ay%f(i,j,k) = Ay%f(i,j,k) + B%f(i,j,k)
        Az%f(i,j,k) = Az%f(i,j,k) + B%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ax%f(Ax%s(1),j,k) = Ax%f(Ax%s(1),j,k) + B%f(Ax%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ay%f(i,Ay%s(2),k) = Ay%f(i,Ay%s(2),k) + B%f(i,Ay%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Az%f(i,j,Az%s(3)) = Az%f(i,j,Az%s(3)) + B%f(i,j,Az%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine face_VF_VF_VF(Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: Bx,By,Bz,Cx,Cy,Cz
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = Bx%f(i,j,k) + Cx%f(i,j,k)
        Ay%f(i,j,k) = By%f(i,j,k) + Cy%f(i,j,k)
        Az%f(i,j,k) = Bz%f(i,j,k) + Cz%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ax%f(Ax%s(1),j,k) = Bx%f(Ax%s(1),j,k) + Cx%f(Ax%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ay%f(i,Ay%s(2),k) = By%f(i,Ay%s(2),k) + Cy%f(i,Ay%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Az%f(i,j,Az%s(3)) = Bz%f(i,j,Az%s(3)) + Cz%f(i,j,Az%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine face_VF_VF_SF(Ax,Ay,Az,Bx,By,Bz,C,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: Bx,By,Bz,C
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = Bx%f(i,j,k) + C%f(i,j,k)
        Ay%f(i,j,k) = By%f(i,j,k) + C%f(i,j,k)
        Az%f(i,j,k) = Bz%f(i,j,k) + C%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ax%f(Ax%s(1),j,k) = Bx%f(Ax%s(1),j,k) + C%f(Ax%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ay%f(i,Ay%s(2),k) = By%f(i,Ay%s(2),k) + C%f(i,Ay%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Az%f(i,j,Az%s(3)) = Bz%f(i,j,Az%s(3)) + C%f(i,j,Az%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine face_VF_S_VF(Ax,Ay,Az,B,Cx,Cy,Cz,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: Cx,Cy,Cz
        real(cp),intent(in) :: B
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = B + Cx%f(i,j,k)
        Ay%f(i,j,k) = B + Cy%f(i,j,k)
        Az%f(i,j,k) = B + Cz%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ax%f(Ax%s(1),j,k) = B + Cx%f(Ax%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ay%f(i,Ay%s(2),k) = B + Cy%f(i,Ay%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Az%f(i,j,Az%s(3)) = B + Cz%f(i,j,Az%s(3))
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
        Ax%f(i,j,k) = Ax%f(i,j,k) + B
        Ay%f(i,j,k) = Ay%f(i,j,k) + B
        Az%f(i,j,k) = Az%f(i,j,k) + B
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ax%f(Ax%s(1),j,k) = Ax%f(Ax%s(1),j,k) + B
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ay%f(i,Ay%s(2),k) = Ay%f(i,Ay%s(2),k) + B
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Az%f(i,j,Az%s(3)) = Az%f(i,j,Az%s(3)) + B
        enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine face_S_VF(c2,Ax,Ay,Az,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        real(cp),intent(in) :: c2
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = c2 + Ax%f(i,j,k)
        Ay%f(i,j,k) = c2 + Ay%f(i,j,k)
        Az%f(i,j,k) = c2 + Az%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ax%f(Ax%s(1),j,k) = c2 + Ax%f(Ax%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ay%f(i,Ay%s(2),k) = c2 + Ay%f(i,Ay%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Az%f(i,j,Az%s(3)) = c2 + Az%f(i,j,Az%s(3))
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
        Ax%f(i,j,k) = Ax%f(i,j,k) + Bx%f(i,j,k)
        Ay%f(i,j,k) = Ay%f(i,j,k) + By%f(i,j,k)
        Az%f(i,j,k) = Az%f(i,j,k) + Bz%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,k) = Ay%f(Ay%s(1),j,k) + By%f(Ay%s(1),j,k)
        Az%f(Az%s(1),j,k) = Az%f(Az%s(1),j,k) + Bz%f(Az%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),k) = Ax%f(i,Ax%s(2),k) + Bx%f(i,Ax%s(2),k)
        Az%f(i,Az%s(2),k) = Az%f(i,Az%s(2),k) + Bz%f(i,Az%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,Ax%s(3)) = Ax%f(i,j,Ax%s(3)) + Bx%f(i,j,Ax%s(3))
        Ay%f(i,j,Ay%s(3)) = Ay%f(i,j,Ay%s(3)) + By%f(i,j,Ay%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),Ax%s(3)) = Ax%f(i,Ax%s(2),Ax%s(3)) + Bx%f(i,Ax%s(2),Ax%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,Ay%s(3)) = Ay%f(Ay%s(1),j,Ay%s(3)) + By%f(Ay%s(1),j,Ay%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3)
        Az%f(Az%s(1),Az%s(2),k) = Az%f(Az%s(1),Az%s(2),k) + Bz%f(Az%s(1),Az%s(2),k)
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
        Ax%f(i,j,k) = Ax%f(i,j,k) + B%f(i,j,k)
        Ay%f(i,j,k) = Ay%f(i,j,k) + B%f(i,j,k)
        Az%f(i,j,k) = Az%f(i,j,k) + B%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,k) = Ay%f(Ay%s(1),j,k) + B%f(Ay%s(1),j,k)
        Az%f(Az%s(1),j,k) = Az%f(Az%s(1),j,k) + B%f(Az%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),k) = Ax%f(i,Ax%s(2),k) + B%f(i,Ax%s(2),k)
        Az%f(i,Az%s(2),k) = Az%f(i,Az%s(2),k) + B%f(i,Az%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,Ax%s(3)) = Ax%f(i,j,Ax%s(3)) + B%f(i,j,Ax%s(3))
        Ay%f(i,j,Ay%s(3)) = Ay%f(i,j,Ay%s(3)) + B%f(i,j,Ay%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),Ax%s(3)) = Ax%f(i,Ax%s(2),Ax%s(3)) + B%f(i,Ax%s(2),Ax%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,Ay%s(3)) = Ay%f(Ay%s(1),j,Ay%s(3)) + B%f(Ay%s(1),j,Ay%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3)
        Az%f(Az%s(1),Az%s(2),k) = Az%f(Az%s(1),Az%s(2),k) + B%f(Az%s(1),Az%s(2),k)
        enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine edge_VF_VF_VF(Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: Bx,By,Bz,Cx,Cy,Cz
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = Bx%f(i,j,k) + Cx%f(i,j,k)
        Ay%f(i,j,k) = By%f(i,j,k) + Cy%f(i,j,k)
        Az%f(i,j,k) = Bz%f(i,j,k) + Cz%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,k) = By%f(Ay%s(1),j,k) + Cy%f(Ay%s(1),j,k)
        Az%f(Az%s(1),j,k) = Bz%f(Az%s(1),j,k) + Cz%f(Az%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),k) = Bx%f(i,Ax%s(2),k) + Cx%f(i,Ax%s(2),k)
        Az%f(i,Az%s(2),k) = Bz%f(i,Az%s(2),k) + Cz%f(i,Az%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,Ax%s(3)) = Bx%f(i,j,Ax%s(3)) + Cx%f(i,j,Ax%s(3))
        Ay%f(i,j,Ay%s(3)) = By%f(i,j,Ay%s(3)) + Cy%f(i,j,Ay%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),Ax%s(3)) = Bx%f(i,Ax%s(2),Ax%s(3)) + Cx%f(i,Ax%s(2),Ax%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,Ay%s(3)) = By%f(Ay%s(1),j,Ay%s(3)) + Cy%f(Ay%s(1),j,Ay%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3)
        Az%f(Az%s(1),Az%s(2),k) = Bz%f(Az%s(1),Az%s(2),k) + Cz%f(Az%s(1),Az%s(2),k)
        enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine edge_VF_VF_SF(Ax,Ay,Az,Bx,By,Bz,C,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        type(realField),intent(in) :: Bx,By,Bz,C
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = Bx%f(i,j,k) + C%f(i,j,k)
        Ay%f(i,j,k) = By%f(i,j,k) + C%f(i,j,k)
        Az%f(i,j,k) = Bz%f(i,j,k) + C%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,k) = By%f(Ay%s(1),j,k) + C%f(Ay%s(1),j,k)
        Az%f(Az%s(1),j,k) = Bz%f(Az%s(1),j,k) + C%f(Az%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),k) = Bx%f(i,Ax%s(2),k) + C%f(i,Ax%s(2),k)
        Az%f(i,Az%s(2),k) = Bz%f(i,Az%s(2),k) + C%f(i,Az%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,Ax%s(3)) = Bx%f(i,j,Ax%s(3)) + C%f(i,j,Ax%s(3))
        Ay%f(i,j,Ay%s(3)) = By%f(i,j,Ay%s(3)) + C%f(i,j,Ay%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),Ax%s(3)) = Bx%f(i,Ax%s(2),Ax%s(3)) + C%f(i,Ax%s(2),Ax%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,Ay%s(3)) = By%f(Ay%s(1),j,Ay%s(3)) + C%f(Ay%s(1),j,Ay%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3)
        Az%f(Az%s(1),Az%s(2),k) = Bz%f(Az%s(1),Az%s(2),k) + C%f(Az%s(1),Az%s(2),k)
        enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine edge_VF_S_VF(Ax,Ay,Az,B,Cx,Cy,Cz,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        real(cp),intent(in) :: B
        type(realField),intent(in) :: Cx,Cy,Cz
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = B + Cx%f(i,j,k)
        Ay%f(i,j,k) = B + Cy%f(i,j,k)
        Az%f(i,j,k) = B + Cz%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,k) = B + Cy%f(Ay%s(1),j,k)
        Az%f(Az%s(1),j,k) = B + Cz%f(Az%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),k) = B + Cx%f(i,Ax%s(2),k)
        Az%f(i,Az%s(2),k) = B + Cz%f(i,Az%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,Ax%s(3)) = B + Cx%f(i,j,Ax%s(3))
        Ay%f(i,j,Ay%s(3)) = B + Cy%f(i,j,Ay%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),Ax%s(3)) = B + Cx%f(i,Ax%s(2),Ax%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,Ay%s(3)) = B + Cy%f(Ay%s(1),j,Ay%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3)
        Az%f(Az%s(1),Az%s(2),k) = B + Cz%f(Az%s(1),Az%s(2),k)
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
        Ax%f(i,j,k) = Ax%f(i,j,k) + B
        Ay%f(i,j,k) = Ay%f(i,j,k) + B
        Az%f(i,j,k) = Az%f(i,j,k) + B
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,k) = Ay%f(Ay%s(1),j,k) + B
        Az%f(Az%s(1),j,k) = Az%f(Az%s(1),j,k) + B
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),k) = Ax%f(i,Ax%s(2),k) + B
        Az%f(i,Az%s(2),k) = Az%f(i,Az%s(2),k) + B
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,Ax%s(3)) = Ax%f(i,j,Ax%s(3)) + B
        Ay%f(i,j,Ay%s(3)) = Ay%f(i,j,Ay%s(3)) + B
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),Ax%s(3)) = Ax%f(i,Ax%s(2),Ax%s(3)) + B
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,Ay%s(3)) = Ay%f(Ay%s(1),j,Ay%s(3)) + B
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3)
        Az%f(Az%s(1),Az%s(2),k) = Az%f(Az%s(1),Az%s(2),k) + B
        enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine edge_S_VF(c2,Ax,Ay,Az,s1,s2)
        implicit none
        type(realField),intent(inout) :: Ax,Ay,Az
        real(cp),intent(in) :: c2
        integer,dimension(3),intent(in) :: s1,s2
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,k) = c2 + Ax%f(i,j,k)
        Ay%f(i,j,k) = c2 + Ay%f(i,j,k)
        Az%f(i,j,k) = c2 + Az%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,k) = c2 + Ay%f(Ay%s(1),j,k)
        Az%f(Az%s(1),j,k) = c2 + Az%f(Az%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),k) = c2 + Ax%f(i,Ax%s(2),k)
        Az%f(i,Az%s(2),k) = c2 + Az%f(i,Az%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        Ax%f(i,j,Ax%s(3)) = c2 + Ax%f(i,j,Ax%s(3))
        Ay%f(i,j,Ay%s(3)) = c2 + Ay%f(i,j,Ay%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do i=s1(1),s2(1)
        Ax%f(i,Ax%s(2),Ax%s(3)) = c2 + Ax%f(i,Ax%s(2),Ax%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do j=s1(2),s2(2)
        Ay%f(Ay%s(1),j,Ay%s(3)) = c2 + Ay%f(Ay%s(1),j,Ay%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=s1(3),s2(3)
        Az%f(Az%s(1),Az%s(2),k) = c2 + Az%f(Az%s(1),Az%s(2),k)
        enddo
        !$OMP END PARALLEL DO
      end subroutine

      end module