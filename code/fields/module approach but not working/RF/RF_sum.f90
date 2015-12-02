      module RF_sum_mod
      use RF_mod
      implicit none
      private

#ifdef _SINGLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
      integer,parameter :: cp = selected_real_kind(32)
#endif

      public :: sum
      public :: sum_face
      public :: sum_edge

      interface sum;        module procedure sum_RF_SF;               end interface
      interface sum;        module procedure sum_RF_VF_collocated;    end interface
      interface sum_face;   module procedure sum_RF_VF_face;          end interface
      interface sum_edge;   module procedure sum_RF_VF_edge;          end interface

      contains

      function sum_RF_SF(a,s1,s2) result(m)
        implicit none
        type(realField),intent(in) :: a
        integer,dimension(3),intent(in) :: s1,s2
        real(cp) :: m,mTemp
        integer :: i,j,k
        mTemp = 0.0_cp
        !$OMP PARALLEL DO REDUCTION(+:mTemp)
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
          mTemp = mTemp + a%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        m = mTemp
      end function

      function sum_RF_VF_collocated(x,y,z,s1,s2) result(m)
        implicit none
        type(realField),intent(in) :: x,y,z
        integer,dimension(3),intent(in) :: s1,s2
        real(cp) :: m
        integer :: i,j,k
        real(cp) :: mTemp
        mTemp = 0.0_cp
        !$OMP PARALLEL DO REDUCTION(+:mTemp)
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
          mTemp = mTemp + x%f(i,j,k)
          mTemp = mTemp + y%f(i,j,k)
          mTemp = mTemp + z%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        m = mTemp
      end function

      function sum_RF_VF_face(x,y,z,s1,s2) result(m)
        implicit none
        type(realField),intent(in) :: x,y,z
        integer,dimension(3),intent(in) :: s1,s2
        real(cp) :: m
        integer :: i,j,k
        real(cp) :: mTempx,mTempy,mTempz,mTemp
        mTempx = 0.0_cp; mTempy = 0.0_cp; mTempz = 0.0_cp; mTemp = 0.0_cp
        !$OMP PARALLEL DO REDUCTION(+:mTempx,mTempy,mTempz)
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        mTempx = mTempx + x%f(i,j,k)
        mTempy = mTempy + y%f(i,j,k)
        mTempz = mTempz + z%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO REDUCTION(+:mTemp)
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        mTemp = mTemp + x%f(x%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO REDUCTION(+:mTemp)
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        mTemp = mTemp + y%f(i,y%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO REDUCTION(+:mTemp)
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        mTemp = mTemp + z%f(i,j,z%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
        m = mTemp + mTempx + mTempy + mTempz
      end function

      function sum_RF_VF_edge(x,y,z,s1,s2) result(m)
        implicit none
        type(realField),intent(in) :: x,y,z
        integer,dimension(3),intent(in) :: s1,s2
        real(cp) :: m
        integer :: i,j,k
        real(cp) :: mTempx,mTempy,mTempz,mTemp
        mTempx = 0.0_cp; mTempy = 0.0_cp; mTempz = 0.0_cp; mTemp = 0.0_cp
        !$OMP PARALLEL DO REDUCTION(+:mTempx,mTempy,mTempz)
        do k=s1(3),s2(3); do j=s1(2),s2(2); do i=s1(1),s2(1)
        mTempx = mTempx + x%f(i,j,k)
        mTempy = mTempy + y%f(i,j,k)
        mTempz = mTempz + z%f(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO REDUCTION(+:mTempy,mTempz)
        do k=s1(3),s2(3); do j=s1(2),s2(2)
        mTempy = mTempy + y%f(y%s(1),j,k)
        mTempz = mTempz + z%f(z%s(1),j,k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO REDUCTION(+:mTempx,mTempz)
        do k=s1(3),s2(3); do i=s1(1),s2(1)
        mTempx = mTempx + x%f(i,x%s(2),k)
        mTempz = mTempz + z%f(i,z%s(2),k)
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO REDUCTION(+:mTempx,mTempy)
        do j=s1(2),s2(2); do i=s1(1),s2(1)
        mTempx = mTempx + x%f(i,j,x%s(3))
        mTempy = mTempy + y%f(i,j,y%s(3))
        enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO REDUCTION(+:mTemp)
        do i=s1(1),s2(1)
        mTemp = mTemp + x%f(i,x%s(2),x%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO REDUCTION(+:mTemp)
        do j=s1(2),s2(2)
        mTemp = mTemp + y%f(y%s(1),j,y%s(3))
        enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO REDUCTION(+:mTemp)
        do k=s1(3),s2(3)
        mTemp = mTemp + z%f(z%s(1),z%s(2),k)
        enddo
        !$OMP END PARALLEL DO
        m = mTemp + mTempx + mTempy + mTempz
      end function

      end module