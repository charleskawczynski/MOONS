       module GF_embed_extract_mod
       ! Pre-processor directives: (_PARALLELIZE_EMBED_EXTRACT_)
       use overlap_mod
       use GF_mod
       implicit none
       private
       public :: EM
       public :: EX
       interface EM;     module procedure embed_GF;    end interface
       interface EX;     module procedure extract_GF;  end interface

       contains

       subroutine embed_extract_GF(A,B,A1,A2,B1,B2)
         implicit none
         type(grid_field),intent(inout) :: A
         type(grid_field),intent(in) :: B
         integer,dimension(3),intent(in) :: A1,A2,B1,B2
#ifdef _PARALLELIZE_EMBED_EXTRACT_
         integer :: i,j,k
         integer :: suppress_warning
         suppress_warning = B2(1) ! B2 is not needed for parallel computations
         !$OMP PARALLEL DO
         do k=A1(3),A2(3);do j=A1(2),A2(2);do i=A1(1),A2(1)
         A%f(i,j,k) = B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
#else
         A%f(A1(1):A2(1),A1(2):A2(2),A1(3):A2(3)) = &
         B%f(B1(1):B2(1),B1(2):B2(2),B1(3):B2(3))
#endif
       end subroutine

       subroutine embed_GF(A,B,AB)
         implicit none
         type(grid_field),intent(inout) :: A
         type(grid_field),intent(in) :: B
         type(overlap),dimension(3),intent(in) :: AB
         call embed_extract_GF(A,B,AB(1:3)%i2(1),&
                                   AB(1:3)%i2(2),&
                                   AB(1:3)%i1(1),&
                                   AB(1:3)%i1(2))
       end subroutine

       subroutine extract_GF(A,B,AB)
         implicit none
         type(grid_field),intent(inout) :: A
         type(grid_field),intent(in) :: B
         type(overlap),dimension(3),intent(in) :: AB
         call embed_extract_GF(A,B,AB(1:3)%i1(1),&
                                   AB(1:3)%i1(2),&
                                   AB(1:3)%i2(1),&
                                   AB(1:3)%i2(2))
       end subroutine

       end module