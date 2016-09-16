       module ops_internal_BC_mod
       ! Not all embed / extract routines are used for each method. For example,
       ! the CT method only uses embedEdge, and not embedCC or embedFace.
       ! 
       ! Pre-processor directives: (_PARALLELIZE_INTERNAL_BC_,_DEBUG_INTERNAL_BC_)

       use current_precision_mod
       use overlap_mod
       use mesh_mod
       use apply_BCs_mod
       use domain_mod
       use RF_mod
       use SF_mod
       use VF_mod

       implicit none

       private
       public :: internal_BC_Face

       interface internal_BC_Face;  module procedure internal_BC_Face_SF;        end interface
       interface internal_BC_Face;  module procedure internal_BC_Face_VF;        end interface

       interface IBC;               module procedure internal_BC_RF;             end interface

       contains

       subroutine internal_BC_RF_raw_face(A,A1,A2,CC_along)
         implicit none
         type(realField),intent(inout) :: A
         integer,dimension(3),intent(in) :: A1,A2
         logical,dimension(3),intent(in) :: CC_along
#ifdef _PARALLELIZE_INTERNAL_BC_
         integer :: i,j,k
         if (CC_along(1)) then
           i=A1(1)
           !$OMP PARALLEL DO
           do k=A1(3)+1,A2(3)-1;do j=A1(2)+1,A2(2)-1 ! xmin
           A%f(i+1,j,k) = -A%f(i,j,k)
           enddo; enddo
           !$OMP END PARALLEL DO
           i=A2(1)
           !$OMP PARALLEL DO
           do k=A1(3)+1,A2(3)-1;do j=A1(2)+1,A2(2)-1 ! xmax
           A%f(i-1,j,k) = -A%f(i,j,k)
           enddo; enddo
           !$OMP END PARALLEL DO
         endif
         if (CC_along(2)) then
           j=A1(2)
           !$OMP PARALLEL DO
           do k=A1(3)+1,A2(3)-1;do i=A1(1)+1,A2(1)-1 ! ymin
           A%f(i,j+1,k) = -A%f(i,j,k)
           enddo; enddo
           !$OMP END PARALLEL DO
           j=A2(2)
           !$OMP PARALLEL DO
           do k=A1(3)+1,A2(3)-1;do i=A1(1)+1,A2(1)-1! ymax
           A%f(i,j-1,k) = -A%f(i,j,k)
           enddo; enddo
           !$OMP END PARALLEL DO
         endif
         if (CC_along(3)) then
           k=A1(3)
           !$OMP PARALLEL DO
           do j=A1(2)+1,A2(2)-1;do i=A1(1)+1,A2(1)-1 ! zmin
           A%f(i,j,k+1) = -A%f(i,j,k)
           enddo; enddo
           !$OMP END PARALLEL DO
           k=A2(3)
           !$OMP PARALLEL DO
           do j=A1(2)+1,A2(2)-1;do i=A1(1)+1,A2(1)-1 ! zmax
           A%f(i,j,k-1) = -A%f(i,j,k)
           enddo; enddo
           !$OMP END PARALLEL DO
         endif
#else
         if (CC_along(1)) then
         A%f(A1(1)+1,A1(2)+1:A2(2)-1,A1(3)+1:A2(3)-1) = -A%f(A1(1)  ,A1(2)+1:A2(2)-1,A1(3)+1:A2(3)-1)
         A%f(A2(1)-1,A1(2)+1:A2(2)-1,A1(3)+1:A2(3)-1) = -A%f(A2(1)  ,A1(2)+1:A2(2)-1,A1(3)+1:A2(3)-1)
         endif
         if (CC_along(2)) then
         A%f(A1(1)+1:A2(1)-1,A1(2)+1,A1(3)+1:A2(3)-1) = -A%f(A1(1)+1:A2(1)-1,A1(2)  ,A1(3)+1:A2(3)-1)
         A%f(A1(1)+1:A2(1)-1,A2(2)-1,A1(3)+1:A2(3)-1) = -A%f(A1(1)+1:A2(1)-1,A2(2)  ,A1(3)+1:A2(3)-1)
         endif
         if (CC_along(3)) then
         A%f(A1(1)+1:A2(1)-1,A1(2)+1:A2(2)-1,A1(3)+1) = -A%f(A1(1)+1:A2(1)-1,A1(2)+1:A2(2)-1,A1(3)  )
         A%f(A1(1)+1:A2(1)-1,A1(2)+1:A2(2)-1,A2(3)-1) = -A%f(A1(1)+1:A2(1)-1,A1(2)+1:A2(2)-1,A2(3)  )
         endif
#endif
         call internal_BC_RF_raw_edge(A,A1,A2,CC_along)
       end subroutine

       subroutine internal_BC_RF_raw_edge(A,A1,A2,CC_along)
         implicit none
         type(realField),intent(inout) :: A
         integer,dimension(3),intent(in) :: A1,A2
         logical,dimension(3),intent(in) :: CC_along
         if (CC_along(2).and.CC_along(3)) then
         A%f(A1(1)+1:A2(1)-1,A1(2)+1,A1(3)+1) = -(A%f(A1(1)+1:A2(1)-1,A1(2)  ,A1(3)  )+&
                                                  A%f(A1(1)+1:A2(1)-1,A1(2)+1,A1(3)  )+&
                                                  A%f(A1(1)+1:A2(1)-1,A1(2)  ,A1(3)+1)+&
                                                  A%f(A1(1)+1:A2(1)-1,A1(2)+1,A1(3)+1))
         A%f(A1(1)+1:A2(1)-1,A2(2)+1,A1(3)+1) = -(A%f(A1(1)+1:A2(1)-1,A2(2)  ,A1(3)  )+&
                                                  A%f(A1(1)+1:A2(1)-1,A2(2)-1,A1(3)  )+&
                                                  A%f(A1(1)+1:A2(1)-1,A2(2)  ,A1(3)+1)+&
                                                  A%f(A1(1)+1:A2(1)-1,A2(2)-1,A1(3)+1))
         A%f(A1(1)+1:A2(1)-1,A1(2)+1,A2(3)+1) = -(A%f(A1(1)+1:A2(1)-1,A1(2)  ,A2(3)  )+&
                                                  A%f(A1(1)+1:A2(1)-1,A1(2)+1,A2(3)  )+&
                                                  A%f(A1(1)+1:A2(1)-1,A1(2)  ,A2(3)-1)+&
                                                  A%f(A1(1)+1:A2(1)-1,A1(2)+1,A2(3)-1))
         A%f(A1(1)+1:A2(1)-1,A2(2)+1,A2(3)+1) = -(A%f(A1(1)+1:A2(1)-1,A2(2)  ,A2(3)  )+&
                                                  A%f(A1(1)+1:A2(1)-1,A2(2)-1,A2(3)  )+&
                                                  A%f(A1(1)+1:A2(1)-1,A2(2)  ,A2(3)-1)+&
                                                  A%f(A1(1)+1:A2(1)-1,A2(2)-1,A2(3)-1))
         endif
         if (CC_along(1).and.CC_along(3)) then
         A%f(A1(1)+1,A1(2)+1:A2(2)-1,A1(3)+1) = -(A%f(A1(1)  ,A1(2)+1:A2(2)-1,A1(3)  )+&
                                                  A%f(A1(1)+1,A1(2)+1:A2(2)-1,A1(3)  )+&
                                                  A%f(A1(1)  ,A1(2)+1:A2(2)-1,A1(3)+1)+&
                                                  A%f(A1(1)+1,A1(2)+1:A2(2)-1,A1(3)+1))
         A%f(A2(1)+1,A1(2)+1:A2(2)-1,A1(3)+1) = -(A%f(A2(1)  ,A1(2)+1:A2(2)-1,A1(3)  )+&
                                                  A%f(A2(1)-1,A1(2)+1:A2(2)-1,A1(3)  )+&
                                                  A%f(A2(1)  ,A1(2)+1:A2(2)-1,A1(3)+1)+&
                                                  A%f(A2(1)-1,A1(2)+1:A2(2)-1,A1(3)+1))
         A%f(A1(1)+1,A1(2)+1:A2(2)-1,A2(3)+1) = -(A%f(A1(1)  ,A1(2)+1:A2(2)-1,A2(3)  )+&
                                                  A%f(A1(1)+1,A1(2)+1:A2(2)-1,A2(3)  )+&
                                                  A%f(A1(1)  ,A1(2)+1:A2(2)-1,A2(3)-1)+&
                                                  A%f(A1(1)+1,A1(2)+1:A2(2)-1,A2(3)-1))
         A%f(A2(1)+1,A1(2)+1:A2(2)-1,A2(3)+1) = -(A%f(A2(1)  ,A1(2)+1:A2(2)-1,A2(3)  )+&
                                                  A%f(A2(1)-1,A1(2)+1:A2(2)-1,A2(3)  )+&
                                                  A%f(A2(1)  ,A1(2)+1:A2(2)-1,A2(3)-1)+&
                                                  A%f(A2(1)-1,A1(2)+1:A2(2)-1,A2(3)-1))
         endif
         if (CC_along(1).and.CC_along(2)) then
         A%f(A1(1)+1,A1(3)+1,A1(2)+1:A2(2)-1) = -(A%f(A1(1)  ,A1(2)  ,A1(3)+1:A2(3)-1)+&
                                                  A%f(A1(1)+1,A1(2)  ,A1(3)+1:A2(3)-1)+&
                                                  A%f(A1(1)  ,A1(2)+1,A1(3)+1:A2(3)-1)+&
                                                  A%f(A1(1)+1,A1(2)+1,A1(3)+1:A2(3)-1))
         A%f(A2(1)+1,A1(3)+1,A1(2)+1:A2(2)-1) = -(A%f(A2(1)  ,A1(2)  ,A1(3)+1:A2(3)-1)+&
                                                  A%f(A2(1)-1,A1(2)  ,A1(3)+1:A2(3)-1)+&
                                                  A%f(A2(1)  ,A1(2)+1,A1(3)+1:A2(3)-1)+&
                                                  A%f(A2(1)-1,A1(2)+1,A1(3)+1:A2(3)-1))
         A%f(A1(1)+1,A2(3)+1,A1(2)+1:A2(2)-1) = -(A%f(A1(1)  ,A2(2)  ,A1(3)+1:A2(3)-1)+&
                                                  A%f(A1(1)+1,A2(2)  ,A1(3)+1:A2(3)-1)+&
                                                  A%f(A1(1)  ,A2(2)-1,A1(3)+1:A2(3)-1)+&
                                                  A%f(A1(1)+1,A2(2)-1,A1(3)+1:A2(3)-1))
         A%f(A2(1)+1,A2(3)+1,A1(2)+1:A2(2)-1) = -(A%f(A2(1)  ,A2(2)  ,A1(3)+1:A2(3)-1)+&
                                                  A%f(A2(1)-1,A2(2)  ,A1(3)+1:A2(3)-1)+&
                                                  A%f(A2(1)  ,A2(2)-1,A1(3)+1:A2(3)-1)+&
                                                  A%f(A2(1)-1,A2(2)-1,A1(3)+1:A2(3)-1))
         endif
       end subroutine

       subroutine internal_BC_RF(A,AB,iA,CC_along)
         implicit none
         type(realField),intent(inout) :: A
         type(overlap),dimension(3),intent(in) :: AB
         logical,dimension(3),intent(in) :: CC_along
         integer,intent(in) :: iA
         call internal_BC_RF_raw_face(A,(/AB(1)%R1(iA),AB(2)%R1(iA),AB(3)%R1(iA)/),&
                                        (/AB(1)%R2(iA),AB(2)%R2(iA),AB(3)%R2(iA)/),&
                                        CC_along)
       end subroutine

       ! *********************************************************************************
       ! ********************** CASE SPECIFIC EXTRACT ROUTINES ***************************
       ! *********************************************************************************

       subroutine internal_BC_Face_SF(Face_t,D)
         implicit none
         type(SF),intent(inout) :: Face_t
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_INTERNAL_BC_RF_
         if (.not.Face_t%is_Face) stop 'Error: Face data not found (2) in internal_BC_Face_SF in ops_internal_BC.f90'
#endif
         do i=1,D%s
         call IBC(Face_t%RF(D%sd(i)%g_tot_id),EE_shape_I(Face_t,D,i),1,Face_t%CC_along)
         enddo
       end subroutine

       subroutine internal_BC_Face_VF(Face_t,D)
         implicit none
         type(VF),intent(inout) :: Face_t
         type(domain),intent(in) :: D
         call internal_BC_Face(Face_t%x,D)
         call internal_BC_Face(Face_t%y,D)
         call internal_BC_Face(Face_t%z,D)
       end subroutine

       ! *********************************************************************************
       ! ****************************** INDEX DETAILS ************************************
       ! *********************************************************************************

       function EE_shape_I(f,D,i) result(s)
         implicit none
         type(SF),intent(in) :: f
         type(domain),intent(in) :: D
         integer,intent(in) :: i
         type(overlap),dimension(3) :: s
         if (f%is_Face) then
           select case (f%face)
           case (1); s = (/D%sd(i)%NB(1),D%sd(i)%CI(2),D%sd(i)%CI(3)/)
           case (2); s = (/D%sd(i)%CI(1),D%sd(i)%NB(2),D%sd(i)%CI(3)/)
           case (3); s = (/D%sd(i)%CI(1),D%sd(i)%CI(2),D%sd(i)%NB(3)/)
           case default; stop 'Error: f%face must = 1,2,3 in ops_embedExtract.f90'
           end select
         elseif (f%is_Edge) then
           select case (f%edge)
           case (1); s = (/D%sd(i)%CI(1),D%sd(i)%NB(2),D%sd(i)%NB(3)/)
           case (2); s = (/D%sd(i)%NB(1),D%sd(i)%CI(2),D%sd(i)%NB(3)/)
           case (3); s = (/D%sd(i)%NB(1),D%sd(i)%NB(2),D%sd(i)%CI(3)/)
           case default; stop 'Error: f%edge must = 1,2,3 in ops_embedExtract.f90'
           end select
         elseif (f%is_CC) then
           s = (/D%sd(i)%CI(1),D%sd(i)%CI(2),D%sd(i)%CI(3)/)
         elseif (f%is_Node) then
           s = (/D%sd(i)%NB(1),D%sd(i)%NB(2),D%sd(i)%NB(3)/)
         else; stop 'Error: no type found in ops_embedExtract.f90'
         endif
       end function

       end module