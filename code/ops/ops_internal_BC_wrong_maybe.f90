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
       use GF_mod
       use SF_mod
       use VF_mod

       implicit none

       private
       public :: internal_BC_Face

       interface internal_BC_Face;  module procedure internal_BC_Face_SF;        end interface
       interface internal_BC_Face;  module procedure internal_BC_Face_VF;        end interface

       interface IBC;               module procedure internal_BC_GF;             end interface

       contains

       subroutine internal_BC_GF_raw(A,B,A1,A2,B1,B2,p,shift,CC_along)
         implicit none
         type(grid_field),intent(inout) :: A
         type(grid_field),intent(in) :: B
         integer,dimension(3),intent(in) :: A1,A2,B1,B2,p,shift
         logical,dimension(3),intent(in) :: CC_along
#ifdef _PARALLELIZE_INTERNAL_BC_GF_
         integer :: i,j,k
         integer,dimension(3) :: suppress_warning
         suppress_warning = B2 ! B2 is not needed for parallel computations
         if (CC_along(1)) then
           !$OMP PARALLEL DO
           do k=A1(3)+1,A2(3)-1;do j=A1(2)+1,A2(2)-1;do i=A1(1),A1(1)+p(1) ! xmin
           A%f(i+shift(1),j,k) = -B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
           !$OMP PARALLEL DO
           do k=A1(3)+1,A2(3)-1;do j=A1(2)+1,A2(2)-1;do i=A2(1),A2(1)-p(1) ! xmax
           A%f(i-shift(1),j,k) = -B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         endif
         if (CC_along(2)) then
           !$OMP PARALLEL DO
           do k=A1(3)+1,A2(3)-1;do j=A1(2),A1(2)+p(2);do i=A1(1)+1,A2(1)-1 ! ymin
           A%f(i,j+shift(2),k) = -B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
           !$OMP PARALLEL DO
           do k=A1(3)+1,A2(3)-1;do j=A2(2),A2(2)-p(2);do i=A1(1)+1,A2(1)-1! ymax
           A%f(i,j-shift(2),k) = -B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         endif
         if (CC_along(3)) then
         !$OMP PARALLEL DO
         do k=A1(3),A1(3)+p(3);do j=A1(2)+1,A2(2)-1;do i=A1(1)+1,A2(1)-1 ! zmin
         A%f(i,j,k+shift(3)) = -B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=A2(3),A2(3)-p(3);do j=A1(2)+1,A2(2)-1;do i=A1(1)+1,A2(1)-1 ! zmax
         A%f(i,j,k-shift(3)) = -B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         endif
#else
         if (CC_along(1)) then
         A%f(A1(1)+shift(1):A1(1)+shift(1)+p(1),A1(2)+1:A2(2)-1,A1(3)+1:A2(3)-1) = &
        -B%f(B1(1)         :B1(1)         +p(1),B1(2)+1:B2(2)-1,B1(3)+1:B2(3)-1)
         A%f(A2(1)-shift(1):A2(1)-shift(1)-p(1),A1(2)+1:A2(2)-1,A1(3)+1:A2(3)-1) = &
        -B%f(B2(1)         :B2(1)         -p(1),B1(2)+1:B2(2)-1,B1(3)+1:B2(3)-1)
         endif
         if (CC_along(2)) then
         A%f(A1(1)+1:A2(1)-1,A1(2)+shift(2):A1(2)+shift(2)+p(2),A1(3)+1:A2(3)-1) = &
        -B%f(B1(1)+1:B2(1)-1,B1(2)         :B1(2)         +p(2),B1(3)+1:B2(3)-1)
         A%f(A1(1)+1:A2(1)-1,A2(2)-shift(2):A2(2)-shift(2)-p(2),A1(3)+1:A2(3)-1) = &
        -B%f(B1(1)+1:B2(1)-1,B2(2)         :B2(2)         -p(2),B1(3)+1:B2(3)-1)
         endif
         if (CC_along(3)) then
         A%f(A1(1)+1:A2(1)-1,A1(2)+1:A2(2)-1,A1(3)+shift(3):A1(3)+shift(3)+p(3)) = &
        -B%f(B1(1)+1:B2(1)-1,B1(2)+1:B2(2)-1,B1(3)         :B1(3)         +p(3))
         A%f(A1(1)+1:A2(1)-1,A1(2)+1:A2(2)-1,A2(3)-shift(3):A2(3)-shift(3)-p(3)) = &
        -B%f(B1(1)+1:B2(1)-1,B1(2)+1:B2(2)-1,B2(3)         :B2(3)         -p(3))
         endif
#endif
       end subroutine

       subroutine internal_BC_GF(A,B,AB,iA,iB,CC_along)
         implicit none
         type(grid_field),intent(inout) :: A
         type(grid_field),intent(in) :: B
         type(overlap),dimension(3),intent(in) :: AB
         logical,dimension(3),intent(in) :: CC_along
         integer,intent(in) :: iA,iB
         call internal_BC_GF_raw(A,B,(/AB(1)%R1(iA),AB(2)%R1(iA),AB(3)%R1(iA)/),&
                                     (/AB(1)%R2(iA),AB(2)%R2(iA),AB(3)%R2(iA)/),&
                                     (/AB(1)%R1(iB),AB(2)%R1(iB),AB(3)%R1(iB)/),&
                                     (/AB(1)%R2(iB),AB(2)%R2(iB),AB(3)%R2(iB)/),&
                                     (/2,2,2/),(/1,1,1/),CC_along)
       end subroutine

       ! *********************************************************************************
       ! ********************** CASE SPECIFIC EXTRACT ROUTINES ***************************
       ! *********************************************************************************

       subroutine internal_BC_Face_SF(Face_t,Face_i,D)
         implicit none
         type(SF),intent(inout) :: Face_t
         type(SF),intent(in) :: Face_i
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_INTERNAL_BC_GF_
         if (.not.Face_i%is_Face) stop 'Error: Face data not found (1) in internal_BC_Face_SF in ops_internal_BC.f90'
         if (.not.Face_t%is_Face) stop 'Error: Face data not found (2) in internal_BC_Face_SF in ops_internal_BC.f90'
#endif
         do i=1,D%s
         call IBC(Face_t%GF(D%sd(i)%g_tot_id),&
                  Face_i%GF(D%sd(i)%g_in_id),&
                  EE_shape_I(Face_t,D,i),1,2,(/Face_t%CC_along(1),&
                                               Face_t%CC_along(2),&
                                               Face_t%CC_along(3)/))
         enddo
       end subroutine

       subroutine internal_BC_Face_VF(Face_t,Face_i,D)
         implicit none
         type(VF),intent(inout) :: Face_t
         type(VF),intent(in) :: Face_i
         type(domain),intent(in) :: D
         call internal_BC_Face(Face_t%x,Face_i%x,D)
         call internal_BC_Face(Face_t%y,Face_i%y,D)
         call internal_BC_Face(Face_t%z,Face_i%z,D)
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