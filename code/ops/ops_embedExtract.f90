       module ops_embedExtract_mod
       ! Not all embed / extract routines are used for each method. For example,
       ! the CT method only uses embedEdge, and not embedCC or embedFace.
       ! 
       ! Pre-processor directives: (_PARALLELIZE_EMBEDEXTRACT_,_DEBUG_EMBEDEXTRACT_)

       use current_precision_mod
       use overlap_mod
       use mesh_mod
       use domain_mod
       use RF_mod
       use SF_mod
       use VF_mod

       implicit none

       private
       public :: extractFace,extractEdge,extractCC
       public :: embedFace
       public :: embedEdge
       public :: embedCC

       public :: embed_N_surface
       public :: extract_N_surface

       public :: embed_F_surface
       public :: extract_F_surface

       interface embedCC;    module procedure embedCC_SF;        end interface
       interface embedCC;    module procedure embedCC_VF;        end interface
       interface EE;         module procedure embedExtract_RF;   end interface

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ***************************** LOW-LEVEL ROUTINE *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine embedExtract_RF_raw(A,B,A1,A2,B1,B2)
         ! This is the embed/extract (EE) routine.
         implicit none
         type(realField),intent(inout) :: A
         type(realField),intent(in) :: B
         integer,dimension(3),intent(in) :: A1,A2,B1,B2
#ifdef _PARALLELIZE_EMBEDEXTRACT_
         integer :: i,j,k
         integer,dimension(3) :: suppress_warning
         suppress_warning = B2 ! B2 is not needed for parallel computations
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

       subroutine embedExtract_RF(A,B,AB,iA,iB)
         ! This is the embed/extract (EE) routine.
         implicit none
         type(realField),intent(inout) :: A
         type(realField),intent(in) :: B
         type(overlap),dimension(3),intent(in) :: AB
         integer,intent(in) :: iA,iB
         call embedExtract_RF_raw(A,B,(/AB(1)%R1(iA),AB(2)%R1(iA),AB(3)%R1(iA)/),&
                                      (/AB(1)%R2(iA),AB(2)%R2(iA),AB(3)%R2(iA)/),&
                                      (/AB(1)%R1(iB),AB(2)%R1(iB),AB(3)%R1(iB)/),&
                                      (/AB(1)%R2(iB),AB(2)%R2(iB),AB(3)%R2(iB)/))
       end subroutine

       ! *********************************************************************************
       ! *************************** CASE SPECIFIC ROUTINES ******************************
       ! *********************************************************************************

       subroutine extractFace(face_i,face_t,D) ! Extracts Lorentz force from induction to momentum
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.face_t%is_Face) stop 'Error: face data not found (1) in extractFace in ops_embedExtract.f90'
         if (.not.face_i%is_Face) stop 'Error: face data not found (2) in extractFace in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(face_i%x%RF(D%sd(i)%g_in_id),face_t%x%RF(D%sd(i)%g_tot_id),EE_shape(face_i%x,D,i),2,1)
         call EE(face_i%y%RF(D%sd(i)%g_in_id),face_t%y%RF(D%sd(i)%g_tot_id),EE_shape(face_i%y,D,i),2,1)
         call EE(face_i%z%RF(D%sd(i)%g_in_id),face_t%z%RF(D%sd(i)%g_tot_id),EE_shape(face_i%z,D,i),2,1)
         enddo
       end subroutine

       subroutine extractEdge(edge_i,edge_t,D) ! Auxiliary (energy budget)
         implicit none
         type(VF),intent(inout) :: edge_i
         type(VF),intent(in) :: edge_t
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.edge_i%is_Edge) stop 'Error: edge data not found (1) in extractEdge in ops_embedExtract.f90'
         if (.not.edge_t%is_Edge) stop 'Error: edge data not found (2) in extractEdge in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(edge_i%x%RF(D%sd(i)%g_in_id),edge_t%x%RF(D%sd(i)%g_tot_id),EE_shape(edge_i%x,D,i),2,1)
         call EE(edge_i%y%RF(D%sd(i)%g_in_id),edge_t%y%RF(D%sd(i)%g_tot_id),EE_shape(edge_i%y,D,i),2,1)
         call EE(edge_i%z%RF(D%sd(i)%g_in_id),edge_t%z%RF(D%sd(i)%g_tot_id),EE_shape(edge_i%z,D,i),2,1)
         enddo
       end subroutine

       subroutine embedEdge(Edge_t,Edge_i,D) ! Embeds velocity from momentum into induction
         implicit none
         type(VF),intent(inout) :: Edge_t
         type(VF),intent(in) :: Edge_i
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.edge_t%is_Edge) stop 'Error: edge data not found (1) in embedEdge in ops_embedExtract.f90'
         if (.not.edge_i%is_Edge) stop 'Error: edge data not found (2) in embedEdge in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(Edge_t%x%RF(D%sd(i)%g_tot_id),Edge_i%x%RF(D%sd(i)%g_in_id),EE_shape(Edge_t%x,D,i),1,2)
         call EE(Edge_t%y%RF(D%sd(i)%g_tot_id),Edge_i%y%RF(D%sd(i)%g_in_id),EE_shape(Edge_t%y,D,i),1,2)
         call EE(Edge_t%z%RF(D%sd(i)%g_tot_id),Edge_i%z%RF(D%sd(i)%g_in_id),EE_shape(Edge_t%z,D,i),1,2)
         enddo
       end subroutine

       subroutine embedCC_SF(CC_t,CC_i,D) ! For material properties
         implicit none
         type(SF),intent(inout) :: CC_t
         type(SF),intent(in) :: CC_i
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.CC_i%is_CC) stop 'Error: CC data not found (1) in embedCC_SF in ops_embedExtract.f90'
         if (.not.CC_t%is_CC) stop 'Error: CC data not found (2) in embedCC_SF in ops_embedExtract.f90'
#endif
         do i=1,D%s
           call EE(CC_t%RF(D%sd(i)%g_tot_id),CC_i%RF(D%sd(i)%g_in_id),EE_shape(CC_t,D,i),1,2)
         enddo
       end subroutine

       subroutine embedFace(Face_t,Face_i,D)
         implicit none
         type(VF),intent(inout) :: Face_t
         type(VF),intent(in) :: Face_i
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.Face_i%is_Face) stop 'Error: Face data not found (1) in embedFace in ops_embedExtract.f90'
         if (.not.Face_t%is_Face) stop 'Error: Face data not found (2) in embedFace in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(Face_t%x%RF(D%sd(i)%g_tot_id),Face_i%x%RF(D%sd(i)%g_in_id),EE_shape(Face_t%x,D,i),1,2)
         call EE(Face_t%y%RF(D%sd(i)%g_tot_id),Face_i%y%RF(D%sd(i)%g_in_id),EE_shape(Face_t%y,D,i),1,2)
         call EE(Face_t%z%RF(D%sd(i)%g_tot_id),Face_i%z%RF(D%sd(i)%g_in_id),EE_shape(Face_t%z,D,i),1,2)
         enddo
       end subroutine

       subroutine extractCC(CC_i,CC_t,D)
         ! Including ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: CC_i
         type(VF),intent(in) :: CC_t
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.CC_i%is_CC) stop 'Error: CC data not found (1) in embedCC in ops_embedExtract.f90'
         if (.not.CC_t%is_CC) stop 'Error: CC data not found (2) in embedCC in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(CC_i%x%RF(D%sd(i)%g_in_id),CC_t%x%RF(D%sd(i)%g_tot_id),EE_shape(CC_i%x,D,i),2,1)
         call EE(CC_i%y%RF(D%sd(i)%g_in_id),CC_t%y%RF(D%sd(i)%g_tot_id),EE_shape(CC_i%y,D,i),2,1)
         call EE(CC_i%z%RF(D%sd(i)%g_in_id),CC_t%z%RF(D%sd(i)%g_tot_id),EE_shape(CC_i%z,D,i),2,1)
         enddo
       end subroutine

       subroutine embedCC_VF(CC_t,CC_i,D)
         ! Including ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: CC_t
         type(VF),intent(in) :: CC_i
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.CC_i%is_CC) stop 'Error: CC data not found (1) in embedCC_VF in ops_embedExtract.f90'
         if (.not.CC_t%is_CC) stop 'Error: CC data not found (2) in embedCC_VF in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(CC_t%x%RF(D%sd(i)%g_tot_id),CC_i%x%RF(D%sd(i)%g_in_id),EE_shape(CC_t%x,D,i),1,2)
         call EE(CC_t%y%RF(D%sd(i)%g_tot_id),CC_i%y%RF(D%sd(i)%g_in_id),EE_shape(CC_t%y,D,i),1,2)
         call EE(CC_t%z%RF(D%sd(i)%g_tot_id),CC_i%z%RF(D%sd(i)%g_in_id),EE_shape(CC_t%z,D,i),1,2)
         enddo
       end subroutine

       subroutine embed_N_surface(N_t,N_i,D) ! For surface mesh testing
         implicit none
         type(SF),intent(inout) :: N_t
         type(SF),intent(in) :: N_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(N_t%RF(D%sd(i)%g_tot_id),N_i%RF(D%sd(i)%g_in_id),EE_shape(N_t,D,i),1,2)
         enddo
       end subroutine

       subroutine extract_N_surface(N_i,N_t,D) ! For surface mesh testing
         implicit none
         type(SF),intent(inout) :: N_i
         type(SF),intent(in) :: N_t
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(N_i%RF(D%sd(i)%g_in_id),N_t%RF(D%sd(i)%g_tot_id),EE_shape(N_i,D,i),2,1)
         enddo
       end subroutine

       subroutine embed_F_surface(Face_t,Face_i,D)
         implicit none
         type(VF),intent(inout) :: Face_t
         type(VF),intent(in) :: Face_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
         call EE(Face_t%x%RF(D%sd(i)%g_tot_id),Face_i%x%RF(D%sd(i)%g_in_id),EE_shape(Face_t%x,D,i),1,2)
         call EE(Face_t%y%RF(D%sd(i)%g_tot_id),Face_i%y%RF(D%sd(i)%g_in_id),EE_shape(Face_t%y,D,i),1,2)
         call EE(Face_t%z%RF(D%sd(i)%g_tot_id),Face_i%z%RF(D%sd(i)%g_in_id),EE_shape(Face_t%z,D,i),1,2)
         enddo
       end subroutine

       subroutine extract_F_surface(face_i,face_t,D)
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
         call EE(face_i%x%RF(D%sd(i)%g_in_id),face_t%x%RF(D%sd(i)%g_tot_id),EE_shape(face_i%x,D,i),2,1)
         call EE(face_i%y%RF(D%sd(i)%g_in_id),face_t%y%RF(D%sd(i)%g_tot_id),EE_shape(face_i%y,D,i),2,1)
         call EE(face_i%z%RF(D%sd(i)%g_in_id),face_t%z%RF(D%sd(i)%g_tot_id),EE_shape(face_i%z,D,i),2,1)
         enddo
       end subroutine

       function EE_shape(f,D,i) result(s)
         implicit none
         type(SF),intent(in) :: f
         type(domain),intent(in) :: D
         integer,intent(in) :: i
         type(overlap),dimension(3) :: s
         if (f%is_Face) then
           select case (f%face)
           case (1); s = (/D%sd(i)%NB(1),D%sd(i)%CE(2),D%sd(i)%CE(3)/)
           case (2); s = (/D%sd(i)%CE(1),D%sd(i)%NB(2),D%sd(i)%CE(3)/)
           case (3); s = (/D%sd(i)%CE(1),D%sd(i)%CE(2),D%sd(i)%NB(3)/)
           case default; stop 'Error: f%face must = 1,2,3 in ops_embedExtract.f90'
           end select
         elseif (f%is_Edge) then
           select case (f%edge)
           case (1); s = (/D%sd(i)%CE(1),D%sd(i)%NB(2),D%sd(i)%NB(3)/)
           case (2); s = (/D%sd(i)%NB(1),D%sd(i)%CE(2),D%sd(i)%NB(3)/)
           case (3); s = (/D%sd(i)%NB(1),D%sd(i)%NB(2),D%sd(i)%CE(3)/)
           case default; stop 'Error: f%edge must = 1,2,3 in ops_embedExtract.f90'
           end select
         elseif (f%is_CC) then
           s = (/D%sd(i)%CE(1),D%sd(i)%CE(2),D%sd(i)%CE(3)/)
         elseif (f%is_Node) then
           s = (/D%sd(i)%NB(1),D%sd(i)%NB(2),D%sd(i)%NB(3)/)
         else; stop 'Error: no type found in ops_embedExtract.f90'
         endif
       end function

       end module