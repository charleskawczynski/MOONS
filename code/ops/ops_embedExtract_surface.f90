       module ops_embedExtract_surface_mod
       ! Not all embed / extract routines are used for each method. For example,
       ! the CT method only uses embedEdge, and not embedCC or embedFace.
       ! 
       ! Pre-processor directives: (_PARALLELIZE_EMBEDEXTRACT_SUGFACE_,_DEBUG_EMBEDEXTRACT_)

       use current_precision_mod
       use overlap_mod
       use mesh_mod
       use domain_mod
       use GF_mod
       use SF_mod
       use VF_mod

       implicit none

       private
       public :: extract_surface,extractCC_surface,extractFace_surface,extractEdge_surface
       public :: embed_surface,embedCC_surface,embedFace_surface,embedEdge_surface

       interface extract_surface;        module procedure extract_SF;                 end interface
       interface extract_surface;        module procedure extract_VF;                 end interface
       interface extractCC_surface;      module procedure extractCC_SF;               end interface
       interface extractCC_surface;      module procedure extractCC_VF;               end interface
       interface extractFace_surface;    module procedure extractFace_SF;             end interface
       interface extractFace_surface;    module procedure extractFace_VF;             end interface
       interface extractEdge_surface;    module procedure extractEdge_SF;             end interface
       interface extractEdge_surface;    module procedure extractEdge_VF;             end interface

       interface embed_surface;          module procedure embed_SF;                   end interface
       interface embed_surface;          module procedure embed_VF;                   end interface
       interface embedCC_surface;        module procedure embedCC_SF;                 end interface
       interface embedCC_surface;        module procedure embedCC_VF;                 end interface
       interface embedFace_surface;      module procedure embedFace_SF;               end interface
       interface embedFace_surface;      module procedure embedFace_VF;               end interface
       interface embedEdge_surface;      module procedure embedEdge_SF;               end interface
       interface embedEdge_surface;      module procedure embedEdge_VF;               end interface

       interface EE;                     module procedure embed_extract_surface_GF;   end interface

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ***************************** LOW-LEVEL ROUTINE *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine embedExtract_GF_raw_p_cells(A,B,A1,A2,B1,B2,p)
         ! This is the embed/extract (EE) routine.
         implicit none
         type(grid_field),intent(inout) :: A
         type(grid_field),intent(in) :: B
         integer,dimension(3),intent(in) :: A1,A2,B1,B2,p
#ifdef _PARALLELIZE_EMBEDEXTRACT_SUGFACE_
         integer :: i,j,k
         integer,dimension(3) :: suppress_warning
         suppress_warning = B2 ! B2 is not needed for parallel computations
         !$OMP PARALLEL DO
         do k=A1(3),A2(3);do j=A1(2),A2(2);do i=A1(1),A1(1)+p(1) ! xmin
         A%f(i,j,k) = B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=A1(3),A2(3);do j=A1(2),A2(2);do i=A2(1),A2(1)-p(1) ! xmax
         A%f(i,j,k) = B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=A1(3),A2(3);do j=A1(2),A1(2)+p(2);do i=A1(1),A2(1) ! ymin
         A%f(i,j,k) = B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=A1(3),A2(3);do j=A2(2),A2(2)-p(2);do i=A1(1),A2(1)! ymax
         A%f(i,j,k) = B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=A1(3),A1(3)+p(3);do j=A1(2),A2(2);do i=A1(1),A2(1) ! zmin
         A%f(i,j,k) = B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=A2(3),A2(3)-p(3);do j=A1(2),A2(2);do i=A1(1),A2(1) ! zmax
         A%f(i,j,k) = B%f(B1(1)+(i-A1(1)),B1(2)+(j-A1(2)),B1(3)+(k-A1(3)))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
#else
         A%f(A1(1):A1(1)+p(1),A1(2):A2(2),A1(3):A2(3)) = &
         B%f(B1(1):B1(1)+p(1),B1(2):B2(2),B1(3):B2(3))
         A%f(A2(1):A2(1)-p(1),A1(2):A2(2),A1(3):A2(3)) = &
         B%f(B2(1):B2(1)-p(1),B1(2):B2(2),B1(3):B2(3))
         A%f(A1(1):A2(1),A1(2):A1(2)+p(2),A1(3):A2(3)) = &
         B%f(B1(1):B2(1),B1(2):B1(2)+p(2),B1(3):B2(3))
         A%f(A1(1):A2(1),A2(2):A2(2)-p(2),A1(3):A2(3)) = &
         B%f(B1(1):B2(1),B2(2):B2(2)-p(2),B1(3):B2(3))
         A%f(A1(1):A2(1),A1(2):A2(2),A1(3):A1(3)+p(3)) = &
         B%f(B1(1):B2(1),B1(2):B2(2),B1(3):B1(3)+p(3))
         A%f(A1(1):A2(1),A1(2):A2(2),A2(3):A2(3)-p(3)) = &
         B%f(B1(1):B2(1),B1(2):B2(2),B2(3):B2(3)-p(3))
#endif
       end subroutine


       subroutine embed_extract_surface_GF(A,B,AB,iA,iB)
         implicit none
         type(grid_field),intent(inout) :: A
         type(grid_field),intent(in) :: B
         type(overlap),dimension(3),intent(in) :: AB
         integer,intent(in) :: iA,iB
         call embedExtract_GF_raw_p_cells(A,B,(/AB(1)%R1(iA),AB(2)%R1(iA),AB(3)%R1(iA)/),&
                                              (/AB(1)%R2(iA),AB(2)%R2(iA),AB(3)%R2(iA)/),&
                                              (/AB(1)%R1(iB),AB(2)%R1(iB),AB(3)%R1(iB)/),&
                                              (/AB(1)%R2(iB),AB(2)%R2(iB),AB(3)%R2(iB)/),&
                                              (/2,2,2/))
       end subroutine

       ! *********************************************************************************
       ! ********************** CASE SPECIFIC EXTRACT ROUTINES ***************************
       ! *********************************************************************************

       subroutine extract_SF(interior,total,D)
         implicit none
         type(SF),intent(inout) :: interior
         type(SF),intent(in) :: total
         type(domain),intent(in) :: D
         if (total%is_CC) then;       call extractCC_surface(interior,total,D)
         elseif (total%is_Node) then; ! call extractNode_surface(interior,total,D)
         stop 'Error: N not supported in extract_SF in ops_embedExtract.f90'
         elseif (total%is_Face) then; call extractFace_surface(interior,total,D)
         elseif (total%is_Edge) then; call extractEdge_surface(interior,total,D)
         else; stop 'Error: bad data input to extract_F in embedExtract.f90'
         endif
       end subroutine
       subroutine extract_VF(interior,total,D)
         implicit none
         type(VF),intent(inout) :: interior
         type(VF),intent(in) :: total
         type(domain),intent(in) :: D
         call extract_surface(interior%x,total%x,D)
         call extract_surface(interior%y,total%y,D)
         call extract_surface(interior%z,total%z,D)
       end subroutine

       subroutine extractCC_SF(CC_i,CC_t,D)
         implicit none
         type(SF),intent(inout) :: CC_i
         type(SF),intent(in) :: CC_t
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.CC_i%is_CC) stop 'Error: CC data not found (1) in extractCC_SF in ops_embedExtract.f90'
         if (.not.CC_t%is_CC) stop 'Error: CC data not found (2) in extractCC_SF in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(CC_i%GF(D%sd(i)%g_in_id),CC_t%GF(D%sd(i)%g_tot_id),EE_shape(CC_i,D,i),2,1)
         enddo
       end subroutine
       subroutine extractCC_VF(CC_i,CC_t,D)
         implicit none
         type(VF),intent(inout) :: CC_i
         type(VF),intent(in) :: CC_t
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.CC_i%is_CC) stop 'Error: CC data not found (1) in extractCC_VF in ops_embedExtract.f90'
         if (.not.CC_t%is_CC) stop 'Error: CC data not found (2) in extractCC_VF in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(CC_i%x%GF(D%sd(i)%g_in_id),CC_t%x%GF(D%sd(i)%g_tot_id),EE_shape(CC_i%x,D,i),2,1)
         call EE(CC_i%y%GF(D%sd(i)%g_in_id),CC_t%y%GF(D%sd(i)%g_tot_id),EE_shape(CC_i%y,D,i),2,1)
         call EE(CC_i%z%GF(D%sd(i)%g_in_id),CC_t%z%GF(D%sd(i)%g_tot_id),EE_shape(CC_i%z,D,i),2,1)
         enddo
       end subroutine

       subroutine extractFace_SF(face_i,face_t,D) ! Extracts Lorentz force from induction to momentum
         implicit none
         type(SF),intent(inout) :: face_i
         type(SF),intent(in) :: face_t
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.face_t%is_Face) stop 'Error: face data not found (1) in extractFace_SF in ops_embedExtract.f90'
         if (.not.face_i%is_Face) stop 'Error: face data not found (2) in extractFace_SF in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(face_i%GF(D%sd(i)%g_in_id),face_t%GF(D%sd(i)%g_tot_id),EE_shape(face_i,D,i),2,1)
         enddo
       end subroutine
       subroutine extractFace_VF(face_i,face_t,D) ! Extracts Lorentz force from induction to momentum
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.face_t%is_Face) stop 'Error: face data not found (1) in extractFace_VF in ops_embedExtract.f90'
         if (.not.face_i%is_Face) stop 'Error: face data not found (2) in extractFace_VF in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(face_i%x%GF(D%sd(i)%g_in_id),face_t%x%GF(D%sd(i)%g_tot_id),EE_shape(face_i%x,D,i),2,1)
         call EE(face_i%y%GF(D%sd(i)%g_in_id),face_t%y%GF(D%sd(i)%g_tot_id),EE_shape(face_i%y,D,i),2,1)
         call EE(face_i%z%GF(D%sd(i)%g_in_id),face_t%z%GF(D%sd(i)%g_tot_id),EE_shape(face_i%z,D,i),2,1)
         enddo
       end subroutine

       subroutine extractEdge_SF(edge_i,edge_t,D) ! Auxiliary (energy budget)
         implicit none
         type(SF),intent(inout) :: edge_i
         type(SF),intent(in) :: edge_t
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.edge_i%is_Edge) stop 'Error: edge data not found (1) in extractEdge_SF in ops_embedExtract.f90'
         if (.not.edge_t%is_Edge) stop 'Error: edge data not found (2) in extractEdge_SF in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(edge_i%GF(D%sd(i)%g_in_id),edge_t%GF(D%sd(i)%g_tot_id),EE_shape(edge_i,D,i),2,1)
         enddo
       end subroutine
       subroutine extractEdge_VF(edge_i,edge_t,D) ! Auxiliary (energy budget)
         implicit none
         type(VF),intent(inout) :: edge_i
         type(VF),intent(in) :: edge_t
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.edge_i%is_Edge) stop 'Error: edge data not found (1) in extractEdge_VF in ops_embedExtract.f90'
         if (.not.edge_t%is_Edge) stop 'Error: edge data not found (2) in extractEdge_VF in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(edge_i%x%GF(D%sd(i)%g_in_id),edge_t%x%GF(D%sd(i)%g_tot_id),EE_shape(edge_i%x,D,i),2,1)
         call EE(edge_i%y%GF(D%sd(i)%g_in_id),edge_t%y%GF(D%sd(i)%g_tot_id),EE_shape(edge_i%y,D,i),2,1)
         call EE(edge_i%z%GF(D%sd(i)%g_in_id),edge_t%z%GF(D%sd(i)%g_tot_id),EE_shape(edge_i%z,D,i),2,1)
         enddo
       end subroutine

       ! *********************************************************************************
       ! ************************ CASE SPECIFIC EMBED ROUTINES ***************************
       ! *********************************************************************************

       subroutine embed_SF(total,interior,D)
         implicit none
         type(SF),intent(inout) :: total
         type(SF),intent(in) :: interior
         type(domain),intent(in) :: D
         if (total%is_CC) then;       call embedCC_surface(total,interior,D)
         elseif (total%is_Node) then; ! call embedNode_surface(total,interior,D)
         stop 'Error: N not supported in embed_SF in ops_embedExtract.f90'
         elseif (total%is_Face) then; call embedFace_surface(total,interior,D)
         elseif (total%is_Edge) then; call embedEdge_surface(total,interior,D)
         else; stop 'Error: bad data input to extract_F in embedExtract.f90'
         endif
       end subroutine
       subroutine embed_VF(total,interior,D)
         implicit none
         type(VF),intent(inout) :: total
         type(VF),intent(in) :: interior
         type(domain),intent(in) :: D
         call embed_surface(total%x,interior%x,D)
         call embed_surface(total%y,interior%y,D)
         call embed_surface(total%z,interior%z,D)
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
         call EE(CC_t%GF(D%sd(i)%g_tot_id),CC_i%GF(D%sd(i)%g_in_id),EE_shape(CC_t,D,i),1,2)
         enddo
       end subroutine
       subroutine embedCC_VF(CC_t,CC_i,D)
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
         call EE(CC_t%x%GF(D%sd(i)%g_tot_id),CC_i%x%GF(D%sd(i)%g_in_id),EE_shape(CC_t%x,D,i),1,2)
         call EE(CC_t%y%GF(D%sd(i)%g_tot_id),CC_i%y%GF(D%sd(i)%g_in_id),EE_shape(CC_t%y,D,i),1,2)
         call EE(CC_t%z%GF(D%sd(i)%g_tot_id),CC_i%z%GF(D%sd(i)%g_in_id),EE_shape(CC_t%z,D,i),1,2)
         enddo
       end subroutine

       subroutine embedFace_SF(Face_t,Face_i,D)
         implicit none
         type(SF),intent(inout) :: Face_t
         type(SF),intent(in) :: Face_i
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.Face_i%is_Face) stop 'Error: Face data not found (1) in embedFace_SF in ops_embedExtract.f90'
         if (.not.Face_t%is_Face) stop 'Error: Face data not found (2) in embedFace_SF in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(Face_t%GF(D%sd(i)%g_tot_id),Face_i%GF(D%sd(i)%g_in_id),EE_shape(Face_t,D,i),1,2)
         enddo
       end subroutine
       subroutine embedFace_VF(Face_t,Face_i,D)
         implicit none
         type(VF),intent(inout) :: Face_t
         type(VF),intent(in) :: Face_i
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.Face_i%is_Face) stop 'Error: Face data not found (1) in embedFace_VF in ops_embedExtract.f90'
         if (.not.Face_t%is_Face) stop 'Error: Face data not found (2) in embedFace_VF in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(Face_t%x%GF(D%sd(i)%g_tot_id),Face_i%x%GF(D%sd(i)%g_in_id),EE_shape(Face_t%x,D,i),1,2)
         call EE(Face_t%y%GF(D%sd(i)%g_tot_id),Face_i%y%GF(D%sd(i)%g_in_id),EE_shape(Face_t%y,D,i),1,2)
         call EE(Face_t%z%GF(D%sd(i)%g_tot_id),Face_i%z%GF(D%sd(i)%g_in_id),EE_shape(Face_t%z,D,i),1,2)
         enddo
       end subroutine

       subroutine embedEdge_SF(Edge_t,Edge_i,D) ! Embeds velocity from momentum into induction
         implicit none
         type(SF),intent(inout) :: Edge_t
         type(SF),intent(in) :: Edge_i
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.edge_t%is_Edge) stop 'Error: edge data not found (1) in embedEdge_SF in ops_embedExtract.f90'
         if (.not.edge_i%is_Edge) stop 'Error: edge data not found (2) in embedEdge_SF in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(Edge_t%GF(D%sd(i)%g_tot_id),Edge_i%GF(D%sd(i)%g_in_id),EE_shape(Edge_t,D,i),1,2)
         enddo
       end subroutine
       subroutine embedEdge_VF(Edge_t,Edge_i,D) ! Embeds velocity from momentum into induction
         implicit none
         type(VF),intent(inout) :: Edge_t
         type(VF),intent(in) :: Edge_i
         type(domain),intent(in) :: D
         integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
         if (.not.edge_t%is_Edge) stop 'Error: edge data not found (1) in embedEdge_VF in ops_embedExtract.f90'
         if (.not.edge_i%is_Edge) stop 'Error: edge data not found (2) in embedEdge_VF in ops_embedExtract.f90'
#endif
         do i=1,D%s
         call EE(Edge_t%x%GF(D%sd(i)%g_tot_id),Edge_i%x%GF(D%sd(i)%g_in_id),EE_shape(Edge_t%x,D,i),1,2)
         call EE(Edge_t%y%GF(D%sd(i)%g_tot_id),Edge_i%y%GF(D%sd(i)%g_in_id),EE_shape(Edge_t%y,D,i),1,2)
         call EE(Edge_t%z%GF(D%sd(i)%g_tot_id),Edge_i%z%GF(D%sd(i)%g_in_id),EE_shape(Edge_t%z,D,i),1,2)
         enddo
       end subroutine

       ! *********************************************************************************
       ! ****************************** INDEX DETAILS ************************************
       ! *********************************************************************************

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