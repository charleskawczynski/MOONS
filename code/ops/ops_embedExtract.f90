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
       public :: extract,extractCC,extractFace,extractEdge
       public :: embed,embedCC,embedFace,embedEdge

       public :: embed_N_surface
       public :: extract_N_surface
       public :: embed_F_surface
       public :: extract_F_surface

       interface extract;        module procedure extract_SF;        end interface
       interface extract;        module procedure extract_VF;        end interface
       interface extractCC;      module procedure extractCC_SF;      end interface
       interface extractCC;      module procedure extractCC_VF;      end interface
       interface extractFace;    module procedure extractFace_SF;    end interface
       interface extractFace;    module procedure extractFace_VF;    end interface
       interface extractEdge;    module procedure extractEdge_SF;    end interface
       interface extractEdge;    module procedure extractEdge_VF;    end interface

       interface embed;          module procedure embed_SF;          end interface
       interface embed;          module procedure embed_VF;          end interface
       interface embedCC;        module procedure embedCC_SF;        end interface
       interface embedCC;        module procedure embedCC_VF;        end interface
       interface embedFace;      module procedure embedFace_SF;      end interface
       interface embedFace;      module procedure embedFace_VF;      end interface
       interface embedEdge;      module procedure embedEdge_SF;      end interface
       interface embedEdge;      module procedure embedEdge_VF;      end interface

       interface EE;             module procedure embedExtract_RF;   end interface

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
       ! ********************** CASE SPECIFIC EXTRACT ROUTINES ***************************
       ! *********************************************************************************

       subroutine extract_SF(interior,total,D)
         implicit none
         type(SF),intent(inout) :: interior
         type(SF),intent(in) :: total
         type(domain),intent(in) :: D
         if (total%is_CC) then;       call extractCC(interior,total,D)
         elseif (total%is_Node) then; ! call extractNode(interior,total,D)
         stop 'Error: N not supported in extract_SF in ops_embedExtract.f90'
         elseif (total%is_Face) then; call extractFace(interior,total,D)
         elseif (total%is_Edge) then; call extractEdge(interior,total,D)
         else; stop 'Error: bad data input to extract_F in embedExtract.f90'
         endif
       end subroutine
       subroutine extract_VF(interior,total,D)
         implicit none
         type(VF),intent(inout) :: interior
         type(VF),intent(in) :: total
         type(domain),intent(in) :: D
         call extract(interior%x,total%x,D)
         call extract(interior%y,total%y,D)
         call extract(interior%z,total%z,D)
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
         call EE(CC_i%RF(D%sd(i)%g_in_id),CC_t%RF(D%sd(i)%g_tot_id),EE_shape(CC_i,D,i),2,1)
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
         call EE(CC_i%x%RF(D%sd(i)%g_in_id),CC_t%x%RF(D%sd(i)%g_tot_id),EE_shape(CC_i%x,D,i),2,1)
         call EE(CC_i%y%RF(D%sd(i)%g_in_id),CC_t%y%RF(D%sd(i)%g_tot_id),EE_shape(CC_i%y,D,i),2,1)
         call EE(CC_i%z%RF(D%sd(i)%g_in_id),CC_t%z%RF(D%sd(i)%g_tot_id),EE_shape(CC_i%z,D,i),2,1)
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
         call EE(face_i%RF(D%sd(i)%g_in_id),face_t%RF(D%sd(i)%g_tot_id),EE_shape(face_i,D,i),2,1)
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
         call EE(face_i%x%RF(D%sd(i)%g_in_id),face_t%x%RF(D%sd(i)%g_tot_id),EE_shape(face_i%x,D,i),2,1)
         call EE(face_i%y%RF(D%sd(i)%g_in_id),face_t%y%RF(D%sd(i)%g_tot_id),EE_shape(face_i%y,D,i),2,1)
         call EE(face_i%z%RF(D%sd(i)%g_in_id),face_t%z%RF(D%sd(i)%g_tot_id),EE_shape(face_i%z,D,i),2,1)
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
         call EE(edge_i%RF(D%sd(i)%g_in_id),edge_t%RF(D%sd(i)%g_tot_id),EE_shape(edge_i,D,i),2,1)
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
         call EE(edge_i%x%RF(D%sd(i)%g_in_id),edge_t%x%RF(D%sd(i)%g_tot_id),EE_shape(edge_i%x,D,i),2,1)
         call EE(edge_i%y%RF(D%sd(i)%g_in_id),edge_t%y%RF(D%sd(i)%g_tot_id),EE_shape(edge_i%y,D,i),2,1)
         call EE(edge_i%z%RF(D%sd(i)%g_in_id),edge_t%z%RF(D%sd(i)%g_tot_id),EE_shape(edge_i%z,D,i),2,1)
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
         if (total%is_CC) then;       call embedCC(total,interior,D)
         elseif (total%is_Node) then; ! call embedNode(total,interior,D)
         stop 'Error: N not supported in embed_SF in ops_embedExtract.f90'
         elseif (total%is_Face) then; call embedFace(total,interior,D)
         elseif (total%is_Edge) then; call embedEdge(total,interior,D)
         else; stop 'Error: bad data input to extract_F in embedExtract.f90'
         endif
       end subroutine
       subroutine embed_VF(total,interior,D)
         implicit none
         type(VF),intent(inout) :: total
         type(VF),intent(in) :: interior
         type(domain),intent(in) :: D
         call embed(total%x,interior%x,D)
         call embed(total%y,interior%y,D)
         call embed(total%z,interior%z,D)
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
         call EE(CC_t%x%RF(D%sd(i)%g_tot_id),CC_i%x%RF(D%sd(i)%g_in_id),EE_shape(CC_t%x,D,i),1,2)
         call EE(CC_t%y%RF(D%sd(i)%g_tot_id),CC_i%y%RF(D%sd(i)%g_in_id),EE_shape(CC_t%y,D,i),1,2)
         call EE(CC_t%z%RF(D%sd(i)%g_tot_id),CC_i%z%RF(D%sd(i)%g_in_id),EE_shape(CC_t%z,D,i),1,2)
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
         call EE(Face_t%RF(D%sd(i)%g_tot_id),Face_i%RF(D%sd(i)%g_in_id),EE_shape(Face_t,D,i),1,2)
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
         call EE(Face_t%x%RF(D%sd(i)%g_tot_id),Face_i%x%RF(D%sd(i)%g_in_id),EE_shape(Face_t%x,D,i),1,2)
         call EE(Face_t%y%RF(D%sd(i)%g_tot_id),Face_i%y%RF(D%sd(i)%g_in_id),EE_shape(Face_t%y,D,i),1,2)
         call EE(Face_t%z%RF(D%sd(i)%g_tot_id),Face_i%z%RF(D%sd(i)%g_in_id),EE_shape(Face_t%z,D,i),1,2)
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
         call EE(Edge_t%RF(D%sd(i)%g_tot_id),Edge_i%RF(D%sd(i)%g_in_id),EE_shape(Edge_t,D,i),1,2)
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
         call EE(Edge_t%x%RF(D%sd(i)%g_tot_id),Edge_i%x%RF(D%sd(i)%g_in_id),EE_shape(Edge_t%x,D,i),1,2)
         call EE(Edge_t%y%RF(D%sd(i)%g_tot_id),Edge_i%y%RF(D%sd(i)%g_in_id),EE_shape(Edge_t%y,D,i),1,2)
         call EE(Edge_t%z%RF(D%sd(i)%g_tot_id),Edge_i%z%RF(D%sd(i)%g_in_id),EE_shape(Edge_t%z,D,i),1,2)
         enddo
       end subroutine

       ! *********************************************************************************
       ! ********************** CASE SPECIFIC SURFACE ROUTINES ***************************
       ! *********************************************************************************

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

       ! *********************************************************************************
       ! ****************************** INDEX DETAILS ************************************
       ! *********************************************************************************

       ! type subdomain
       !   ! Legend:
       !   !        C = Cell Center
       !   !              E = exclude first exterior point
       !   !              I = include first exterior point
       !   !        N = Node
       !   !              B = include boundary point (for node data)
       !   !              I = include first exterior point
       !   !              E = exclude boundary point
       !   !        T = total domain (fluid, e.g.)
       !   type(overlap),dimension(3) :: CE
       !   type(overlap),dimension(3) :: CI
       !   type(overlap),dimension(3) :: NB
       !   type(overlap),dimension(3) :: NI
       !   type(overlap),dimension(3) :: NE
       !   logical,dimension(3) :: defined = .false.
       !   integer :: g_in_id,g_tot_id
       ! end type

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