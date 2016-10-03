       module apply_BCs_faces_mod
       use current_precision_mod
       use check_BCs_mod
       use overlap_mod
       use mesh_mod
       use domain_mod
       use RF_mod
       use SF_mod
       use VF_mod

       implicit none

       private
       public :: apply_BCs_faces

       contains

       ! *********************************************************************************
       ! ***************************** LOW-LEVEL ROUTINE *********************************
       ! *********************************************************************************

       subroutine apply_BCs_faces_VF(U)
         implicit none
         type(VF),intent(inout) :: U
         call apply_BCs_faces_SF(U%x)
         call apply_BCs_faces_SF(U%y)
         call apply_BCs_faces_SF(U%z)
       end subroutine
       subroutine apply_BCs_faces_SF(U)
         implicit none
         type(SF),intent(inout) :: U
#ifdef _DEBUG_APPLY_BCS_FACES_
         call check_defined(U)
#endif
         if (U%is_CC) then;       call apply_BCs_faces_CC(U)
         elseif (U%is_Node) then; call apply_BCs_faces_Node(U)
         elseif (U%is_Face) then; call apply_BCs_faces_Face(U)
         elseif (U%is_Edge) then; call apply_BCs_faces_Edge(U)
         else; stop 'Error: bad data input to apply_BCs_faces_SF in apply_BCs_faces.f90'
         endif
       end subroutine

       subroutine apply_BCs_faces_CC(U)
         implicit none
         type(SF),intent(inout) :: U
         integer :: i
#ifdef _DEBUG_APPLY_BCS_FACES_
         if (.not.U%is_CC) stop 'Error: CC data not found (1) in apply_BCs_faces_CC in apply_BCs_faces.f90'
#endif
         ! call Dirichlet_C(bulk,surf,G,I,S,p)
         call EM(U%RF(D%sd(i)%g_R2_id),U%RF(D%sd(i)%g_R1_id),EE_shape(U,D,i),'embedCC_SF')
         if (U%BCs%Dirichlet%defined) then
           do i=1,U%BCs%Dirichlet%s
           call Dirichlet_C(U%BF(U%BCs%Dirichlet%sd(i)%g_R2_id)%b,&
                            U%BF(U%BCs%Dirichlet%sd(i)%g_R1_id)%f,&
                            G,I,S,1)
           enddo
         endif
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
         call EM(CC_t%x%RF(D%sd(i)%g_R2_id),CC_i%x%RF(D%sd(i)%g_R1_id),EE_shape(CC_t%x,D,i),'embedCC_VF')
         call EM(CC_t%y%RF(D%sd(i)%g_R2_id),CC_i%y%RF(D%sd(i)%g_R1_id),EE_shape(CC_t%y,D,i),'embedCC_VF')
         call EM(CC_t%z%RF(D%sd(i)%g_R2_id),CC_i%z%RF(D%sd(i)%g_R1_id),EE_shape(CC_t%z,D,i),'embedCC_VF')
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
         call EM(Face_t%RF(D%sd(i)%g_R2_id),Face_i%RF(D%sd(i)%g_R1_id),EE_shape(Face_t,D,i),'embedFace_SF')
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
         call EM(Face_t%x%RF(D%sd(i)%g_R2_id),Face_i%x%RF(D%sd(i)%g_R1_id),EE_shape(Face_t%x,D,i),'embedFace_VF')
         call EM(Face_t%y%RF(D%sd(i)%g_R2_id),Face_i%y%RF(D%sd(i)%g_R1_id),EE_shape(Face_t%y,D,i),'embedFace_VF')
         call EM(Face_t%z%RF(D%sd(i)%g_R2_id),Face_i%z%RF(D%sd(i)%g_R1_id),EE_shape(Face_t%z,D,i),'embedFace_VF')
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
         call EM(Edge_t%RF(D%sd(i)%g_R2_id),Edge_i%RF(D%sd(i)%g_R1_id),EE_shape(Edge_t,D,i),'embedFace_VF')
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
         call EM(Edge_t%x%RF(D%sd(i)%g_R2_id),Edge_i%x%RF(D%sd(i)%g_R1_id),EE_shape(Edge_t%x,D,i),'embedEdge_VF')
         call EM(Edge_t%y%RF(D%sd(i)%g_R2_id),Edge_i%y%RF(D%sd(i)%g_R1_id),EE_shape(Edge_t%y,D,i),'embedEdge_VF')
         call EM(Edge_t%z%RF(D%sd(i)%g_R2_id),Edge_i%z%RF(D%sd(i)%g_R1_id),EE_shape(Edge_t%z,D,i),'embedEdge_VF')
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
       !   logical,dimension(3) :: defined = .false.
       !   integer :: g_R1_id,g_R2_id
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