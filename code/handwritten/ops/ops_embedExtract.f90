      module ops_embedExtract_mod
      ! Not all embed / extract routines are used for each method. For example,
      ! the CT method only uses embedEdge, and not embedCC or embedFace.
      !
      ! Pre-processor directives: (_PARALLELIZE_EMBEDEXTRACT_,_DEBUG_EMBEDEXTRACT_)

      use current_precision_mod
      use overlap_mod
      use data_location_mod
      use face_edge_corner_indexing_mod
      use mesh_mod
      use GF_embed_extract_mod
      use mesh_domain_mod
      use GF_mod
      use SF_mod
      use VF_mod

      implicit none

      private
      public :: extract,extractCC,extractFace,extractEdge
      public :: embed,embedCC,embedFace,embedEdge

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

      contains

      ! *********************************************************************************
      ! ********************** CASE SPECIFIC EXTRACT ROUTINES ***************************
      ! *********************************************************************************

      subroutine extract_SF(interior,total,MD)
        implicit none
        type(SF),intent(inout) :: interior
        type(SF),intent(in) :: total
        type(mesh_domain),intent(in) :: MD
        if (is_CC(total%DL)) then;       call extractCC(interior,total,MD)
        elseif (is_Node(total%DL)) then; ! call extractNode(interior,total,MD)
        stop 'Error: N not supported in extract_SF in ops_embedExtract.f90'
        elseif (is_Face(total%DL)) then; call extractFace(interior,total,MD)
        elseif (is_Edge(total%DL)) then; call extractEdge(interior,total,MD)
        else; stop 'Error: bad data input to extract_F in embedExtract.f90'
        endif
      end subroutine
      subroutine extract_VF(interior,total,MD)
        implicit none
        type(VF),intent(inout) :: interior
        type(VF),intent(in) :: total
        type(mesh_domain),intent(in) :: MD
        call extract(interior%x,total%x,MD)
        call extract(interior%y,total%y,MD)
        call extract(interior%z,total%z,MD)
      end subroutine

      subroutine extractCC_SF(CC_i,CC_t,MD)
        implicit none
        type(SF),intent(inout) :: CC_i
        type(SF),intent(in) :: CC_t
        type(mesh_domain),intent(in) :: MD
        integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
        if (.not.is_CC(CC_i%DL)) stop 'Error: CC data not found (1) in extractCC_SF in ops_embedExtract.f90'
        if (.not.is_CC(CC_t%DL)) stop 'Error: CC data not found (2) in extractCC_SF in ops_embedExtract.f90'
#endif
        do i=1,MD%D%s
        call EX(CC_i%BF(MD%D%sd(i)%physical%g_R1_id)%GF,&
                CC_t%BF(MD%D%sd(i)%physical%g_R2_id)%GF,&
                EE_shape(CC_i,MD,i))
        enddo
      end subroutine
      subroutine extractCC_VF(CC_i,CC_t,MD)
        implicit none
        type(VF),intent(inout) :: CC_i
        type(VF),intent(in) :: CC_t
        type(mesh_domain),intent(in) :: MD
        call extractCC_SF(CC_i%x,CC_t%x,MD)
        call extractCC_SF(CC_i%y,CC_t%y,MD)
        call extractCC_SF(CC_i%z,CC_t%z,MD)
      end subroutine

      subroutine extractFace_SF(face_i,face_t,MD) ! Extracts Lorentz force from induction to momentum
        implicit none
        type(SF),intent(inout) :: face_i
        type(SF),intent(in) :: face_t
        type(mesh_domain),intent(in) :: MD
        integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
        if (.not.is_Face(face_t%DL)) stop 'Error: face data not found (1) in extractFace_SF in ops_embedExtract.f90'
        if (.not.is_Face(face_i%DL)) stop 'Error: face data not found (2) in extractFace_SF in ops_embedExtract.f90'
#endif
        do i=1,MD%D%s
        call EX(face_i%BF(MD%D%sd(i)%physical%g_R1_id)%GF,&
                face_t%BF(MD%D%sd(i)%physical%g_R2_id)%GF,&
                EE_shape(face_i,MD,i))
        enddo
      end subroutine
      subroutine extractFace_VF(face_i,face_t,MD) ! Extracts Lorentz force from induction to momentum
        implicit none
        type(VF),intent(inout) :: face_i
        type(VF),intent(in) :: face_t
        type(mesh_domain),intent(in) :: MD
        call extractFace_SF(face_i%x,face_t%x,MD)
        call extractFace_SF(face_i%y,face_t%y,MD)
        call extractFace_SF(face_i%z,face_t%z,MD)
      end subroutine

      subroutine extractEdge_SF(edge_i,edge_t,MD) ! Auxiliary (energy budget)
        implicit none
        type(SF),intent(inout) :: edge_i
        type(SF),intent(in) :: edge_t
        type(mesh_domain),intent(in) :: MD
        integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
        if (.not.is_Edge(edge_i%DL)) stop 'Error: edge data not found (1) in extractEdge_SF in ops_embedExtract.f90'
        if (.not.is_Edge(edge_t%DL)) stop 'Error: edge data not found (2) in extractEdge_SF in ops_embedExtract.f90'
#endif
        do i=1,MD%D%s
        call EX(edge_i%BF(MD%D%sd(i)%physical%g_R1_id)%GF,&
                edge_t%BF(MD%D%sd(i)%physical%g_R2_id)%GF,&
                EE_shape(edge_i,MD,i))
        enddo
      end subroutine
      subroutine extractEdge_VF(edge_i,edge_t,MD) ! Auxiliary (energy budget)
        implicit none
        type(VF),intent(inout) :: edge_i
        type(VF),intent(in) :: edge_t
        type(mesh_domain),intent(in) :: MD
        call extractEdge_SF(edge_i%x,edge_t%x,MD)
        call extractEdge_SF(edge_i%y,edge_t%y,MD)
        call extractEdge_SF(edge_i%z,edge_t%z,MD)
      end subroutine

      ! *********************************************************************************
      ! ************************ CASE SPECIFIC EMBED ROUTINES ***************************
      ! *********************************************************************************

      subroutine embed_SF(total,interior,MD)
        implicit none
        type(SF),intent(inout) :: total
        type(SF),intent(in) :: interior
        type(mesh_domain),intent(in) :: MD
              if (is_CC(total%DL)) then;  call embedCC(total,interior,MD)
        elseif (is_Node(total%DL)) then; ! call embedNode(total,interior,MD)
        stop 'Error: N not supported in embed_SF in ops_embedExtract.f90'
        elseif (is_Face(total%DL)) then; call embedFace(total,interior,MD)
        elseif (is_Edge(total%DL)) then; call embedEdge(total,interior,MD)
        else; stop 'Error: bad data input to extract_F in embedExtract.f90'
        endif
      end subroutine
      subroutine embed_VF(total,interior,MD)
        implicit none
        type(VF),intent(inout) :: total
        type(VF),intent(in) :: interior
        type(mesh_domain),intent(in) :: MD
        call embed(total%x,interior%x,MD)
        call embed(total%y,interior%y,MD)
        call embed(total%z,interior%z,MD)
      end subroutine

      subroutine embedCC_SF(CC_t,CC_i,MD) ! For material properties
        implicit none
        type(SF),intent(inout) :: CC_t
        type(SF),intent(in) :: CC_i
        type(mesh_domain),intent(in) :: MD
        integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
        if (.not.is_CC(CC_i%DL)) stop 'Error: CC data not found (1) in embedCC_SF in ops_embedExtract.f90'
        if (.not.is_CC(CC_t%DL)) stop 'Error: CC data not found (2) in embedCC_SF in ops_embedExtract.f90'
#endif
        do i=1,MD%D%s
        call EM(CC_t%BF(MD%D%sd(i)%physical%g_R2_id)%GF,&
                CC_i%BF(MD%D%sd(i)%physical%g_R1_id)%GF,&
                EE_shape(CC_t,MD,i))
        enddo
      end subroutine
      subroutine embedCC_VF(CC_t,CC_i,MD)
        implicit none
        type(VF),intent(inout) :: CC_t
        type(VF),intent(in) :: CC_i
        type(mesh_domain),intent(in) :: MD
        call embedCC_SF(CC_t%x,CC_i%x,MD)
        call embedCC_SF(CC_t%y,CC_i%y,MD)
        call embedCC_SF(CC_t%z,CC_i%z,MD)
      end subroutine

      subroutine embedFace_SF(Face_t,Face_i,MD)
        implicit none
        type(SF),intent(inout) :: Face_t
        type(SF),intent(in) :: Face_i
        type(mesh_domain),intent(in) :: MD
        integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
        if (.not.is_Face(Face_i%DL)) stop 'Error: Face data not found (1) in embedFace_SF in ops_embedExtract.f90'
        if (.not.is_Face(Face_t%DL)) stop 'Error: Face data not found (2) in embedFace_SF in ops_embedExtract.f90'
#endif
        do i=1,MD%D%s
        call EM(Face_t%BF(MD%D%sd(i)%physical%g_R2_id)%GF,&
                Face_i%BF(MD%D%sd(i)%physical%g_R1_id)%GF,&
                EE_shape(Face_t,MD,i))
        enddo
      end subroutine
      subroutine embedFace_VF(Face_t,Face_i,MD)
        implicit none
        type(VF),intent(inout) :: Face_t
        type(VF),intent(in) :: Face_i
        type(mesh_domain),intent(in) :: MD
        call embedFace_SF(face_t%x,face_i%x,MD)
        call embedFace_SF(face_t%y,face_i%y,MD)
        call embedFace_SF(face_t%z,face_i%z,MD)
      end subroutine

      subroutine embedEdge_SF(Edge_t,Edge_i,MD) ! Embeds velocity from momentum into induction
        implicit none
        type(SF),intent(inout) :: Edge_t
        type(SF),intent(in) :: Edge_i
        type(mesh_domain),intent(in) :: MD
        integer :: i
#ifdef _DEBUG_EMBEDEXTRACT_
        if (.not.is_Edge(edge_t%DL)) stop 'Error: edge data not found (1) in embedEdge_SF in ops_embedExtract.f90'
        if (.not.is_Edge(edge_i%DL)) stop 'Error: edge data not found (2) in embedEdge_SF in ops_embedExtract.f90'
#endif
        do i=1,MD%D%s
        call EM(Edge_t%BF(MD%D%sd(i)%physical%g_R2_id)%GF,&
                Edge_i%BF(MD%D%sd(i)%physical%g_R1_id)%GF,&
                EE_shape(Edge_t,MD,i))
        enddo
      end subroutine
      subroutine embedEdge_VF(Edge_t,Edge_i,MD) ! Embeds velocity from momentum into induction
        implicit none
        type(VF),intent(inout) :: Edge_t
        type(VF),intent(in) :: Edge_i
        type(mesh_domain),intent(in) :: MD
        call embedEdge_SF(Edge_t%x,Edge_i%x,MD)
        call embedEdge_SF(Edge_t%y,Edge_i%y,MD)
        call embedEdge_SF(Edge_t%z,Edge_i%z,MD)
      end subroutine

      ! *********************************************************************************
      ! ****************************** INDEX DETAILS ************************************
      ! *********************************************************************************

       ! type mesh_domain
       !   type(physical_domain) :: D
       !   type(mesh) :: m_R1,m_R2
       ! end type
       ! type physical_domain
       !   integer :: s ! Number of physical_sub_domains
       !   type(physical_sub_domain),dimension(:),allocatable :: sd
       !   logical :: defined = .false.
       ! end type
       ! type physical_sub_domain
       !   type(sub_domain) :: total
       !   type(sub_domain) :: physical
       !   logical :: defined = .false.
       ! end type
       ! type sub_domain
       !   type(overlap),dimension(3) :: C ! cell center
       !   type(overlap),dimension(3) :: N ! node
       !   type(overlap),dimension(3) :: M ! mixed
       !   logical :: defined = .false.
       !   integer :: g_R1_id = 0
       !   integer :: g_R2_id = 0
       ! end type

      function EE_shape(f,MD,i) result(s)
        ! mesh_domain doesn't use mixed for versatility
        implicit none
        type(SF),intent(in) :: f
        type(mesh_domain),intent(in) :: MD
        integer,intent(in) :: i
        type(overlap),dimension(3) :: s
        if (is_Face(f%DL)) then
          select case(get_Face(f%DL))
          case (1); call init(s(1),MD%D%sd(i)%physical%N(1))
                    call init(s(2),MD%D%sd(i)%physical%C(2))
                    call init(s(3),MD%D%sd(i)%physical%C(3))
          case (2); call init(s(1),MD%D%sd(i)%physical%C(1))
                    call init(s(2),MD%D%sd(i)%physical%N(2))
                    call init(s(3),MD%D%sd(i)%physical%C(3))
          case (3); call init(s(1),MD%D%sd(i)%physical%C(1))
                    call init(s(2),MD%D%sd(i)%physical%C(2))
                    call init(s(3),MD%D%sd(i)%physical%N(3))
          case default; stop 'Error: bad DL face in ops_embedExtract.f90'
          end select
        elseif (is_Edge(f%DL)) then
          select case(get_Edge(f%DL))
          case (1); call init(s(1),MD%D%sd(i)%physical%C(1))
                    call init(s(2),MD%D%sd(i)%physical%N(2))
                    call init(s(3),MD%D%sd(i)%physical%N(3))
          case (2); call init(s(1),MD%D%sd(i)%physical%N(1))
                    call init(s(2),MD%D%sd(i)%physical%C(2))
                    call init(s(3),MD%D%sd(i)%physical%N(3))
          case (3); call init(s(1),MD%D%sd(i)%physical%N(1))
                    call init(s(2),MD%D%sd(i)%physical%N(2))
                    call init(s(3),MD%D%sd(i)%physical%C(3))
          case default; stop 'Error: bad DL edge in ops_embedExtract.f90'
          end select
        elseif (is_CC(f%DL)) then
          call init(s(1),MD%D%sd(i)%physical%C(1))
          call init(s(2),MD%D%sd(i)%physical%C(2))
          call init(s(3),MD%D%sd(i)%physical%C(3))
        elseif (is_Node(f%DL)) then
          call init(s(1),MD%D%sd(i)%physical%N(1))
          call init(s(2),MD%D%sd(i)%physical%N(2))
          call init(s(3),MD%D%sd(i)%physical%N(3))
        else; stop 'Error: no type found in ops_embedExtract.f90'
        endif
      end function

      end module