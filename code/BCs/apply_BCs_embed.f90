       module apply_BCs_embed_mod
       use current_precision_mod
       use check_BCs_mod
       use face_edge_corner_indexing_mod
       use apply_BCs_faces_bridge_mod
       use overlap_mod
       use mesh_mod
       use domain_mod
       use data_location_mod
       use GF_mod
       use SF_mod
       use VF_mod

       implicit none

       private
       public :: apply_BCs_faces_em
       interface apply_BCs_faces_em;     module procedure apply_BCs_faces_SF;    end interface
       interface apply_BCs_faces_em;     module procedure apply_BCs_faces_VF;    end interface

       contains

       ! *********************************************************************************
       ! ***************************** LOW-LEVEL ROUTINE *********************************
       ! *********************************************************************************

       subroutine apply_BCs_faces_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_BCs_faces_SF(U%x,m)
         call apply_BCs_faces_SF(U%y,m)
         call apply_BCs_faces_SF(U%z,m)
       end subroutine
       subroutine apply_BCs_faces_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
#ifdef _DEBUG_APPLY_BCS_FACES_
         call check_defined(U)
#endif
         if (U%is_CC) then;       call apply_BCs_faces_CC(U,m)
         elseif (U%is_Node) then; call apply_BCs_faces_Node(U,m)
         elseif (U%is_Face) then; call apply_BCs_faces_Face(U,m)
         elseif (U%is_Edge) then; call apply_BCs_faces_Edge(U,m)
         else; stop 'Error: bad data input to apply_BCs_faces_SF in apply_BCs_embed.f90'
         endif
       end subroutine

       subroutine apply_BCs_faces_CC(U)
         implicit none
         type(SF),intent(inout) :: U
         integer :: t,i
         do t=1,U%s
         do i=1,U%BF(t)%BCs_%P_D%N
         call U%BF(t)%BCs_%P_D%SP(i)%P(U%BF(t)%GF,&
                                       U%BF(t)%BCs_%f(U%BF(t)%BCs_%D%g%sd(i)%g_R1_id),&
                                       U%BF(t)%BCs_%D,i)
         enddo
         do i=1,U%BF(t)%BCs_%P_N%N
         call U%BF(t)%BCs_%P_N%SP(i)%P(U%BF(t)%GF,&
                                       U%BF(t)%BCs_%f(U%BF(t)%BCs_%N%g%sd(i)%g_R1_id),&
                                       U%BF(t)%BCs_%N,i)
         enddo
         enddo
       end subroutine
       subroutine apply_BCs_faces_Node(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: t
         do t=1,m%s
         ! call Dirichlet_N(U%BF(t),m%B(t)); call Neumann_N(U%BF(t),m%B(t))
         enddo
       end subroutine
       subroutine apply_BCs_faces_Face(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: t
         do t=1,m%s
         ! call Dirichlet_F(U%BF(t),m%B(t)); call Neumann_F(U%BF(t),m%B(t))
         enddo
       end subroutine
       subroutine apply_BCs_faces_Edge(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: t
         do t=1,m%s
         ! call Dirichlet_E(U%BF(t),m%B(t)); call Neumann_E(U%BF(t),m%B(t))
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

       function EE_shape(DL,D,i) result(s)
         implicit none
         type(data_location),intent(in) :: DL
         type(domain),intent(in) :: D
         integer,intent(in) :: i
         type(overlap),dimension(3) :: s
         if (is_Face(DL)) then
           select case (DL%face)
           case (1); s = (/D%sd(i)%NB(1),D%sd(i)%CI(2),D%sd(i)%CI(3)/)
           case (2); s = (/D%sd(i)%CI(1),D%sd(i)%NB(2),D%sd(i)%CI(3)/)
           case (3); s = (/D%sd(i)%CI(1),D%sd(i)%CI(2),D%sd(i)%NB(3)/)
           case default; stop 'Error: f%face must = 1,2,3 in ops_embedExtract.f90'
           end select
         elseif (is_Edge(DL)) then
           select case (DL%edge)
           case (1); s = (/D%sd(i)%CI(1),D%sd(i)%NB(2),D%sd(i)%NB(3)/)
           case (2); s = (/D%sd(i)%NB(1),D%sd(i)%CI(2),D%sd(i)%NB(3)/)
           case (3); s = (/D%sd(i)%NB(1),D%sd(i)%NB(2),D%sd(i)%CI(3)/)
           case default; stop 'Error: f%edge must = 1,2,3 in ops_embedExtract.f90'
           end select
         elseif (is_CC(DL)) then
           s = (/D%sd(i)%CI(1),D%sd(i)%CI(2),D%sd(i)%CI(3)/)
         elseif (is_Node(DL)) then
           s = (/D%sd(i)%NB(1),D%sd(i)%NB(2),D%sd(i)%NB(3)/)
         else; stop 'Error: no type found in ops_embedExtract.f90'
         endif
       end function

       end module