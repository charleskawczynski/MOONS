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
         integer :: t,i
#ifdef _DEBUG_APPLY_BCS_FACES_
         call check_defined(U)
#endif
         do t=1,U%s
         do i=1,U%BF(t)%BCs_%PA_face_BCs%N
         call U%BF(t)%BCs_%PA_face_BCs%SP(i)%P(U%BF(t)%GF,&
                                       U%BF(t)%BCs_%f(U%BF(t)%BCs_%face_BCs%g%sd(i)%g_R1_id),&
                                       U%BF(t)%BCs_%face_BCs,i)
         enddo
         enddo
       end subroutine

       end module