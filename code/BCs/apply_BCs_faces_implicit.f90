       module apply_BCs_faces_implicit_mod
       use current_precision_mod
       use GF_mod
       use SF_mod
       use VF_mod
       use bctype_mod
       use boundary_conditions_mod
       use block_field_mod
       use mesh_mod
       use check_BCs_mod
       use apply_BCs_embed_mod
       use face_edge_corner_indexing_mod
       implicit none

       private
       public :: apply_BCs_faces_implicit


       interface apply_BCs_faces_implicit;  module procedure apply_BCs_faces_VF;     end interface
       interface apply_BCs_faces_implicit;  module procedure apply_BCs_faces_SF;     end interface

       contains

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
         integer :: k
#ifdef _DEBUG_APPLY_BCS_
       call check_defined(U)
#endif
         call apply_BCs_faces_implicit_em(U)
       end subroutine


       end module