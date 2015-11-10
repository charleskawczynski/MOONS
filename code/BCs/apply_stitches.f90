       module apply_stitches_mod
       use apply_stitches_faces_mod
       use apply_stitches_edges_mod
       use apply_stitches_corners_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       implicit none

       private
       public :: apply_stitches

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       interface apply_stitches;    module procedure apply_stitches_VF;     end interface
       interface apply_stitches;    module procedure apply_stitches_SF;     end interface

       contains

       subroutine apply_stitches_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_stitches(U%x,m)
         call apply_stitches(U%y,m)
         call apply_stitches(U%z,m)
       end subroutine

       subroutine apply_stitches_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_stitches_faces(U,m)
         call apply_stitches_edges(U,m)
         call apply_stitches_corners(U,m)
       end subroutine

       end module