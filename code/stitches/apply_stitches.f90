       module apply_stitches_mod
       use current_precision_mod
       use apply_stitches_faces_mod
       use apply_stitches_edges_mod
       use apply_stitches_corners_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       implicit none

       private
       public :: apply_stitches

       interface apply_stitches;    module procedure apply_stitches_VF;     end interface
       interface apply_stitches;    module procedure apply_stitches_SF;     end interface

       contains

       subroutine apply_stitches_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         if (m%s.gt.1) then
           call apply_stitches(U%x,m)
           call apply_stitches(U%y,m)
           call apply_stitches(U%z,m)
         endif
       end subroutine

       subroutine apply_stitches_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         if (m%s.gt.1) then
           call apply_stitches_faces(U,m)
           call apply_stitches_edges(U,m)
           ! call apply_stitches_corners(U,m)
         endif
       end subroutine

       end module