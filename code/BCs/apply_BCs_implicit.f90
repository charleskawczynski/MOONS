       module apply_BCs_implicit_mod
       use current_precision_mod
       use SF_mod
       use VF_mod
       use apply_BCs_faces_implicit_mod
       use apply_BCs_edges_implicit_mod
       ! use apply_BCs_corners_implicit_mod
       use mesh_mod
       implicit none

       private
       public :: apply_BCs_implicit

       interface apply_BCs_implicit;  module procedure apply_BCs_implicit_VF;   end interface
       interface apply_BCs_implicit;  module procedure apply_BCs_implicit_SF;   end interface

       contains

       subroutine apply_BCs_implicit_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_BCs_implicit(U%x,m)
         call apply_BCs_implicit(U%y,m)
         call apply_BCs_implicit(U%z,m)
       end subroutine

       subroutine apply_BCs_implicit_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         ! call apply_BCs_faces_implicit(U,m)
         ! call apply_BCs_edges_implicit(U,m)
         ! call apply_BCs_corners_implicit(U,m)

         call apply_BCs_faces_implicit(U,m)
         if (m%s.gt.1) call apply_BCs_edges_implicit(U,m)
         ! call apply_BCs_coners_implicit(U,m)
         ! if (m%s.gt.1) call apply_stitches_corners(U,m)

       end subroutine

       end module 