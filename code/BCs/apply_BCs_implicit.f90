       module apply_BCs_implicit_mod
       use SF_mod
       use VF_mod
       use apply_BCs_faces_implicit_mod
       ! use apply_BCs_edges_implicit_mod
       ! use apply_BCs_corners_implicit_mod
       use mesh_mod
       implicit none

       private
       public :: apply_BCs_implicit

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       interface apply_BCs_implicit;  module procedure apply_BCs_VF;             end interface
       interface apply_BCs_implicit;  module procedure apply_BCs_SF;             end interface

       ! interface apply_BCs_implicit;  module procedure apply_BCs_VF_given_BC;    end interface
       ! interface apply_BCs_implicit;  module procedure apply_BCs_SF_given_BC;    end interface

       contains

       subroutine apply_BCs_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_BCs_implicit(U%x,m)
         call apply_BCs_implicit(U%y,m)
         call apply_BCs_implicit(U%z,m)
       end subroutine

       subroutine apply_BCs_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_BCs_faces_implicit(U,m)
         ! call apply_BCs_edges_implicit(U,m)
         ! call apply_BCs_corners_implicit(U,m)
       end subroutine

       subroutine apply_BCs_VF_given_BC(U,m,BC)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: BC
         call apply_BCs_SF_given_BC(U%x,m,BC%x)
         call apply_BCs_SF_given_BC(U%y,m,BC%y)
         call apply_BCs_SF_given_BC(U%z,m,BC%z)
       end subroutine

       subroutine apply_BCs_SF_given_BC(U,m,BC)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: BC
         call apply_BCs_faces_implicit(U,m)
         ! call apply_BCs_edges_implicit(U,m)
         ! call apply_BCs_corners_implicit(U,m)
       end subroutine

       end module 