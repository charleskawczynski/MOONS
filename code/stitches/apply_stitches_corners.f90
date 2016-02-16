       module apply_stitches_corners_mod
       use RF_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       implicit none

       private
       public :: apply_stitches_corners

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       interface apply_stitches_corners;    module procedure apply_stitches_corners_VF;     end interface
       interface apply_stitches_corners;    module procedure apply_stitches_corners_SF;     end interface

       contains

       subroutine apply_stitches_corners_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_stitches_corners(U%x,m)
         call apply_stitches_corners(U%y,m)
         call apply_stitches_corners(U%z,m)
       end subroutine

       subroutine apply_stitches_corners_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         logical :: suppress_warning
         integer :: supp_warning
         suppress_warning = U%is_CC
         supp_warning = m%N_cells_tot
       end subroutine


       end module