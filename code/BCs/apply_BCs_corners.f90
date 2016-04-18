       module apply_BCs_corners_mod
       use current_precision_mod
       use RF_mod
       use SF_mod
       use VF_mod
       use bctype_mod
       use BCs_mod
       use grid_mod
       use mesh_mod
       implicit none

       private
       public :: apply_BCs_corners




       interface apply_BCs_corners;       module procedure apply_BCs_corners_VF;     end interface
       interface apply_BCs_corners;       module procedure apply_BCs_corners_SF;     end interface

       contains

       subroutine apply_BCs_corners_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_BCs_corners(U%x,m)
         call apply_BCs_corners(U%y,m)
         call apply_BCs_corners(U%z,m)
       end subroutine

       subroutine apply_BCs_corners_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         logical :: suppress_warning
         integer :: supp_warning
         suppress_warning = U%is_CC
         supp_warning = m%N_cells_tot
       end subroutine

       end module