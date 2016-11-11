       module block_Laplacian_stencil_mod
       use current_precision_mod
       use grid_mod
       use GF_mod
       use stencil_1D_mod
       use stencil_3D_mod
       use data_location_mod
       implicit none

       private
       public :: init_Laplacian
       interface init_Laplacian;  module procedure init_Laplacian_SF;     end interface
       interface init_Laplacian;  module procedure init_Laplacian_SF_VP;  end interface

       interface init_Laplacian;  module procedure init_Laplacian_VF;     end interface
       interface init_Laplacian;  module procedure init_Laplacian_VF_VP;  end interface

       contains

      ! ****************************************************************************************
      ! ****************************************** SF ******************************************
      ! ****************************************************************************************

       subroutine init_Laplacian_SF(lap,g)
         implicit none
         type(stencil_3D),intent(inout) :: lap
         type(grid),intent(in) :: g
         call init(lap,g,DL_CC())
         call assign_consecutive(lap)
         call add_diagonals(lap)
         call clean(lap)
       end subroutine

       subroutine init_Laplacian_SF_VP(lap,g,sig)
         implicit none
         type(stencil_3D),intent(inout) :: lap
         type(grid),intent(in) :: g
         type(grid_field),dimension(3),intent(in) :: sig
         call init(lap,g,DL_CC())
         call assign_consecutive(lap,sig)
         call add_diagonals(lap)
         call clean(lap)
       end subroutine

      ! ****************************************************************************************
      ! ****************************************** VF ******************************************
      ! ****************************************************************************************

       subroutine init_Laplacian_VF(lap,g)
         implicit none
         type(stencil_3D),dimension(3),intent(inout) :: lap
         type(grid),intent(in) :: g
         integer :: i
         do i=1,3; call init(lap(i),g,DL_Face(i)); enddo
         do i=1,3; call assign_consecutive(lap(i)); enddo
         do i=1,3; call add_diagonals(lap(i)); enddo
         do i=1,3; call clean(lap(i)); enddo
       end subroutine

       subroutine init_Laplacian_VF_VP(lap,g,sig)
         implicit none
         type(stencil_3D),dimension(3),intent(inout) :: lap
         type(grid),intent(in) :: g
         type(grid_field),dimension(3),intent(in) :: sig
         integer :: i
         do i=1,3; call init(lap(i),g,DL_Face(i)); enddo
         do i=1,3; call assign_consecutive(lap(i),sig); enddo
         do i=1,3; call add_diagonals(lap(i)); enddo
         do i=1,3; call clean(lap(i)); enddo
       end subroutine

       end module