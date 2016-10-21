      module PCG_aux_mod
      use current_precision_mod
      use bctype_mod
      use mesh_mod
      use SF_mod
      use VF_mod
      implicit none

      private

      public :: modify_forcing1
      interface modify_forcing1;             module procedure modify_forcing1_SF;              end interface
      interface modify_forcing1;             module procedure modify_forcing1_VF;              end interface

      contains

      subroutine modify_forcing1_SF(f,m,x)
        implicit none
        type(SF),intent(inout) :: f
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: x
        integer :: i
        call assign_wall_Dirichlet(f,0.0_cp,x)
        call multiply_wall_Neumann(f,0.5_cp,x)
      end subroutine

      subroutine modify_forcing1_VF(f,m,x)
        implicit none
        type(VF),intent(inout) :: f
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: x
        call assign_wall_Dirichlet(f,0.0_cp,x)
        call multiply_wall_Neumann(f,0.5_cp,x)
      end subroutine

      subroutine zeroGhostPoints_conditional_SF(f,m)
        implicit none
        type(SF),intent(inout) :: f
        type(mesh),intent(in) :: m
        integer :: i
        call assign_ghost_N_XPeriodic(f,0.0_cp)
      end subroutine
      subroutine zeroGhostPoints_conditional_SF2(f,m,x)
        implicit none
        type(SF),intent(inout) :: f
        type(SF),intent(in) :: x
        type(mesh),intent(in) :: m
        integer :: i
        call assign_ghost_N_XPeriodic(f,0.0_cp,x)
      end subroutine

      subroutine zeroGhostPoints_conditional_VF(f,m)
        implicit none
        type(VF),intent(inout) :: f
        type(mesh),intent(in) :: m
        call zeroGhostPoints_conditional(f%x,m)
        call zeroGhostPoints_conditional(f%y,m)
        call zeroGhostPoints_conditional(f%z,m)
      end subroutine
      subroutine zeroGhostPoints_conditional_VF2(f,m,x)
        implicit none
        type(VF),intent(inout) :: f
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: x
        call zeroGhostPoints_conditional(f%x,m,x%x)
        call zeroGhostPoints_conditional(f%y,m,x%y)
        call zeroGhostPoints_conditional(f%z,m,x%z)
      end subroutine

      end module