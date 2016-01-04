      module matrix_free_operators_mod
      ! NOTE:
      ! ONLY CENTERED DERIVATIVES SHOULD BE USED IN THIS 
      ! FILE SINCE IMPLICIT SOLVERS IN MOONS DEPEND ON BC
      ! IMPLICIT DERIVATIVES.
      use mesh_mod
      use SF_mod
      use VF_mod
      use ops_discrete_mod
      use ops_aux_mod
      use ops_interp_mod
      use apply_BCs_mod
      use apply_BCs_implicit_mod
      use matrix_free_params_mod
      implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      private
      public :: Lap_uniform_props_explicit,Lap_uniform_props
      public :: Lap_nonuniform_props_explicit,Lap_nonuniform_props
      public :: ind_diffusion_explicit,ind_diffusion
      public :: eng_diffusion_explicit,eng_diffusion
      public :: mom_diffusion_explicit,mom_diffusion

      contains

      subroutine Lap_uniform_props_explicit(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(∇)
        implicit none
        type(SF),intent(inout) :: Ax
        type(SF),intent(in) :: x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: tempk
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        call grad(tempk,x,m)
        call div(Ax,tempk,m)
        call zeroGhostPoints(Ax)
      end subroutine
      subroutine Lap_uniform_props(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(∇)
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: tempk
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        call apply_BCs_implicit(x,m)
        call grad(tempk,x,m)
        call div(Ax,tempk,m)
        call zeroGhostPoints(Ax)
      end subroutine

      subroutine Lap_nonuniform_props_explicit(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(k∇)
        implicit none
        type(SF),intent(inout) :: Ax
        type(SF),intent(in) :: x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: tempk
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        call grad(tempk,x,m)
        call multiply(tempk,k)
        call div(Ax,tempk,m)
        call zeroGhostPoints(Ax)
      end subroutine
      subroutine Lap_nonuniform_props(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(k∇)
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: tempk
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        call apply_BCs_implicit(x,m)
        call grad(tempk,x,m)
        call multiply(tempk,k)
        call div(Ax,tempk,m)
        call zeroGhostPoints(Ax)
      end subroutine

      subroutine ind_diffusion_explicit(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = {I + c_ind ∇x(k∇x)}
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: tempk
        type(matrix_free_params),intent(in) :: MFP
        call curl(tempk,x,m)
        call multiply(tempk,k)
        call curl(Ax,tempk,m)
        ! call multiply(Ax,MFP%c_ind)
        ! call add(Ax,x)
        call zeroGhostPoints(Ax)
      end subroutine
      subroutine ind_diffusion(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = {I + c_ind ∇x(k∇x)}
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: tempk
        type(matrix_free_params),intent(in) :: MFP
        call apply_BCs_implicit(x,m)
        call curl(tempk,x,m)
        call multiply(tempk,k)
        call curl(Ax,tempk,m)
        call multiply(Ax,MFP%c_ind)
        call add(Ax,x)
        call zeroGhostPoints(Ax)
      end subroutine

      subroutine eng_diffusion_explicit(Ax,x,k,m,MFP,tempk)
        ! Computes:
        !        A = {I + c_eng ∇•(k∇)}
        implicit none
        type(SF),intent(inout) :: Ax
        type(SF),intent(inout) :: x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        type(VF),intent(inout) :: tempk
        call grad(tempk,x,m)
        call multiply(tempk,k)
        call div(Ax,tempk,m)
        call multiply(Ax,MFP%c_eng)
        call add(Ax,x)
        call zeroGhostPoints(Ax)
      end subroutine
      subroutine eng_diffusion(Ax,x,k,m,MFP,tempk)
        ! Computes:
        !        A = {I + c_eng ∇•(k∇)}
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        type(VF),intent(inout) :: tempk
        call apply_BCs_implicit(x,m)
        call grad(tempk,x,m)
        call multiply(tempk,k)
        call div(Ax,tempk,m)
        call multiply(Ax,MFP%c_eng)
        call add(Ax,x)
        call zeroGhostPoints(Ax)
      end subroutine

      subroutine mom_diffusion_explicit(Ax,x,k,m,MFP,tempk)
        ! Computes:
        !        A = {I + c_mom ∇•(k∇)}
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        type(VF),intent(inout) :: tempk
        call lap_centered(Ax,x,m)
        call multiply(Ax,MFP%c_mom)
        call add(Ax,x)
        call zeroGhostPoints(Ax)
      end subroutine
      subroutine mom_diffusion(Ax,x,k,m,MFP,tempk)
        ! Computes:
        !        A = {I + c_mom ∇•(k∇)}
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        type(VF),intent(inout) :: tempk
        call apply_BCs_implicit(x,m)
        call lap_centered(Ax,x,m)
        call multiply(Ax,MFP%c_mom)
        call add(Ax,x)
        call zeroGhostPoints(Ax)
      end subroutine

      end module