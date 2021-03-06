      module matrix_free_operators_mod
      use matrix_free_operators_interfaces_mod
      ! NOTES:
      !      ONLY CENTERED DERIVATIVES SHOULD BE USED IN THIS
      !      FILE SINCE IMPLICIT SOLVERS IN MOONS DEPEND ON BC
      !      IMPLICIT DERIVATIVES. SEE DOCUMENTATION.
      !
      use current_precision_mod
      use mesh_extend_mod
      use data_location_extend_mod
      use SF_extend_mod
      use VF_extend_mod
      use TF_extend_mod
      use ops_discrete_mod
      use ops_aux_mod
      use ops_interp_mod
      use apply_BCs_mod
      use apply_BCs_implicit_mod
      use matrix_free_params_mod
      implicit none

      private
      public :: Lap_uniform_SF_explicit,Lap_uniform_SF
      public :: Lap_uniform_VF_explicit,Lap_uniform_VF
      public :: Lap_nonuniform_props_explicit,Lap_nonuniform_props
      public :: ind_diffusion_explicit,ind_diffusion
      public :: nrg_diffusion_explicit,nrg_diffusion
      public :: mom_diffusion_explicit,mom_diffusion

      public :: op_SF,op_SF_explicit
      public :: op_VF,op_VF_explicit

      contains

      subroutine Lap_uniform_SF_explicit(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(∇)
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        suppress_warning = is_CC(k%x)
        call lap_centered(Ax,x,m,tempk%x)
        ! call grad(tempk,x,m)
        ! call div(Ax,tempk,m)
      end subroutine
      subroutine Lap_uniform_SF(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(∇)
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        call apply_BCs_implicit(x)
        call Lap_uniform_SF_explicit(Ax,x,k,m,MFP,tempk)
      end subroutine

      subroutine Lap_uniform_VF_explicit(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(∇)
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        suppress_warning = is_CC(k%x)
        call lap_centered(Ax,x,m,tempk)
        ! call lap(Ax,x,m)
      end subroutine
      subroutine Lap_uniform_VF(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(∇)
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        call apply_BCs_implicit(x)
        call Lap_uniform_VF_explicit(Ax,x,k,m,MFP,tempk)
      end subroutine

      subroutine Lap_nonuniform_props_explicit(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(k∇)
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        call grad(tempk%x,x,m)
        call multiply(tempk%x,k%x)
        call div(Ax,tempk%x,m)
      end subroutine
      subroutine Lap_nonuniform_props(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(k∇)
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        call apply_BCs_implicit(x)
        call Lap_nonuniform_props_explicit(Ax,x,k,m,MFP,tempk)
      end subroutine

      subroutine ind_diffusion_explicit(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = {I + coeff ∇x(k∇x)}
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        call curl(tempk%x,x,m)
        call multiply(tempk%x,k%x)
        call curl(Ax,tempk%x,m)
        call multiply(Ax,MFP%coeff_implicit_time_split)
        call add(Ax,x)
      end subroutine
      subroutine ind_diffusion(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = {I + coeff ∇x(k∇x)}
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        call apply_BCs_implicit(x)
        call ind_diffusion_explicit(Ax,x,k,m,MFP,tempk)
      end subroutine

      subroutine nrg_diffusion_explicit(Ax,x,k,m,MFP,tempk)
        ! Computes:
        !        A = {I + coeff ∇•(k∇)}
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        call grad(tempk%x,x,m)
        call multiply(tempk%x,k%x)
        call div(Ax,tempk%x,m)
        call multiply(Ax,MFP%coeff_implicit_time_split)
        call add(Ax,x)
      end subroutine
      subroutine nrg_diffusion(Ax,x,k,m,MFP,tempk)
        ! Computes:
        !        A = {I + coeff ∇•(k∇)}
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        call apply_BCs_implicit(x)
        call nrg_diffusion_explicit(Ax,x,k,m,MFP,tempk)
      end subroutine

      subroutine mom_diffusion_explicit(Ax,x,k,m,MFP,tempk)
        ! Computes:
        !        A = {I + coeff ∇•(∇)}
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = is_CC(k)
        call lap_centered(Ax,x,m,tempk)
        call multiply(Ax,MFP%coeff_implicit_time_split)
        call add(Ax,x)
      end subroutine
      subroutine mom_diffusion(Ax,x,k,m,MFP,tempk)
        ! Computes:
        !        A = {I + coeff ∇•(∇)}
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        call apply_BCs_implicit(x)
        call mom_diffusion_explicit(Ax,x,k,m,MFP,tempk)
      end subroutine

      end module