      module matrix_free_operators_mod
      ! NOTES:
      !      ONLY CENTERED DERIVATIVES SHOULD BE USED IN THIS
      !      FILE SINCE IMPLICIT SOLVERS IN MOONS DEPEND ON BC
      !      IMPLICIT DERIVATIVES. SEE DOCUMENTATION.
      !
      use current_precision_mod
      use mesh_mod
      use data_location_mod
      use SF_mod
      use VF_mod
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

      abstract interface
        subroutine op_SF(Ax,x,k,m,MFP,tempk)
          import :: SF,VF,mesh,matrix_free_params
          implicit none
          type(SF),intent(inout) :: Ax,x
          type(VF),intent(in) :: k
          type(VF),intent(inout) :: tempk
          type(mesh),intent(in) :: m
          type(matrix_free_params),intent(in) :: MFP
        end subroutine
      end interface

      abstract interface
        subroutine op_SF_explicit(Ax,x,k,m,MFP,tempk)
          import :: SF,VF,mesh,matrix_free_params
          implicit none
          type(SF),intent(inout) :: Ax,x
          type(VF),intent(in) :: k
          type(VF),intent(inout) :: tempk
          type(mesh),intent(in) :: m
          type(matrix_free_params),intent(in) :: MFP
        end subroutine
      end interface

      abstract interface
        subroutine op_VF(Ax,x,k,m,MFP,tempk)
          import :: VF,mesh,matrix_free_params
          implicit none
          type(VF),intent(inout) :: Ax,x
          type(VF),intent(in) :: k
          type(VF),intent(inout) :: tempk
          type(mesh),intent(in) :: m
          type(matrix_free_params),intent(in) :: MFP
        end subroutine
      end interface

      abstract interface
        subroutine op_VF_explicit(Ax,x,k,m,MFP,tempk)
          import :: VF,mesh,matrix_free_params
          implicit none
          type(VF),intent(inout) :: Ax,x
          type(VF),intent(in) :: k
          type(VF),intent(inout) :: tempk
          type(mesh),intent(in) :: m
          type(matrix_free_params),intent(in) :: MFP
        end subroutine
      end interface

      contains

      subroutine Lap_uniform_SF_explicit(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(∇)
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        suppress_warning = is_CC(k)
        suppress_warning = is_CC(tempk)
        call lap_centered(Ax,x,m)
        ! call grad(tempk,x,m)
        ! call div(Ax,tempk,m)
      end subroutine
      subroutine Lap_uniform_SF(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(∇)
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
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
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        suppress_warning = is_CC(k)
        suppress_warning = is_CC(tempk)
        call lap_centered(Ax,x,m) ! Involves dynamic allocations
        ! call lap(Ax,x,m)
      end subroutine
      subroutine Lap_uniform_VF(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(∇)
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
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
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        call grad(tempk,x,m)
        call multiply(tempk,k)
        call div(Ax,tempk,m)
      end subroutine
      subroutine Lap_nonuniform_props(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(k∇)
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
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
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        call curl(tempk,x,m)
        call multiply(tempk,k)
        call curl(Ax,tempk,m)
        call multiply(Ax,MFP%coeff)
        call add(Ax,x)
      end subroutine
      subroutine ind_diffusion(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = {I + coeff ∇x(k∇x)}
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
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
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        call grad(tempk,x,m)
        call multiply(tempk,k)
        call div(Ax,tempk,m)
        call multiply(Ax,MFP%coeff)
        call add(Ax,x)
      end subroutine
      subroutine nrg_diffusion(Ax,x,k,m,MFP,tempk)
        ! Computes:
        !        A = {I + coeff ∇•(k∇)}
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
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
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = is_CC(k)
        suppress_warning = is_CC(tempk)
        ! lap_centered is a very bad and expensive routine. It needs
        ! to be updated (a VF is allocated and deallocated inside).
        ! The reason this is not as simple as the laplacian operator
        ! is because U is staggered, and so k (the intermediate location),
        ! is staggered AND different for each component, which cannot be
        ! achieved by a scalar field.
        call lap_centered(Ax,x,m)
        call multiply(Ax,MFP%coeff)
        call add(Ax,x)
      end subroutine
      subroutine mom_diffusion(Ax,x,k,m,MFP,tempk)
        ! Computes:
        !        A = {I + coeff ∇•(∇)}
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        call apply_BCs_implicit(x)
        call mom_diffusion_explicit(Ax,x,k,m,MFP,tempk)
      end subroutine

      end module