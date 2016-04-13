      module matrix_free_operators_mod
      ! NOTES:
      !      ONLY CENTERED DERIVATIVES SHOULD BE USED IN THIS 
      !      FILE SINCE IMPLICIT SOLVERS IN MOONS DEPEND ON BC
      !      IMPLICIT DERIVATIVES.
      !      
      ! IMPORTANT:
      !      The intent in some of the routines below
      !      were specified only to match an interface.
      !      Ideally, the intent would be "in", but this
      !      would require multiple interfaces and a larger
      !      argument list. Therefore, one must understand
      !      that these intents are for code reusability
      !      over safe programming.
      !      
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
      public :: Lap_uniform_SF_explicit,Lap_uniform_SF
      public :: Lap_uniform_VF_explicit,Lap_uniform_VF
      public :: Lap_nonuniform_props_explicit,Lap_nonuniform_props
      public :: ind_diffusion_explicit,ind_diffusion
      public :: eng_diffusion_explicit,eng_diffusion
      public :: mom_diffusion_explicit,mom_diffusion

      public :: op_SF,op_SF_explicit
      public :: op_VF,op_VF_explicit

      abstract interface
        subroutine op_SF(Ax,x,k,m,MFP,tempk)
          import :: SF,VF,mesh,matrix_free_params
          implicit none
          type(SF),intent(inout) :: Ax,x
          type(VF),intent(inout) :: k,tempk
          type(mesh),intent(in) :: m
          type(matrix_free_params),intent(in) :: MFP
        end subroutine
      end interface

      abstract interface
        subroutine op_SF_explicit(Ax,x,k,m,MFP,tempk)
          import :: SF,VF,mesh,matrix_free_params
          implicit none
          type(SF),intent(inout) :: Ax,x
          type(VF),intent(inout) :: k,tempk
          type(mesh),intent(in) :: m
          type(matrix_free_params),intent(in) :: MFP
        end subroutine
      end interface

      abstract interface
        subroutine op_VF(Ax,x,k,m,MFP,tempk)
          import :: VF,mesh,matrix_free_params
          implicit none
          type(VF),intent(inout) :: Ax,x
          type(VF),intent(inout) :: k,tempk
          type(mesh),intent(in) :: m
          type(matrix_free_params),intent(in) :: MFP
        end subroutine
      end interface

      abstract interface
        subroutine op_VF_explicit(Ax,x,k,m,MFP,tempk)
          import :: VF,mesh,matrix_free_params
          implicit none
          type(VF),intent(inout) :: Ax,x
          type(VF),intent(inout) :: k,tempk
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
        type(VF),intent(inout) :: k,tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        suppress_warning = k%is_CC
        call grad(tempk,x,m)
        call div(Ax,tempk,m)
      end subroutine
      subroutine Lap_uniform_SF(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(∇)
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(VF),intent(inout) :: k,tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        suppress_warning = k%is_CC
        call apply_BCs_implicit(x,m)
        call grad(tempk,x,m)
        call div(Ax,tempk,m)
      end subroutine

      subroutine Lap_uniform_VF_explicit(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(∇)
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(VF),intent(inout) :: k,tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        suppress_warning = tempk%is_CC
        call lap_centered(Ax,x,m,k)
        call zeroGhostPoints(Ax)
      end subroutine
      subroutine Lap_uniform_VF(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(∇)
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(VF),intent(inout) :: k,tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        suppress_warning = tempk%is_CC
        call apply_BCs_implicit(x,m)
        call lap_centered(Ax,x,m,k)
        call zeroGhostPoints(Ax)
      end subroutine

      subroutine Lap_nonuniform_props_explicit(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = ∇•(k∇)
        implicit none
        type(SF),intent(inout) :: Ax,x
        type(VF),intent(inout) :: k,tempk
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
        type(VF),intent(inout) :: k,tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = MFP%suppress_warning
        call apply_BCs_implicit(x,m)
        call grad(tempk,x,m)
        call multiply(tempk,k)
        call div(Ax,tempk,m)
      end subroutine

      subroutine ind_diffusion_explicit(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = {I + c_ind ∇x(k∇x)}
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(VF),intent(inout) :: k,tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        call curl(tempk,x,m)
        call multiply(tempk,k)
        call curl(Ax,tempk,m)
        call multiply(Ax,MFP%c_ind)
        call add(Ax,x)
        call zeroGhostPoints(Ax)
      end subroutine
      subroutine ind_diffusion(Ax,x,k,m,MFP,tempk)
        ! COMPUTES:
        !        A = {I + c_ind ∇x(k∇x)}
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(VF),intent(inout) :: k,tempk
        type(mesh),intent(in) :: m
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
        type(SF),intent(inout) :: Ax,x
        type(VF),intent(inout) :: k,tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
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
        type(VF),intent(inout) :: k,tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
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
        type(VF),intent(inout) :: Ax,x
        type(VF),intent(inout) :: k,tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = tempk%is_CC
        ! lap_centered is a very bad and expensive routine. It needs
        ! to be updated (a VF is allocated and deallocated inside).
        ! The reason this is not as simple as the laplacian operator
        ! is because U is staggered, and so k (the intermediate location), 
        ! is staggered AND different for each component, which cannot be
        ! achieved by a scalar field.
        call lap_centered(Ax,x,m,k)
        call multiply(Ax,MFP%c_mom)
        call add(Ax,x)
        call zeroGhostPoints(Ax)
      end subroutine
      subroutine mom_diffusion(Ax,x,k,m,MFP,tempk)
        ! Computes:
        !        A = {I + c_mom ∇•(k∇)}
        implicit none
        type(VF),intent(inout) :: Ax,x
        type(VF),intent(inout) :: k,tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        logical :: suppress_warning
        suppress_warning = tempk%is_CC
        call apply_BCs_implicit(x,m)
        ! lap_centered is a very bad and expensive routine. It needs
        ! to be updated (a VF is allocated and deallocated inside).
        ! The reason this is not as simple as the laplacian operator
        ! is because U is staggered, and so k (the intermediate location), 
        ! is staggered AND different for each component, which cannot be
        ! achieved by a scalar field.
        call lap_centered(Ax,x,m,k)
        call multiply(Ax,MFP%c_mom)
        call add(Ax,x)
        call zeroGhostPoints(Ax)
      end subroutine

      end module