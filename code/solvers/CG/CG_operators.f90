      module CG_operators_mod
      use mesh_mod
      use ops_discrete_mod
      use ops_discrete_local_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use ops_interp_mod
      implicit none

      private
      public :: Laplacian_uniform_props
      public :: Laplacian_nonuniform_props
      public :: B_diff_nat_stag
      public :: B_diff_CC
      public :: B_diff_nat_stag_diag

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      contains

      subroutine Laplacian_uniform_props(Ax,x,vol,m,tempk,c)
        ! Computes:   A = ∇²
        implicit none
        type(SF),intent(inout) :: Ax
        type(SF),intent(in) :: x,vol
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: tempk
        real(cp),intent(in) :: c
        call grad(tempk,x,m)
        call div(Ax,tempk,m)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
      end subroutine

      subroutine Laplacian_nonuniform_props(Ax,x,vol,m,tempk,c,k)
        ! Computes:   A = ∇•(k∇)
        implicit none
        type(SF),intent(inout) :: Ax
        type(SF),intent(in) :: x,vol
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: tempk
        real(cp),intent(in) :: c
        call grad(tempk,x,m)
        call multiply(tempk,k)
        call div(Ax,tempk,m)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
      end subroutine

      subroutine B_diff_nat_stag(Ax,x,vol,m,tempk,c,k)
        ! Computes:   A = V {I + dt/Rem ∇x(k∇x)}
        ! Where B and J are intended to live at the 
        ! cell face and edge respectively. V is the cell volume.
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x,vol
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: tempk
        real(cp),intent(in) :: c
        call curl(tempk,x,m)
        call multiply(tempk,k)
        call curl(Ax,tempk,m)
        call multiply(Ax,c)
        call add(Ax,x)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
      end subroutine

      subroutine B_diff_nat_stag_diag(Ax,x,vol,m,tempk,c,k,local_index)
        ! Computes:   A = ∇x(k∇x)
        ! Where B and J are intended to live at the 
        ! cell face and edge respectively. V is the cell volume.
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x,vol
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: tempk
        real(cp),intent(in) :: c
        integer,intent(in) :: local_index
        call curl(tempk,x,m,local_index)
        call multiply(tempk,k)
        call curl(Ax,tempk,m,local_index)
      end subroutine

      subroutine B_diff_CC(Ax,x,vol,m,tempk,c,k)
        ! Computes:   A = {I + dt/Rem ∇x(k∇x)}
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x,vol
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: tempk
        real(cp),intent(in) :: c
        call curlcurl(Ax,x,k,tempk,m)
        call multiply(Ax,c)
        call add(Ax,x)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
      end subroutine

      end module