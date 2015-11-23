      module Jacobi_operators_mod
      use mesh_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod

      implicit none

      private
      public :: compute_Au
      public :: compute_LU
      public :: compute_Dinv

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

      subroutine compute_Dinv_SF(Dinv,operator,m)
        implicit none
        external :: operator
        type(SF),intent(in) :: Dinv
        type(mesh),intent(in) :: m
        call get_diagonal(Dinv,operator,m)
        call divide(1.0_cp,Dinv)
        call zeroGhostPoints(Dinv)
      end subroutine

      subroutine compute_Dinv_VF(Dinv,operator,m)
        implicit none
        external :: operator
        type(VF),intent(in) :: Dinv
        type(mesh),intent(in) :: m
        call get_diagonal(Dinv,operator,m)
        call divide(1.0_cp,Dinv)
        call zeroGhostPoints(Dinv)
      end subroutine

      subroutine compute_Au_SF(Au,u,k,m) ! ∇•(k∇u)
        implicit none
        type(SF),intent(inout) :: Au
        type(SF),intent(in) :: u,k
        type(mesh),intent(in) :: m
        call lap(Au,u,k,m)
      end subroutine

      subroutine compute_Au_SF_uniform(Au,u,m,temp) ! ∇•(∇u)
        implicit none
        type(SF),intent(inout) :: Au
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        type(SF),intent(inout) :: temp
        call grad(temp,u,m)
        call multiply(temp,k)
        call div(Au,temp,m)
      end subroutine

      subroutine compute_LU(LU,u,m)
        implicit none
        type(SF),intent(inout) :: LU
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        type(SF) :: D
        call init(D,u)
        call compute_Au(Au,u,m)
        call subtract(Au,D)
        call delete(D)
      end subroutine

      end module