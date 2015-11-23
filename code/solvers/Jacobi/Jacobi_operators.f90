      module Jacobi_operators_mod
      use mesh_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod

      implicit none

      private
      public :: compute_Au
      interface compute_Au;   module procedure compute_Au_SF;    end interface
      interface compute_Au;   module procedure compute_Au_VF;    end interface

      public :: compute_LU
      interface compute_LU;   module procedure compute_LU_SF;    end interface
      interface compute_LU;   module procedure compute_LU_VF;    end interface

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

      subroutine compute_Au_SF(Au,u,k,m,temp) ! ∇•(∇u)
        implicit none
        type(SF),intent(inout) :: Au
        type(VF),intent(inout) :: temp
        type(VF),intent(in) :: k
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        call grad(temp,u,m)
        call multiply(temp,k)
        call div(Au,temp,m)
      end subroutine

      subroutine compute_Au_VF(Au,u,k,m,temp) ! ∇•(∇u)
        implicit none
        type(VF),intent(inout) :: Au
        type(VF),intent(inout) :: temp
        type(VF),intent(in) :: k
        type(VF),intent(in) :: u
        type(mesh),intent(in) :: m
        call curl(temp,u,m)
        call multiply(temp,k)
        call curl(Au,temp,m)
      end subroutine

      subroutine compute_LU_SF(LU,u,k,D,m,tempk,temp)
        implicit none
        type(SF),intent(inout) :: LU,temp
        type(SF),intent(in) :: u,D
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        call compute_Au(LU,u,k,m,tempk)
        call multiply(temp,D,u)
        call subtract(LU,temp)
      end subroutine

      subroutine compute_LU_VF(LU,u,k,D,m,temp)
        implicit none
        type(VF),intent(inout) :: LU
        type(VF),intent(inout) :: temp
        type(VF),intent(in) :: u,D
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        call compute_Au(LU,u,k,m,temp)
        call multiply(temp,D,u)
        call subtract(LU,temp)
      end subroutine

      end module