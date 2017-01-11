      module PCG_aux_mod
      use current_precision_mod
      use mesh_mod
      use apply_BCs_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use matrix_free_params_mod
      use matrix_free_operators_mod
      implicit none

      private
      public :: compute_Ax_BC
      interface compute_Ax_BC; module procedure compute_Ax_BC_MF_SF; end interface
      interface compute_Ax_BC; module procedure compute_Ax_BC_MF_VF; end interface

      interface compute_x_BC; module procedure compute_x_BC_SF; end interface
      interface compute_x_BC; module procedure compute_x_BC_VF; end interface

      contains

      subroutine compute_Ax_BC_MF_SF(operator_explicit,Ax_BC,p,x,k,m,MFP,tempk)
        implicit none
        procedure(op_SF_explicit) :: operator_explicit
        type(SF),intent(inout) :: Ax_BC,p
        type(VF),intent(inout) :: tempk
        type(VF),intent(in) :: k
        type(SF),intent(in) :: x
        type(matrix_free_params),intent(in) :: MFP
        type(mesh),intent(in) :: m
        call compute_x_BC(p)
        call operator_explicit(Ax_BC,p,k,m,MFP,tempk)
        call assign_wall_Dirichlet(Ax_BC,0.0_cp,x)
        call assign_ghost_XPeriodic(Ax_BC,0.0_cp,x)
      end subroutine

      subroutine compute_Ax_BC_MF_VF(operator_explicit,Ax_BC,p,x,k,m,MFP,tempk)
        implicit none
        procedure(op_VF_explicit) :: operator_explicit
        type(VF),intent(inout) :: Ax_BC,p
        type(VF),intent(inout) :: tempk
        type(VF),intent(in) :: k,x
        type(matrix_free_params),intent(in) :: MFP
        type(mesh),intent(in) :: m
        call compute_x_BC(p)
        call operator_explicit(Ax_BC,p,k,m,MFP,tempk)
        call assign_wall_Dirichlet(Ax_BC,0.0_cp,x)
        call assign_ghost_XPeriodic(Ax_BC,0.0_cp,x)
      end subroutine

      subroutine compute_x_BC_SF(p)
        implicit none
        type(SF),intent(inout) :: p
        call assign(p,0.0_cp)
        call apply_BCs(p)
        call assign_ghost_N_XPeriodic(p,0.0_cp)
      end subroutine

      subroutine compute_x_BC_VF(p)
        implicit none
        type(VF),intent(inout) :: p
        call assign(p,0.0_cp)
        call apply_BCs(p)
        call assign_ghost_N_XPeriodic(p,0.0_cp)
      end subroutine

      end module