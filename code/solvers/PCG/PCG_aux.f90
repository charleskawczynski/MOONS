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
      public :: modify_RHS
      interface modify_RHS; module procedure modify_RHS_SF; end interface
      interface modify_RHS; module procedure modify_RHS_VF; end interface

      public :: compute_Ax_BC
      interface compute_Ax_BC; module procedure compute_Ax_BC_MF_SF; end interface
      interface compute_Ax_BC; module procedure compute_Ax_BC_MF_VF; end interface

      interface compute_x_BC; module procedure compute_x_BC_SF; end interface
      interface compute_x_BC; module procedure compute_x_BC_VF; end interface

      contains

      subroutine modify_RHS_SF(operator,operator_explicit,x,b,vol,k,m,&
        MFP,tempx,tempk,Ax,r,p)
        implicit none
        procedure(op_SF) :: operator
        procedure(op_SF_explicit) :: operator_explicit
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b,vol
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        type(SF),intent(inout) :: tempx,Ax,r,p
        call assign(r,b)                              ! r = b
        call multiply_wall_Neumann(r,0.5_cp,x)        ! r = b_mod
        if (x%all_Neumann) call subtract_physical_mean(r,vol,tempx) ! Not sure correct loc
        call compute_Ax_BC(operator_explicit,tempx,p,x,k,m,MFP,tempk)
        call subtract(r,tempx)                        ! r = (b_mod - Ax_BC - Ax)
        ! if (x%all_Neumann) call subtract_physical_mean(r,vol,tempx) ! Not sure correct loc
        call operator(Ax,x,k,m,MFP,tempk)
        call multiply_wall_Neumann(Ax,0.5_cp,x)
        call subtract(r,Ax)                           ! r = (b_mod - Ax_BC - Ax_mod)
        call multiply(r,vol)                          ! r = vol*(b_mod - Ax_BC - Ax_mod)
        if (.not.is_CC(x)) call assign_wall_Dirichlet(r,0.0_cp,x)
      end subroutine

      subroutine modify_RHS_VF(operator,operator_explicit,x,b,vol,k,m,&
        MFP,tempx,tempk,Ax,r,p)
        implicit none
        procedure(op_VF) :: operator
        procedure(op_VF_explicit) :: operator_explicit
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,vol
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        type(VF),intent(inout) :: tempx,Ax,r,p
        call assign(r,b)                              ! r = b
        call multiply_wall_Neumann(r,0.5_cp,x)        ! r = b_mod
        ! if (x%all_Neumann) call subtract_physical_mean(r,vol,tempx) ! Not sure correct loc
        call compute_Ax_BC(operator_explicit,tempx,p,x,k,m,MFP,tempk)
        call subtract(r,tempx)                        ! r = (b_mod - Ax_BC - Ax)
        ! if (x%all_Neumann) call subtract_physical_mean(r,vol,tempx) ! Not sure correct loc
        call operator(Ax,x,k,m,MFP,tempk)
        call multiply_wall_Neumann(Ax,0.5_cp,x)
        call subtract(r,Ax)                           ! r = (b_mod - Ax_BC - Ax_mod)
        call multiply(r,vol)                          ! r = vol*(b_mod - Ax_BC - Ax_mod)
        if (.not.is_CC(x)) call assign_wall_Dirichlet(r,0.0_cp,x)
      end subroutine

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