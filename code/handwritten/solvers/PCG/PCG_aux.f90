      module PCG_aux_mod
      use current_precision_mod
      use mesh_extend_mod
      use apply_BCs_mod
      use ops_discrete_mod
      use ops_aux_mod
      use bctype_mod
      use export_raw_processed_mod
      use SF_extend_mod
      use VF_extend_mod
      use TF_mod
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

      subroutine modify_RHS_SF(operator,operator_explicit,x,x_BC,b,vol,k,m,&
        MFP,tempx,tempk,Ax,r)
        implicit none
        procedure(op_SF) :: operator
        procedure(op_SF_explicit) :: operator_explicit
        type(SF),intent(inout) :: x,x_BC
        type(SF),intent(in) :: b,vol
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        type(SF),intent(inout) :: tempx,Ax,r
        call assign(r,b)                              ! r = b
        if (x%all_Neumann) call subtract_physical_mean(r,vol,tempx) ! Not sure correct loc
        call compute_Ax_BC(operator_explicit,tempx,x,x_BC,k,m,MFP,tempk)
        call subtract(r,tempx)                        ! r = (b_mod - Ax_BC - Ax)
        call operator(Ax,x,k,m,MFP,tempk)
        call subtract(r,Ax)                           ! r = (b_mod - Ax_BC - Ax_mod)
        call multiply(r,vol)                          ! r = vol*(b_mod - Ax_BC - Ax_mod)
        if (.not.is_CC(x)) then
          call assign_wall_Periodic_single(r,0.0_cp,x_BC)
          call assign_wall_Dirichlet(r,0.0_cp,x_BC)
          call multiply_wall_Neumann(r,0.5_cp,x_BC)     ! To make A symmetric
        endif
      end subroutine

      subroutine modify_RHS_VF(operator,operator_explicit,x,x_BC,b,vol,k,m,&
        MFP,tempx,tempk,Ax,r)
        implicit none
        procedure(op_VF) :: operator
        procedure(op_VF_explicit) :: operator_explicit
        type(VF),intent(inout) :: x,x_BC
        type(VF),intent(in) :: b,vol
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        type(VF),intent(inout) :: tempx,Ax,r
        call assign(r,b)                              ! r = b
        call compute_Ax_BC(operator_explicit,tempx,x,x_BC,k,m,MFP,tempk)
        call subtract(r,tempx)                        ! r = (b_mod - Ax_BC - Ax)
        call operator(Ax,x,k,m,MFP,tempk)
        call subtract(r,Ax)                           ! r = (b_mod - Ax_BC - Ax_mod)
        call multiply(r,vol)                          ! r = vol*(b_mod - Ax_BC - Ax_mod)
        if (.not.is_CC(x)) then
          call assign_wall_Periodic_single(r,0.0_cp,x_BC)
          call assign_wall_Dirichlet(r,0.0_cp,x_BC)
          call multiply_wall_Neumann(r,0.5_cp,x_BC)     ! To make A symmetric
        endif
      end subroutine

      subroutine compute_Ax_BC_MF_SF(operator_explicit,Ax_BC,x,x_BC,k,m,MFP,tempk)
        implicit none
        procedure(op_SF_explicit) :: operator_explicit
        type(SF),intent(inout) :: Ax_BC,x_BC
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(SF),intent(in) :: x
        type(matrix_free_params),intent(in) :: MFP
        type(mesh),intent(in) :: m
        call compute_x_BC(x_BC)
        call operator_explicit(Ax_BC,x_BC,k,m,MFP,tempk)
        call assign_ghost_XPeriodic(Ax_BC,0.0_cp,x)
      end subroutine

      subroutine compute_Ax_BC_MF_VF(operator_explicit,Ax_BC,x,x_BC,k,m,MFP,tempk)
        implicit none
        procedure(op_VF_explicit) :: operator_explicit
        type(VF),intent(inout) :: Ax_BC,x_BC
        type(VF),intent(in) :: x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(matrix_free_params),intent(in) :: MFP
        type(mesh),intent(in) :: m
        call compute_x_BC(x_BC)
        call operator_explicit(Ax_BC,x_BC,k,m,MFP,tempk)
        call assign_ghost_XPeriodic(Ax_BC,0.0_cp,x)
      end subroutine

      subroutine compute_x_BC_SF(x_BC)
        implicit none
        type(SF),intent(inout) :: x_BC
        call assign(x_BC,0.0_cp)
        call apply_BCs(x_BC)
        call assign_ghost_N_XPeriodic(x_BC,0.0_cp)
      end subroutine

      subroutine compute_x_BC_VF(x_BC)
        implicit none
        type(VF),intent(inout) :: x_BC
        call assign(x_BC,0.0_cp)
        call apply_BCs(x_BC)
        call assign_ghost_N_XPeriodic(x_BC,0.0_cp)
      end subroutine

      end module