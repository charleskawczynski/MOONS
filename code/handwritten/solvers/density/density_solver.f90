       module density_solver_mod
       use current_precision_mod
       use mesh_extend_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use norms_mod
       use AB2_mod
       use compute_energy_mod
       use export_raw_processed_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_advect_mod
       use ops_norms_mod
       use apply_BCs_mod
       use PCG_mod
       use matrix_free_operators_mod

       implicit none
       private

       public :: Euler

       contains

       ! **********************************************************************
       ! **********************************************************************
       ! *********************** EXPLICIT TIME MARCHING ***********************
       ! **********************************************************************
       ! **********************************************************************

       subroutine Euler(rho,U,dt,m,temp_CC1,temp_F)
         ! Solves
         !             ∂ρ/∂t + ∇•(ρu) = 0
         implicit none
         type(SF),intent(inout) :: rho,temp_CC1
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U
         real(cp),intent(in) :: dt
         type(VF),intent(inout) :: temp_F
         call cellCenter2Face(temp_F,rho,m)
         call multiply(temp_F,U)
         call div(temp_CC1,temp_F,m)
         call multiply(temp_CC1,-dt)
         call add(rho,temp_CC1)
         call apply_BCs(rho)
       end subroutine

       end module