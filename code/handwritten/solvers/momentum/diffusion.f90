       module diffusion_mod
       use current_precision_mod
       use mesh_extend_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_advect_mod
       use ops_norms_mod

       implicit none
       private

       public :: diffusion_uniform_viscosity
       public :: diffusion_variable_viscosity

       contains

       subroutine add_diffusion_uniform_viscosity(F,U,coefficient,m,temp_F)
         implicit none
         type(VF),intent(inout) :: F
         type(VF),intent(in) :: U
         real(cp),intent(in) :: coefficient
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_F
         call lap(temp_F,U,m)
         call multiply(temp_F,coefficient)
         call add(F,temp_F)
       end subroutine

       subroutine add_diffusion_uniform_viscosity_centered(F,U,coefficient,m,temp_F,temp_E)
         ! Seems to work better for stitching, but O(dx^1) on boundaries
         implicit none
         type(VF),intent(inout) :: F
         type(VF),intent(in) :: U
         real(cp),intent(in) :: coefficient
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_F,temp_E
         call lap_centered(temp_F,U,m,temp_E)
         call multiply(temp_F,coefficient)
         call add(F,temp_F)
       end subroutine

       subroutine diffusion_variable_viscosity(F,U,mu_E,mu_CC,coefficient,m,CC_diag_E_off_diag,temp_F1,temp_F2)
         implicit none
         type(VF),intent(in) :: U,mu_E
         type(SF),intent(in) :: mu_CC
         type(VF),intent(inout) :: F
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: coefficient
         type(VF),intent(inout) :: temp_F1,temp_F2
         type(TF),intent(inout) :: CC_diag_E_off_diag
         call grad(CC_diag_E_off_diag,U,m)
         call multiply_diag(CC_diag_E_off_diag,mu_CC)
         call multiply_off_diag(CC_diag_E_off_diag,mu_E)
         call div(temp_F1,CC_diag_E_off_diag,m)

         call grad(CC_diag_E_off_diag,U,m)
         call transpose(CC_diag_E_off_diag)
         call multiply_diag(CC_diag_E_off_diag,mu_CC)
         call multiply_off_diag(CC_diag_E_off_diag,mu_E)
         call div(temp_F2,CC_diag_E_off_diag,m)

         call add(temp_F1,temp_F2)

         call multiply(temp_F1,coefficient)
         call add(F,temp_F1)
       end subroutine

       end module