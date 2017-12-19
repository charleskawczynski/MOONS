       module level_set_solver_mod
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
       use apply_BCs_mod
       use apply_stitches_mod

       implicit none
       private

       public :: Euler_LS

       contains

       subroutine Euler_LS(phi,U,m,dt,temp_F,temp_CC)
         implicit none
         type(SF),intent(inout) :: phi
         type(VF),intent(inout) :: U,temp_F
         type(SF),intent(inout) :: temp_CC
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         call cellCenter2Face(temp_F,phi,m)
         call multiply(temp_F,U)
         call div(temp_CC,temp_F,m)
         call multiply(temp_CC,-1.0_cp*dt)
         call add(phi,temp_CC)
         call apply_BCs(phi,m)
       end subroutine

       end module