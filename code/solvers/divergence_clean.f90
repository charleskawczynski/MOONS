       module divergence_clean_mod
       ! Computes: X = X* - ∇φ,   ∇²φ = ∇•X*
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use ops_aux_mod
       use ops_discrete_mod
       use apply_BCs_mod
       use PCG_mod
       use GS_poisson_mod

       implicit none
       private

       public :: div_clean_PCG
       public :: div_clean_GS

       contains

       subroutine div_clean_PCG(PCG,U,p,Ustar,m,temp_F1,temp_CC,compute_norms)
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_F1,Ustar
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call zeroWall_conditional(Ustar,m,U)
         call div(temp_CC,Ustar,m)
         call solve(PCG,p,temp_CC,m,compute_norms)
         call grad(temp_F1,p,m)
         call subtract(U,Ustar,temp_F1)
         call apply_BCs(U,m)
       end subroutine

       subroutine div_clean_GS(GS,U,p,m,Ustar,temp_F1,temp_CC,compute_norms)
         implicit none
         type(GS_Poisson_SF),intent(inout) :: GS
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_F1,Ustar
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call zeroWall_conditional(Ustar,m,U)
         call div(temp_CC,Ustar,m)
         call solve(GS,p,temp_CC,m,compute_norms)
         call grad(temp_F1,p,m)
         call subtract(U,Ustar,temp_F1)
         call apply_BCs(U,m)
       end subroutine

       end module