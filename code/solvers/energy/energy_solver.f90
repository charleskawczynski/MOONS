       module energy_solver_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use domain_mod
       use norms_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use apply_BCs_mod
       use PCG_mod
       use PCG_solver_mod

       implicit none

       private
       public :: explicitEuler
       public :: explicitEuler_with_source
       public :: diffusion_implicit
       public :: all_terms_explicit

       contains

       subroutine explicitEuler(T,U_F,dt,Re,Pr,m,temp_CC1,temp_CC2,temp_F)
         ! Solves
         !             ∂T/∂t + (u • ∇)T = ∇²T
         implicit none
         type(SF),intent(inout) :: T,temp_CC1,temp_CC2
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U_F
         real(cp),intent(in) :: dt,Re,Pr
         type(VF),intent(inout) :: temp_F
         call cellCenter2Face(temp_F,T,m)
         call multiply(temp_F,U_F)
         call div(temp_CC1,temp_F,m)
         call multiply(temp_CC1,-1.0_cp)
         call lap(temp_CC2,T,m)
         call multiply(temp_CC2,1.0_cp/(Re*Pr))
         call add(temp_CC1,temp_CC2)
         call multiply(temp_CC1,dt)
         call add(T,temp_CC1)
         call apply_BCs(T,m)
       end subroutine

       subroutine explicitEuler_with_source(T,U_F,dt,Re,Pr,m,Q_CC,temp_CC1,temp_CC2,temp_F)
         ! Solves
         !             ∂T/∂t + (u • ∇)T = ∇²T + Q
         implicit none
         type(SF),intent(inout) :: T,temp_CC1,temp_CC2
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U_F,Q_CC
         real(cp),intent(in) :: dt,Re,Pr
         type(VF),intent(inout) :: temp_F
         call cellCenter2Face(temp_F,T,m)
         call multiply(temp_F,U_F)
         call div(temp_CC1,temp_F,m)
         call multiply(temp_CC1,-1.0_cp)
         call lap(temp_CC2,T,m)
         call multiply(temp_CC2,1.0_cp/(Re*Pr))
         call add(temp_CC1,temp_CC2)
         call add(temp_CC1,Q_CC)
         call multiply(temp_CC1,dt)
         call add(T,temp_CC1)
         call apply_BCs(T,m)
       end subroutine

       subroutine diffusion_implicit(PCG,T,U_F,dt,Re,Pr,m,n,compute_norms,temp_CC1,temp_CC2,temp_F)
         ! Solves
         !             ∂T/∂t + (u • ∇)T = ∇²T + j²/σ + Φ
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG
         type(SF),intent(inout) :: T,temp_CC1,temp_CC2
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U_F
         real(cp),intent(in) :: dt,Re,Pr
         type(VF),intent(inout) :: temp_F
         logical,intent(in) :: compute_norms
         integer,intent(in) :: n
         call cellCenter2Face(temp_F,T,m)
         call multiply(temp_F,U_F)
         call div(temp_CC1,temp_F,m)
         call multiply(temp_CC1,-1.0_cp)
         call lap(temp_CC2,T,m)
         call multiply(temp_CC2,0.5_cp/(Re*Pr))
         call add(temp_CC1,temp_CC2)
         call multiply(temp_CC1,dt)
         call solve(PCG,T,temp_CC1,m,n,compute_norms)
       end subroutine

       subroutine all_terms_explicit(T,U_F,U_CC,J_CC,sigmaInv_CC,dt,Re,Pr,m,&
         temp_CC1,temp_CC2,temp_F,temp_CC_TF1,temp_CC_TF2)
         ! Solves
         !             ∂T/∂t + (u • ∇)T = ∇²T + j²/σ + Φ
         ! 
         implicit none
         type(SF),intent(inout) :: T,temp_CC1,temp_CC2
         type(SF),intent(in) :: sigmaInv_CC
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U_F,U_CC,J_CC
         TYPE(TF),intent(inout) :: temp_CC_TF1,temp_CC_TF2
         real(cp),intent(in) :: dt,Re,Pr
         type(VF),intent(inout) :: temp_F
         ! Convection
         call cellCenter2Face(temp_F,T,m)
         call multiply(temp_F,U_F)
         call div(temp_CC1,temp_F,m)
         call multiply(temp_CC1,-1.0_cp)
         ! Thermal diffusion
         call lap(temp_CC2,T,m)
         call multiply(temp_CC2,1.0_cp/(Re*Pr))
         call add(temp_CC1,temp_CC2)
          ! Joule dissipation
         call multiply(temp_CC_TF2%x,J_CC,sigmaInv_CC)
         call add(temp_CC1,temp_CC_TF2%x)
         ! Viscous dissipation
         call grad(temp_CC_TF1,U_CC,m)
         call transpose(temp_CC_TF2,temp_CC_TF1)
         call add(temp_CC_TF2,temp_CC_TF1)
         call square(temp_CC_TF2) ! S_ij S_ij in tensor format
         call add(temp_CC2,temp_CC_TF2) ! S_ij S_ij in scalar format
         call multiply(temp_CC2,0.5_cp) ! temp_CC1 = 0.5 (∂_j u_i + ∂_i u_j) (∂_j u_i + ∂_i u_j)
         call add(temp_CC1,temp_CC2)
         ! Explicit Euler
         call multiply(temp_CC1,dt)
         call add(T,temp_CC1)
         call apply_BCs(T,m)
       end subroutine

       end module