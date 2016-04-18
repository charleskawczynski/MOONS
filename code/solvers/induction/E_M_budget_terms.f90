       module E_M_budget_terms_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_norms_mod
       use norms_mod

       implicit none

       private
       public :: unsteady
       public :: Lorentz
       public :: Joule_Dissipation
       public :: Poynting
       public :: Flux
       public :: E_M_Convection
       public :: maxwell_stress

       contains

       subroutine unsteady(e,B,Bnm1,dt,m,temp_F,temp_CC)
         ! Computes
         !       ∂  B²
         ! e = ∫ -- -- dV
         !       ∂t 2μ
         ! Assumes μ is constant.
         implicit none
         type(VF),intent(in) :: B,Bnm1
         real(cp),intent(in) :: dt
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_F,temp_CC
         real(cp),intent(inout) :: e
         call subtract(temp_F,B,Bnm1)
         call multiply(temp_F,0.5_cp/dt)
         call face2CellCenter(temp_CC,temp_F,m)
         call Ln(e,temp_CC,1.0_cp,m)
       end subroutine

       subroutine Lorentz(e,J,B,U_CC,m,temp_CC1,temp_CC2,temp_CC3,temp_F)
         ! Computes
         ! 
         ! e = ∫ u•(jxB) dV
         ! 
         implicit none
         type(VF),intent(in) :: J,B,U_CC
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_CC1,temp_CC2,temp_CC3,temp_F
         real(cp),intent(inout) :: e
         call edge2CellCenter(temp_CC1,J,m,temp_F)
         call face2CellCenter(temp_CC2,B,m)
         call cross(temp_CC3,temp_CC1,temp_CC2)
         call multiply(temp_CC3,U_CC)
         call Ln(e,temp_CC3,1.0_cp,m)
       end subroutine

       subroutine E_M_Convection(e,B,U_CC,m,temp_CC1,temp_CC_SF)
         ! Computes
         ! 
         ! e = ∫ (u•∇)E_m dV
         ! 
         implicit none
         type(VF),intent(in) :: B,U_CC
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp_CC_SF
         type(VF),intent(inout) :: temp_CC1
         real(cp),intent(inout) :: e
         call face2CellCenter(temp_CC1,B,m)
         call square(temp_CC1)
         call add(temp_CC_SF,temp_CC1)
         call multiply(temp_CC_SF,0.5_cp)
         call grad(temp_CC1,temp_CC_SF,m)
         call multiply(temp_CC1,U_CC)
         call Ln(e,temp_CC1,1.0_cp,m)
       end subroutine

       subroutine maxwell_stress(e,B,U_CC,m,temp_CC1,temp_CC_TF)
         ! Computes
         ! 
         ! e = ∫ u•(B•∇(B/μ)) dV
         ! 
         ! Assumes μ is constant.
         implicit none
         type(VF),intent(in) :: B,U_CC
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_CC1
         type(TF),intent(inout) :: temp_CC_TF
         real(cp),intent(inout) :: e
         call face2CellCenter(temp_CC1,B,m)
         call grad(temp_CC_TF,temp_CC1,m)
         call multiply(temp_CC_TF,temp_CC1)
         call add(temp_CC1,temp_CC_TF)
         call multiply(temp_CC_TF%x,temp_CC1,U_CC)
         call Ln(e,temp_CC_TF%x,1.0_cp,m)
       end subroutine

       subroutine Joule_Dissipation(e,J,sigmaInv_CC,m,temp_CC,temp_F)
         ! Computes
         !       j²
         ! e = ∫ - dV
         !       σ
         implicit none
         type(VF),intent(in) :: J
         type(SF),intent(in) :: sigmaInv_CC
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_CC,temp_F
         real(cp),intent(inout) :: e
         call edge2CellCenter(temp_CC,J,m,temp_F)
         call square(temp_CC)
         call multiply(temp_CC,sigmaInv_CC)
         call Ln(e,temp_CC,1.0_cp,m)
       end subroutine

       subroutine Poynting(e,B,J,U,sigmaInv_F,m,&
         temp_CC,temp_F,temp_F1_TF,temp_F2_TF,temp_F3_TF)
         ! Computes
         ! 
         ! e = ∫ ( E x B μ⁻¹) • n dA
         ! 
         ! Assumes μ is constant.
         implicit none
         type(VF),intent(in) :: B,J,U
         type(VF),intent(in) :: sigmaInv_F
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp_CC
         type(VF),intent(inout) :: temp_F
         type(TF),intent(inout) :: temp_F1_TF,temp_F2_TF,temp_F3_TF
         real(cp),intent(inout) :: e
         ! Compute uxB in E = j/sig - uxB
         call face2Face(temp_F1_TF,U,m,temp_CC) ! U to tensor field
         call face2Face(temp_F2_TF,B,m,temp_CC) ! B to tensor field
         call cross(temp_F,temp_F1_TF,temp_F2_TF) ! uxB
         call face2Face(temp_F1_TF,temp_F,m,temp_CC) ! uxB to tensor field

         call edge2Face_no_diag(temp_F3_TF,J,m) ! J to tensor field
         call multiply(temp_F3_TF%x,sigmaInv_F) ! J/sig
         call multiply(temp_F3_TF%y,sigmaInv_F) ! J/sig
         call multiply(temp_F3_TF%z,sigmaInv_F) ! J/sig

         call subtract(temp_F3_TF,temp_F1_TF) ! E = j/sig - uxB
         call cross(temp_F,temp_F3_TF,temp_F2_TF) ! temp_F = E x B
         call boundaryFlux(e,temp_F,m) ! Computes ∫∫ u•n dA
       end subroutine

       subroutine Flux(e,B,U,m,temp_F,temp_CC,temp_CC_SF)
         ! Computes
         ! 
         !        B²
         ! e = ∫∫ -- (u • n) dA
         !        2μ
         ! 
         ! Assumes μ is constant.
         implicit none
         type(VF),intent(in) :: B,U
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp_CC_SF
         type(VF),intent(inout) :: temp_F,temp_CC
         real(cp),intent(inout) :: e
         call multiply(temp_F,B,B)
         call multiply(temp_F,0.5_cp)
         call face2CellCenter(temp_CC,temp_F,m)
         call add(temp_CC_SF,temp_CC)
         call multiply(temp_F,U,temp_CC_SF) ! temp_F = U B²/2
         call boundaryFlux(e,temp_F,m) ! Computes ∫∫ u•n dA
       end subroutine

       end module