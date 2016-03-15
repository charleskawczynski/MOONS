       module E_K_budget_terms_mod
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
       public :: Convection
       public :: Transport
       public :: Viscous_Dissipation
       public :: Lorentz

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

       subroutine unsteady(e,U,Unm1,dt,m,temp_F,temp_CC)
         ! Computes
         !       ∂    u²
         ! e = ∫ -- ρ -- dV
         !       ∂t   2
         ! Assumes ρ is constant (and 1).
         implicit none
         type(VF),intent(in) :: U,Unm1
         real(cp),intent(in) :: dt
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_F,temp_CC
         real(cp),intent(inout) :: e
         call subtract(temp_F,U,Unm1)
         call multiply(temp_F,0.5_cp/dt)
         call face2CellCenter(temp_CC,temp_F,m)
         call Ln(e,temp_CC,1.0_cp,m)
       end subroutine

       subroutine Convection(e,U,U_CC,m,&
         temp_F1,temp_F2,temp_CC,temp_CC_SF)
         ! Computes
         ! 
         ! e = ∫ (u • ∇) E_K dV
         ! 
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: U,U_CC
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_CC
         type(SF),intent(inout) :: temp_CC_SF
         call dot(temp_CC_SF,U_CC,U_CC,temp_CC)
         call multiply(temp_CC_SF,0.5_cp)
         call grad(temp_F1,temp_CC_SF,m)
         call multiply(temp_F2,temp_F1,U)
         call face2CellCenter(temp_CC,temp_F2,m)
         call Ln(e,temp_CC,1.0_cp,m)
       end subroutine

       subroutine Transport(e,U_CC,p,m,&
         temp_CC1_TF,temp_CC2_TF)
         ! Computes
         ! 
         ! e = ∫ ∂_i ( u_i p - 2 μ u_j S_ij ) dV
         ! 
         ! Assumes μ is constant.
         implicit none
         type(VF),intent(in) :: U_CC
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         type(TF),intent(inout) :: temp_CC1_TF,temp_CC2_TF
         real(cp),intent(inout) :: e
         call grad(temp_CC1_TF,U_CC,m)
         call transpose(temp_CC2_TF,temp_CC1_TF)
         call add(temp_CC1_TF,temp_CC2_TF)
         call multiply(temp_CC1_TF,U_CC)
         call add(temp_CC2_TF%x,temp_CC1_TF)
         call multiply(temp_CC1_TF%x,U_CC,p)
         call subtract(temp_CC1_TF%x,temp_CC2_TF%x)
         call div(temp_CC2_TF%x%x,temp_CC1_TF%x,m)
         call Ln(e,temp_CC2_TF%x%x,1.0_cp,m)
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

       subroutine Viscous_Dissipation(e,U_CC,m,&
         temp_CC1_TF,temp_CC2_TF)
         ! Computes
         ! 
         ! e = ∫ 2 μ S_ij S_ij dV
         ! 
         ! Where S_ij = 0.5 (∂_j u_i + ∂_i u_j)
         implicit none
         type(VF),intent(in) :: U_CC
         type(mesh),intent(in) :: m
         type(TF),intent(inout) :: temp_CC1_TF,temp_CC2_TF
         real(cp),intent(inout) :: e
         call grad(temp_CC1_TF,U_CC,m)
         call transpose(temp_CC2_TF,temp_CC1_TF)
         call add(temp_CC2_TF,temp_CC1_TF)
         call square(temp_CC2_TF) ! S_ij S_ij in tensor format
         call Ln(e,temp_CC2_TF,1.0_cp,m)
         e = 0.5_cp*e
       end subroutine

       end module