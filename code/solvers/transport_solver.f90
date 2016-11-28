       module transport_solver_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_aux_mod
       use ops_interp_mod
       use divergence_clean_mod
       use ops_discrete_mod
       use ops_norms_mod
       use apply_BCs_mod
       use norms_mod
       use PCG_mod
       use divergence_clean_mod
       use mesh_domain_mod

       implicit none

       private
       public :: transport_solver

       contains

       subroutine transport_solver(PCG_X,PCG_phi,X,phi,RHS,m,temp_F,temp_CC,compute_norms)
         ! Solves the general equation:
         !
         !    ∂X/∂t = AX + F + c∇φ
         !
         ! Let:
         ! θ = temporal treatment of A: 0 (explicit) < θ <  1   (implicit)
         ! c = temporal treatment of φ: 0 (implicit) < c < 1/2 (trapezoidal)
         ! α = (1-θ)*dt
         ! β = (1-c)*dt
         ! γ = 1/((1-c)*dt)
         !
         ! (1 + α*A)*X_{*} = K_{n}
         ! ∇²φ_{n+1} = γ∇•K_{n}
         ! X_{n+1} = X_{*} - β*∇φ_{n+1}
         !
         ! K_{n} = dt*(F_{n} + θ*AX_{n} - c*∇φ_{n}) + X_{n}
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_X
         type(PCG_solver_SF),intent(inout) :: PCG_phi
         type(VF),intent(inout) :: X,temp_F
         type(SF),intent(inout) :: phi,temp_CC
         type(VF),intent(in) :: RHS
         type(mesh),intent(in) :: m
         logical,intent(in) :: compute_norms
         call solve(PCG_X,X,RHS,m,compute_norms)
         call clean_div(PCG_phi,X,phi,m,temp_F,temp_CC,compute_norms)
       end subroutine

       end module