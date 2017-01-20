       module clean_divergence_mod
       ! Computes:
       !      ∇²φ_{n+1} = ∇•K_{n}
       !      X_{n+1} = X_{*} - ∇φ_{n+1}
       ! Where:
       !      X* = dt*(F_{n} + θ*AX_{n} + explicit terms) + X_{n}
       !
       ! According to:
       !        Kim, J. & Moin, P. Application of a Fractional-Step
       !        Method to Incompressible Naview-Stokes Equations.
       !        J. Comput. Phys. 323, 308–323 (1985).
       !
       ! 2nd order time marching is achieved using fully
       ! implicit time marching for pressure.
       ! See section 2, after Eq. 5 and before Fig. 1.
       !
       ! We prefer this over trapezoidal method as this
       ! method requires the simplest interface.
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use ops_discrete_mod
       use apply_BCs_mod
       use PCG_mod
       use GS_Poisson_mod

       implicit none
       private
       public :: clean_div
       interface clean_div;    module procedure clean_div_PCG;    end interface
       interface clean_div;    module procedure clean_div_PCG_MG; end interface
       interface clean_div;    module procedure clean_div_GS;     end interface
       interface clean_div;    module procedure clean_div_GS_MG;  end interface

       contains

       subroutine clean_div_PCG(PCG,X,Xstar,phi,m,temp_F,temp_CC,compute_norms)
         ! Computes:
         !      ∇²φ_{n+1} = ∇•K_{n}
         !      X_{n+1} = X_{*} - ∇φ_{n+1}
         ! Where:
         !      X* = dt*(F_{n} + θ*AX_{n} + explicit terms) + X_{n}
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG
         type(SF),intent(inout) :: phi
         type(VF),intent(in) :: Xstar
         type(VF),intent(inout) :: X,temp_F
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call div(temp_CC,Xstar,m)
         call solve(PCG,phi,temp_CC,m,compute_norms)
         call grad(temp_F,phi,m)
         call subtract(X,Xstar,temp_F)
         call apply_BCs(X)
       end subroutine

       subroutine clean_div_PCG_MG(PCG,X,Xstar,phi,MG,m,temp_F,temp_CC,compute_norms)
         ! Computes:
         !      ∇²φ_{n+1} = ∇•K_{n}
         !      X_{n+1} = X_{*} - ∇φ_{n+1} - MG
         ! Where:
         !      X* = dt*(F_{n} + θ*AX_{n} + explicit terms) + X_{n}
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG
         type(SF),intent(inout) :: phi
         type(VF),intent(in) :: Xstar
         type(VF),intent(inout) :: X,temp_F
         type(VF),intent(in) :: MG
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call div(temp_CC,Xstar,m)
         call solve(PCG,phi,temp_CC,m,compute_norms)
         call grad(temp_F,phi,m)
         call subtract(X,Xstar,temp_F)
         call subtract(X,MG)
         call apply_BCs(X)
       end subroutine


       subroutine clean_div_GS(GS,X,Xstar,phi,m,temp_F,temp_CC,compute_norms)
         ! Computes:
         !      ∇²φ_{n+1} = ∇•K_{n}
         !      X_{n+1} = X_{*} - ∇φ_{n+1}
         ! Where:
         !      X* = dt*(F_{n} + θ*AX_{n} + explicit terms) + X_{n}
         implicit none
         type(GS_Poisson_SF),intent(inout) :: GS
         type(SF),intent(inout) :: phi
         type(VF),intent(in) :: Xstar
         type(VF),intent(inout) :: X,temp_F
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call div(temp_CC,Xstar,m)
         call solve(GS,phi,temp_CC,m,compute_norms)
         call grad(temp_F,phi,m)
         call subtract(X,Xstar,temp_F)
         call apply_BCs(X)
       end subroutine

       subroutine clean_div_GS_MG(GS,X,Xstar,phi,MG,m,temp_F,temp_CC,compute_norms)
         ! Computes:
         !      ∇²φ_{n+1} = ∇•K_{n}
         !      X_{n+1} = X_{*} - ∇φ_{n+1} - MG
         ! Where:
         !      X* = dt*(F_{n} + θ*AX_{n} + explicit terms) + X_{n}
         implicit none
         type(GS_Poisson_SF),intent(inout) :: GS
         type(SF),intent(inout) :: phi
         type(VF),intent(in) :: Xstar
         type(VF),intent(inout) :: X,temp_F
         type(VF),intent(in) :: MG
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call div(temp_CC,Xstar,m)
         call solve(GS,phi,temp_CC,m,compute_norms)
         call grad(temp_F,phi,m)
         call subtract(X,Xstar,temp_F)
         call subtract(X,MG)
         call apply_BCs(X)
       end subroutine

       end module