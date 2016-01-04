       module induction_solver_mod
       ! Constrained Transport (CT) Method reference:
       ! "Tóth, G. The divergence Constraint in Shock-Capturing 
       ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_discrete_complex_mod
       use apply_BCs_mod
       use norms_mod
       use AB2_mod
       use GS_Poisson_mod
       use CG_mod
       use PCG_mod

       implicit none

       private

       ! Explicit time marching methods (CT methods)
       public :: CT_Finite_Rem
       public :: CT_Low_Rem

       ! Implicit time marching methods (diffusion implicit)
       public :: ind_CG_BE_EE_cleanB_CG
       public :: ind_CG_CN_AB2_cleanB_CG
       public :: ind_CG_theta_AB2_cleanB_CG


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

       subroutine CT_Finite_Rem(B,B0,U_F,J,E,sigmaInv,m,dt,Rem,tmp_F,tmp_E)
         ! Solves:
         !             ∂B/∂t = -∇xE , E = j/σ - uxB⁰ , j = Rem⁻¹∇xB
         ! or
         !             ∂B/∂t = ∇x(uxB⁰) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:
         !             B,J,E
         ! Method:
         !             Constrained Transport (CT)
         ! Info:
         !             cell face => B
         !             cell edge => J,E,sigmaInv,U
         !             Finite Rem
         implicit none
         type(VF),intent(inout) :: B,J,E
         type(VF),intent(in) :: B0,sigmaInv,U_F
         type(VF),intent(inout) :: tmp_E,tmp_F
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt,Rem
         call faceCrossFace_E(tmp_E,U_F,B0,m)
         call multiply(E,tmp_E,-1.0_cp)
         call curl(J,B,m)
         call multiply(J,1.0_cp/Rem)
         call multiply(tmp_E,J,sigmaInv)
         call add(E,tmp_E)
         call curl(tmp_F,E,m)
         call multiply(tmp_F,-dt)
         call add(B,tmp_F)
         call apply_BCs(B,m)
       end subroutine

       subroutine CT_Low_Rem(B,B0,U_F,J,E,sigmaInv,m,dt,N_induction,temp_F,temp_E)
         ! Solves:
         !             ∂B/∂t = -∇xE , E = j/σ - uxB⁰ , j = ∇xB
         ! or
         !             ∂B/∂t = ∇x(uxB⁰) - ∇x(σ⁻¹∇xB)
         ! Computes:
         !             B,J,E
         ! Method:
         !             Constrained Transport (CT)
         ! Info:
         !             cell face => B
         !             cell edge => J,E,sigmaInv,U
         !             Low Rem
         !             Quasi-static B
         implicit none
         type(VF),intent(inout) :: B,J,E
         type(VF),intent(in) :: B0,sigmaInv,U_F
         type(VF),intent(inout) :: temp_F,temp_E
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         integer,intent(in) :: N_induction
         integer :: i
         do i=1,N_induction
           call faceCrossFace_E(temp_E,U_F,B0,m)
           call multiply(E,temp_E,-1.0_cp)
           call curl(J,B,m)
           call multiply(temp_E,J,sigmaInv)
           call add(E,temp_E)
           call curl(temp_F,E,m)
           call multiply(temp_F,-dt)
           call add(B,temp_F)
           call apply_BCs(B,m)
         enddo
       end subroutine

       subroutine ind_CG_BE_EE_cleanB_CG(CG_B,CG_cleanB,B,B0,U_Ft,m,dt,N_induction,&
         N_cleanB,compute_norms,temp_F,temp_F2,temp_E1,temp_E2,curlUCrossB,temp_CC,phi)
         ! Solves:
         !             ∂B/∂t = -∇xE , E = j/σ - uxB⁰ , j = Rem⁻¹∇xB
         ! or
         !             ∂B/∂t = ∇x(uxB⁰) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:
         !             B,J,E
         ! Method:
         !             Diffusion-implicit with Conjugate Gradient method (CG)
         ! Info:
         !             cell face => B
         !             cell edge => J,E,sigmaInv,U
         !             Finite Rem
         implicit none
         type(CG_solver_VF),intent(inout) :: CG_B
         type(CG_solver_SF),intent(inout) :: CG_cleanB
         type(VF),intent(inout) :: B
         type(VF),intent(in) :: B0,U_Ft
         type(SF),intent(inout) :: temp_CC,phi
         type(VF),intent(inout) :: temp_F,temp_F2,temp_E1,temp_E2,curlUCrossB
         type(mesh),intent(in) :: m
         integer,intent(in) :: N_induction,N_cleanB
         logical,intent(in) :: compute_norms
         real(cp),intent(in) :: dt
         call add(temp_F,B0,B) ! Finite Rem
         call faceCurlCross_F(curlUCrossB,U_Ft,temp_F,m,temp_E1,temp_E2,temp_F2)
         call multiply(curlUCrossB,-dt) ! Must be negative (in divergence form)
         call add(curlUCrossB,B)
         call solve(CG_B,B,curlUCrossB,m,N_induction,compute_norms)
         ! Clean B
         call div(temp_CC,B,m)
         call solve(CG_cleanB,phi,temp_CC,m,N_cleanB,compute_norms)
         call grad(temp_F,phi,m)
         call subtract(B,temp_F)
         call apply_BCs(B,m)
       end subroutine

       subroutine ind_CG_CN_AB2_cleanB_CG(CG_B,CG_cleanB,B,Bnm1,B0,B0nm1,U_Ft,m,dt,N_induction,&
         N_cleanB,compute_norms,temp_F,temp_F2,temp_F3,temp_E1,temp_E2,curlUCrossB,temp_CC,phi)
         ! Solves:
         !             ∂B/∂t = -∇xE , E = j/σ - uxB⁰ , j = Rem⁻¹∇xB
         ! or
         !             ∂B/∂t = ∇x(uxB⁰) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:
         !             B,J,E
         ! Method:
         !             Diffusion-implicit with Conjugate Gradient method (CG)
         ! Info:
         !             cell face => B
         !             cell edge => J,E,sigmaInv,U
         !             Finite Rem
         implicit none
         type(CG_solver_VF),intent(inout) :: CG_B
         type(CG_solver_SF),intent(inout) :: CG_cleanB
         type(VF),intent(inout) :: B,Bnm1
         type(VF),intent(in) :: B0,B0nm1,U_Ft
         type(SF),intent(inout) :: temp_CC,phi
         type(VF),intent(inout) :: temp_F,temp_F2,temp_F3,temp_E1,temp_E2,curlUCrossB
         type(mesh),intent(in) :: m
         integer,intent(in) :: N_induction,N_cleanB
         logical,intent(in) :: compute_norms
         real(cp),intent(in) :: dt
         call add(temp_F,B0,B) ! Finite Rem
         call faceCurlCross_F(temp_F3,U_Ft,temp_F,m,temp_E1,temp_E2,temp_F2)
         call add(temp_F,B0nm1,Bnm1) ! Finite Rem
         call faceCurlCross_F(curlUCrossB,U_Ft,temp_F,m,temp_E1,temp_E2,temp_F3)
         call AB2_overwrite(curlUCrossB,temp_F3)
         call multiply(curlUCrossB,-dt) ! Must be negative (in divergence form)
         call add(curlUCrossB,B)
         call assign(Bnm1,B)
         call solve(CG_B,B,curlUCrossB,m,N_induction,compute_norms)
         ! Clean B
         call div(temp_CC,B,m)
         call solve(CG_cleanB,phi,temp_CC,m,N_cleanB,compute_norms)
         call grad(temp_F,phi,m)
         call subtract(B,temp_F)
         call apply_BCs(B,m)
       end subroutine

       subroutine ind_CG_theta_AB2_cleanB_CG(CG_B,CG_cleanB,B,B0,U_Ft,m,&
         sigmaInv_edge,dt,Rem,theta,N_induction,N_cleanB,compute_norms,&
         temp_F,temp_F2,temp_E1,temp_E2,curlUCrossB,temp_CC,phi)
         ! Solves:
         !             ∂B/∂t = -∇xE , E = j/σ - uxB⁰ , j = Rem⁻¹∇xB
         ! or
         !             ∂B/∂t = ∇x(uxB⁰) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:
         !             B,J,E
         ! Method:
         !             Diffusion-implicit with Conjugate Gradient method (CG)
         ! Info:
         !             cell face => B
         !             cell edge => J,E,sigmaInv,U
         !             Finite Rem
         implicit none
         type(CG_solver_VF),intent(inout) :: CG_B
         type(CG_solver_SF),intent(inout) :: CG_cleanB
         type(VF),intent(inout) :: B
         type(VF),intent(in) :: B0,U_Ft,sigmaInv_edge
         type(SF),intent(inout) :: temp_CC,phi
         type(VF),intent(inout) :: temp_F,temp_F2,temp_E1,temp_E2,curlUCrossB
         type(mesh),intent(in) :: m
         integer,intent(in) :: N_induction,N_cleanB
         logical,intent(in) :: compute_norms
         real(cp),intent(in) :: dt,Rem,theta

         call add(temp_F,B0,B) ! Finite Rem
         call faceCurlCross_F(curlUCrossB,U_Ft,temp_F,m,temp_E1,temp_E2,temp_F2)
         call multiply(curlUCrossB,-dt) ! Must be negative (in divergence form)

         call add(curlUCrossB,B)

         call curl(temp_E1,B,m)
         call multiply(temp_E1,sigmaInv_edge)
         call curl(temp_F,temp_E1,m)
         call multiply(temp_F,dt/Rem*(1.0_cp-theta))
         call add(curlUCrossB,temp_F)

         call solve(CG_B,B,curlUCrossB,m,N_induction,compute_norms)

         ! Clean B
         call div(temp_CC,B,m)
         call solve(CG_cleanB,phi,temp_CC,m,N_cleanB,compute_norms)
         call grad(temp_F,phi,m)
         call subtract(B,temp_F)
         call apply_BCs(B,m)
       end subroutine

       end module