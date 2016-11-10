       module induction_solver_mod
       ! Constrained Transport (CT) Method reference:
       ! "Tóth, G. The divergence Constraint in Shock-Capturing 
       ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_advect_mod
       use ops_norms_mod
       use apply_BCs_mod
       use norms_mod
       use AB2_mod
       use compute_energy_mod
       use GS_Poisson_mod
       use PCG_mod
       use Jacobi_mod
       use induction_aux_mod
       use export_raw_processed_mod
       use mesh_domain_mod
       use ops_embedExtract_mod

       implicit none

       private

       ! Explicit time marching methods (CT methods)
       public :: CT_Finite_Rem
       public :: CT_Finite_Rem_perfect_vacuum
       public :: CT_Low_Rem
       public :: CT_Finite_Rem_interior_solved
       public :: JAC_interior_solved

       ! Implicit time marching methods (diffusion implicit)
       public :: ind_PCG_BE_EE_cleanB_PCG

       contains

       subroutine CT_Finite_Rem(B,B0,U_E,J,sigmaInv_E,m,dt,&
         temp_F1,temp_F2,temp_F3,temp_E,temp_E_TF)
         ! Solves:    ∂B/∂t = ∇x(ux(B⁰+B)) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:  B (above)
         ! Note:      J = Rem⁻¹∇xB    -> J ALREADY HAS Rem⁻¹ !
         ! Method:    Constrained Transport (CT)
         ! Info:      cell face => B,B0,cell edge => J,sigmaInv_E,U_E,Finite Rem
         implicit none
         type(VF),intent(inout) :: B,temp_E,temp_F1,temp_F2,temp_F3
         type(VF),intent(in) :: B0,sigmaInv_E,J
         type(TF),intent(inout) :: temp_E_TF
         type(TF),intent(in) :: U_E
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         call add(temp_F2,B,B0) ! Since finite Rem
         call advect_B(temp_F1,U_E,temp_F2,m,temp_E_TF,temp_E)
         call multiply(temp_E,J,sigmaInv_E)
         call curl(temp_F3,temp_E,m)
         call subtract(temp_F1,temp_F3)
         call multiply(temp_F1,dt)
         call add(B,temp_F1)
         call apply_BCs(B,m)
       end subroutine

       subroutine CT_Finite_Rem_interior_solved(PCG_cleanB,B,B_interior,curlE,&
         phi,m,MD_sigma,dt,N_induction,compute_norms,SF_CC,VF_F1)
         ! Solves:  ∂B/∂t = ∇•∇B,  in vacuum domain, where B_interior is fixed.
         ! Note:    J = Rem⁻¹∇xB    -> J ALREADY HAS Rem⁻¹ !
         ! Method:  Constrained Transport (CT)
         ! Info:    Cell face => B,cell edge => J,sigmaInv_E,U_E,Finite Rem
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG_cleanB
         type(VF),intent(inout) :: B,VF_F1
         type(VF),intent(in) :: B_interior,curlE
         type(SF),intent(inout) :: SF_CC,phi
         type(mesh_domain),intent(in) :: MD_sigma
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         integer,intent(in) :: N_induction
         logical,intent(in) :: compute_norms
         integer :: i
         do i=1,N_induction
           call multiply(VF_F1,curlE,-dt)
           call add(B,VF_F1)
           call apply_BCs(B,m)
           call embedFace(B,B_interior,MD_sigma)
         enddo
         ! Clean B
         call div(SF_CC,B,m)
         call solve(PCG_cleanB,phi,SF_CC,m,compute_norms)
         call grad(VF_F1,phi,m)
         call subtract(B,VF_F1)
         call apply_BCs(B,m)
         call embedFace(B,B_interior,MD_sigma)
       end subroutine

       subroutine JAC_interior_solved(JAC,PCG_cleanB,B,RHS,phi,m,&
         n,N_induction,compute_norms,SF_CC,VF_F)
         ! Solves: ∇•(∇B) = 0 using Jacobi method + cleaning procedure
         implicit none
         type(Jacobi),intent(inout) :: JAC
         type(PCG_solver_SF),intent(inout) :: PCG_cleanB
         type(VF),intent(inout) :: B
         type(VF),intent(in) :: RHS
         type(SF),intent(inout) :: SF_CC,phi
         type(VF),intent(inout) :: VF_F
         type(mesh),intent(in) :: m
         integer,intent(in) :: n,N_induction
         logical,intent(in) :: compute_norms
         integer :: i
         do i=1,n
           call solve(JAC,B,RHS,m,N_induction,.true.)
           ! Clean B
           call div(SF_CC,B,m)
           call solve(PCG_cleanB,phi,SF_CC,m,compute_norms)
           call grad(VF_F,phi,m)
           call subtract(B,VF_F)
           call apply_BCs(B,m)
         enddo
       end subroutine

       subroutine CT_Finite_Rem_perfect_vacuum(PCG_B,PCG_cleanB,B,B0,U_E,J,m,&
         D_conductor,dt,compute_norms,temp_CC,temp_F1,&
         temp_F2,temp_E,temp_E_TF,phi)
         ! This has not yet been tested and is likely flawed currently.
         ! 
         ! Solves:    ∂B/∂t = ∇x(ux(B⁰+B)) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:  B (above)
         ! Note:      J = Rem⁻¹∇xB    -> J ALREADY HAS Rem⁻¹ !
         ! Method:    Constrained Transport (CT)
         ! Info:      cell face => B,B0,cell edge => J,sigmaInv_E,U_E,Finite Rem
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_B
         type(PCG_solver_SF),intent(inout) :: PCG_cleanB
         type(VF),intent(inout) :: B,temp_E,temp_F1,temp_F2
         type(SF),intent(inout) :: temp_CC,phi
         type(VF),intent(in) :: B0,J
         type(TF),intent(inout) :: temp_E_TF
         type(TF),intent(in) :: U_E
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         type(mesh_domain),intent(in) :: D_conductor
         logical,intent(in) :: compute_norms
         type(VF) :: temp
         call add(temp_F2,B,B0) ! Since finite Rem
         call advect_B(temp_F1,U_E,temp_F2,m,temp_E_TF,temp_E)
         call assign(temp_E,J)
         call curl(temp_F2,temp_E,m)
         call subtract(temp_F1,temp_F2)
         call multiply(temp_F1,dt)
         call add(B,temp_F1)
         ! Solve for B in vacuum by Poisson equation:
         call div(temp_CC,B,m)
         call grad(temp_F1,temp_CC,m)
         call assign(temp_F2,B)
         call solve(PCG_B,B,temp_F1,m,compute_norms)
         call init_Face(temp,m,D_conductor)
         call extractFace(temp,temp_F2,D_conductor)
         call embedFace(B,temp,D_conductor)
         call delete(temp)
         ! Clean B
         call div(temp_CC,B,m)
         call solve(PCG_cleanB,phi,temp_CC,m,compute_norms)
         call grad(temp_F1,phi,m)
         call subtract(B,temp_F1)
         call apply_BCs(B,m)
       end subroutine

       subroutine CT_Low_Rem(B,B0,U_E,J,sigmaInv_E,m,N_induction,dt,temp_F1,temp_F2,temp_E,temp_E_TF)
         ! Solves:
         ! Solves:    ∂B/∂t = ∇x(uxB⁰) - ∇x(σ⁻¹∇xB)
         ! Computes:  B (above)
         ! Note:      J = ∇xB    -> J does not have Rem⁻¹ !
         ! Method:    Constrained Transport (CT)
         ! Info:      cell face => B,B0,cell edge => J,sigmaInv_E,U_E,Low Rem
         implicit none
         type(VF),intent(inout) :: B,J,temp_E,temp_F1,temp_F2
         type(VF),intent(in) :: B0,sigmaInv_E
         type(TF),intent(inout) :: temp_E_TF
         type(TF),intent(in) :: U_E
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         integer,intent(in) :: N_induction
         integer :: i
         do i=1,N_induction
           call advect_B(temp_F1,U_E,B0,m,temp_E_TF,temp_E)
           call curl_curl_test_VF_VF(temp_F2,B,m)
           call subtract(temp_F1,temp_F2)
           call multiply(temp_F1,dt)
           call add(B,temp_F1)
           call apply_BCs(B,m)
         enddo
       end subroutine

       subroutine ind_PCG_BE_EE_cleanB_PCG(PCG_B,PCG_cleanB,B,B0,U_E,m,&
         dt,compute_norms,temp_F1,temp_F2,temp_E,&
         temp_E_TF,temp_CC,phi)
         ! Solves:    ∂B/∂t = ∇x(ux(B⁰+B)) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:  B (above)
         ! Method:    Preconditioned Conjugate Gradient Method (induction equation)
         !            Preconditioned Conjugate Gradient Method (elliptic cleaning procedure)
         ! Info:      cell face => B,B0,cell edge => J,sigmaInv_E,U_E,Finite Rem
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_B
         type(PCG_solver_SF),intent(inout) :: PCG_cleanB
         type(VF),intent(inout) :: B
         type(VF),intent(in) :: B0
         type(TF),intent(in) :: U_E
         type(SF),intent(inout) :: temp_CC,phi
         type(TF),intent(inout) :: temp_E_TF
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_E
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         logical,intent(in) :: compute_norms
         ! Induction
         call add(temp_F2,B,B0) ! Since finite Rem
         call advect_B(temp_F1,U_E,temp_F2,m,temp_E_TF,temp_E)
         call multiply(temp_F1,dt)
         call add(temp_F1,B)
         call solve(PCG_B,B,temp_F1,m,compute_norms)
         ! Clean B
         call div(temp_CC,B,m)
         call solve(PCG_cleanB,phi,temp_CC,m,compute_norms)
         call grad(temp_F1,phi,m)
         call subtract(B,temp_F1)
         call apply_BCs(B,m)
       end subroutine

       end module