       module induction_solver_mod
       ! Constrained Transport (CT) Method reference:
       ! "Tóth, G. The divergence Constraint in Shock-Capturing
       ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use curl_curl_B_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_advect_mod
       use ops_norms_mod
       use apply_BCs_mod
       use norms_mod
       use AB2_mod
       use compute_energy_mod
       use matrix_free_params_mod
       use GS_Poisson_mod
       use PCG_mod
       use Jacobi_mod
       use induction_aux_mod
       use export_raw_processed_mod
       use clean_divergence_mod
       use update_intermediate_field_BCs_mod
       use mesh_domain_mod
       use ops_embedExtract_mod
       use time_marching_params_mod

       implicit none

       private

       ! Explicit time marching methods (CT methods)
       public :: CT_Finite_Rem
       public :: CT_Finite_Rem_perfect_vacuum
       public :: CT_Low_Rem
       public :: CT_Low_Rem_matrix_based
       public :: CT_Finite_Rem_interior_solved
       public :: JAC_interior_solved

       ! Implicit time marching methods (diffusion implicit)
       public :: ind_PCG_BE_EE_cleanB_PCG
       public :: ind_PCG_CN_AB2_cleanB_PCG

       contains

       subroutine CT_Finite_Rem(B,B0,U_E,J,F,sigmaInv_E,m,N_multistep,dt,&
         temp_F1,temp_F2,temp_F3,temp_E,temp_E_TF)
         ! Solves:    ∂B/∂t = ∇x(ux(B⁰+B)) - Rem⁻¹∇x(σ⁻¹∇xB) + F
         ! Computes:  B (above)
         ! Note:      J = Rem⁻¹∇xB    -> J ALREADY HAS Rem⁻¹ !
         ! Method:    Constrained Transport (CT)
         ! Info:      cell face => B,B0,cell edge => J,sigmaInv_E,U_E,Finite Rem
         implicit none
         type(VF),intent(inout) :: B,temp_E,temp_F1,temp_F2,temp_F3
         type(VF),intent(in) :: B0,sigmaInv_E,J,F
         type(TF),intent(inout) :: temp_E_TF
         type(TF),intent(in) :: U_E
         type(mesh),intent(in) :: m
         integer,intent(in) :: N_multistep
         real(cp),intent(in) :: dt
         integer :: i
         do i=1,N_multistep
           call add(temp_F2,B,B0) ! Since finite Rem
           call advect_B(temp_F1,U_E,temp_F2,m,temp_E_TF,temp_E)
           call multiply(temp_E,J,sigmaInv_E)
           call curl(temp_F3,temp_E,m)
           call subtract(temp_F1,temp_F3)
           call add(temp_F1,F)
           call multiply(temp_F1,dt)
           call add(B,temp_F1)
           call apply_BCs(B)
         enddo
       end subroutine

       subroutine CT_Low_Rem(B,B0,U_E,J,sigmaInv_E,m,N_multistep,dt,temp_F1,temp_F2,temp_E,temp_E_TF)
         implicit none
         type(VF),intent(inout) :: B,J,temp_E,temp_F1,temp_F2
         type(VF),intent(in) :: B0,sigmaInv_E
         type(TF),intent(inout) :: temp_E_TF
         type(TF),intent(in) :: U_E
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         integer,intent(in) :: N_multistep
         integer :: i
         do i=1,N_multistep
           call curl_curl_B_matrix_free(temp_F2,J,B,sigmaInv_E,m,temp_E)
           call advect_B(temp_F1,U_E,B0,m,temp_E_TF,temp_E)
           call multiply(temp_F1,dt)
           call add_product(B,temp_F2,-dt)
           call add(B,temp_F1)
           call apply_BCs(B)
         enddo
       end subroutine

       subroutine CT_Low_Rem_matrix_based(B,B0,U_E,m,N_multistep,dt,temp_F1,temp_F2,temp_E,temp_E_TF)
         implicit none
         type(VF),intent(inout) :: B,temp_E,temp_F1,temp_F2
         type(VF),intent(in) :: B0
         type(TF),intent(inout) :: temp_E_TF
         type(TF),intent(in) :: U_E
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         integer,intent(in) :: N_multistep
         integer :: i
         do i=1,N_multistep
           call advect_B(temp_F1,U_E,B0,m,temp_E_TF,temp_E)
           call curl_curl_B_matrix_based(temp_F2,B,m) ! A = -(I + curl(curl))
           call multiply(temp_F2,-dt)
           call multiply(temp_F1,dt)
           call add(B,temp_F1,temp_F2)
           call apply_BCs(B)
         enddo
       end subroutine

       subroutine ind_PCG_BE_EE_cleanB_PCG(PCG_B,PCG_cleanB,B,Bstar,phi,B0,U_E,&
         F,m,dt,compute_norms,temp_F1,temp_F2,temp_E,temp_E_TF,temp_CC,temp_CC_VF)
         ! Solves:    ∂B/∂t = ∇x(ux(B⁰+B)) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:  B (above)
         ! Method:    Preconditioned Conjugate Gradient Method (induction equation)
         !            Preconditioned Conjugate Gradient Method (elliptic cleaning procedure)
         ! Info:      cell face => B,B0,cell edge => J,sigmaInv_E,U_E,Finite Rem
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_B
         type(PCG_solver_SF),intent(inout) :: PCG_cleanB
         type(VF),intent(inout) :: B,Bstar
         type(VF),intent(in) :: B0,F
         type(TF),intent(in) :: U_E
         type(SF),intent(inout) :: temp_CC,phi
         type(TF),intent(inout) :: temp_E_TF
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_E,temp_CC_VF
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         logical,intent(in) :: compute_norms
         ! call add(temp_F2,B,B0) ! Since finite Rem
         call advect_B(temp_F1,U_E,B0,m,temp_E_TF,temp_E)
         call multiply(temp_F1,dt)
         call add(temp_F1,B)
         ! call add_product(temp_F1,F,dt)
         call solve(PCG_B,Bstar,temp_F1,m,compute_norms)
         call clean_div(PCG_cleanB,B,Bstar,phi,1.0_cp,m,temp_F1,temp_CC,compute_norms)
         call update_intermediate_field_BCs(Bstar,B,phi,m,temp_F1,temp_F2,temp_CC_VF)
       end subroutine

       subroutine ind_PCG_CN_AB2_cleanB_PCG(PCG_B,PCG_cleanB,B,Bstar,phi,B0,U_E,&
         J,sigmaInv_E,curlUCrossB,curlUCrossB_nm1,F,m,MFP,dt,compute_norms,temp_F1,&
         temp_F2,temp_E,temp_E_TF,temp_CC,temp_CC_VF)
         ! Solves:    (B^n-B^{n+1})/dt+.5 Rem⁻¹∇x(σ⁻¹∇xB^{n+1})=AB2(∇x(ux(B⁰^n+B^n)))-.5 Rem⁻¹∇x(σ⁻¹∇xB^n)
         ! Computes:  B (above)
         ! Method:    PCG Method (induction equation)
         !            PCG Method (elliptic cleaning procedure)
         ! Info:      cell face => B,B0,cell edge => J,sigmaInv_E,U_E,Finite Rem
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_B
         type(PCG_solver_SF),intent(inout) :: PCG_cleanB
         type(VF),intent(inout) :: B,Bstar
         type(VF),intent(in) :: B0,F,J,sigmaInv_E
         type(TF),intent(in) :: U_E
         type(SF),intent(inout) :: temp_CC,phi
         type(TF),intent(inout) :: temp_E_TF
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_E,temp_CC_VF,curlUCrossB,curlUCrossB_nm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         type(matrix_free_params),intent(in) :: MFP
         logical,intent(in) :: compute_norms
         call assign(curlUCrossB_nm1,curlUCrossB)
         call add(temp_F2,B,B0) ! Since finite Rem
         call advect_B(curlUCrossB,U_E,temp_F2,m,temp_E_TF,temp_E)
         call AB2(temp_F1,curlUCrossB,curlUCrossB_nm1)
         call multiply(temp_F1,dt)

         call multiply(temp_E,J,sigmaInv_E)
         call curl(temp_F2,temp_E,m)
         call multiply(temp_F2,MFP%coeff_explicit)
         call add(temp_F1,temp_F2)

         call AB2(temp_F2,F,F)
         call add_product(temp_F1,temp_F2,dt)

         call add(temp_F1,B)
         call solve(PCG_B,Bstar,temp_F1,m,compute_norms)
         call clean_div(PCG_cleanB,B,Bstar,phi,1.0_cp/dt,m,temp_F1,temp_CC,compute_norms)
         call update_intermediate_field_BCs(Bstar,B,phi,m,temp_F1,temp_F2,temp_CC_VF)
       end subroutine

       subroutine CT_Finite_Rem_interior_solved(PCG_cleanB,B,B0,Bstar,J,B_interior,U_E,curlE,&
         phi,m,MD_sigma,TMP,Rem,finite_Rem,compute_norms,SF_CC,temp_F1,temp_F2,curlUCrossB,temp_E,temp_E_TF)
         ! Solves:  ∂B/∂t = ∇•∇B,  in vacuum domain, where B_interior is fixed.
         ! Note:    J = Rem⁻¹∇xB    -> J ALREADY HAS Rem⁻¹ !
         ! Method:  Constrained Transport (CT)
         ! Info:    Cell face => B,cell edge => J,sigmaInv_E,U_E,Finite Rem
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG_cleanB
         type(VF),intent(inout) :: B,Bstar,J,temp_F1,temp_F2,curlUCrossB,temp_E,curlE
         type(VF),intent(in) :: B0,B_interior
         type(SF),intent(inout) :: SF_CC,phi
         type(TF),intent(in) :: U_E
         type(TF),intent(inout) :: temp_E_TF
         type(mesh_domain),intent(in) :: MD_sigma
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         logical,intent(in) :: compute_norms,finite_Rem
         real(cp),intent(in) :: Rem
         integer :: i
         if (TMP%n_step.le.1) then
           call compute_J(J,B,Rem,m,finite_Rem)
           call add(temp_F2,B,B0)
           call advect_B(curlUCrossB,U_E,temp_F2,m,temp_E_TF,temp_E)
           call curl(curlE,J,m)
           call subtract(curlE,curlUCrossB)
         endif
         do i=1,TMP%multistep_iter
           call multiply(temp_F1,curlE,-TMP%dt)
           call add(B,temp_F1)
           call apply_BCs(B)
           call embedFace(B,B_interior,MD_sigma)
         enddo
         call clean_div(PCG_cleanB,B,Bstar,phi,1.0_cp/TMP%dt,m,temp_F1,SF_CC,compute_norms)
         call embedFace(B,B_interior,MD_sigma)
       end subroutine

       subroutine JAC_interior_solved(JAC,PCG_cleanB,B,Bstar,RHS,phi,m,&
         N_multistep,N_induction,dt,compute_norms,SF_CC,temp_F1,temp_F2,temp_CC_VF)
         ! Solves: ∇•(∇B) = 0 using Jacobi method + cleaning procedure
         implicit none
         type(Jacobi),intent(inout) :: JAC
         type(PCG_solver_SF),intent(inout) :: PCG_cleanB
         type(VF),intent(inout) :: B,Bstar
         type(VF),intent(in) :: RHS
         type(SF),intent(inout) :: SF_CC,phi
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_CC_VF
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         integer,intent(in) :: N_multistep,N_induction
         logical,intent(in) :: compute_norms
         integer :: i
         do i=1,N_multistep
           call solve(JAC,B,RHS,m,N_induction,.true.)
           call clean_div(PCG_cleanB,B,Bstar,phi,1.0_cp/dt,m,temp_F1,SF_CC,compute_norms)
           call update_intermediate_field_BCs(Bstar,B,phi,m,temp_F1,temp_F2,temp_CC_VF)
         enddo
       end subroutine

       subroutine CT_Finite_Rem_perfect_vacuum(PCG_B,PCG_cleanB,B,Bstar,B0,U_E,J,m,&
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
         type(VF),intent(inout) :: B,Bstar,temp_E,temp_F1,temp_F2
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
         call clean_div(PCG_cleanB,B,Bstar,phi,1.0_cp/dt,m,temp_F1,temp_CC,compute_norms)
       end subroutine

       end module