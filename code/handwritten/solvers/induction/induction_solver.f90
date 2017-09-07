       module induction_solver_mod
       ! Constrained Transport (CT) Method reference:
       ! "Tóth, G. The divergence Constraint in Shock-Capturing
       ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
       use current_precision_mod
       use mesh_extend_mod
       use SF_extend_mod
       use VF_extend_mod
       use TF_extend_mod
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
       public :: CT_Finite_Rem_perfect_vacuum
       public :: CT_Finite_Rem_interior_solved
       public :: JAC_interior_solved

       contains

       subroutine CT_Finite_Rem_interior_solved(PCG_cleanB,B,B0,Bstar,J,B_interior,U_E,curlE,&
         phi,m,MD_sigma,TMP,Rem,compute_norms,SF_CC,temp_F1,temp_F2,curlUCrossB,temp_E,temp_E_TF)
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
         logical,intent(in) :: compute_norms
         real(cp),intent(in) :: Rem
         integer :: i
         if (TMP%n_step.le.1) then
           call compute_J(J,B,Rem,m)
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
         N_multistep,N_induction,dt,compute_norms,SF_CC,temp_F1,temp_E)
         ! Solves: ∇•(∇B) = 0 using Jacobi method + cleaning procedure
         implicit none
         type(Jacobi),intent(inout) :: JAC
         type(PCG_solver_SF),intent(inout) :: PCG_cleanB
         type(VF),intent(inout) :: B,Bstar
         type(VF),intent(in) :: RHS
         type(SF),intent(inout) :: SF_CC,phi
         type(VF),intent(inout) :: temp_F1,temp_E
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         integer,intent(in) :: N_multistep,N_induction
         logical,intent(in) :: compute_norms
         integer :: i
         do i=1,N_multistep
           call solve(JAC,B,RHS,m,N_induction,.true.)
           call clean_div(PCG_cleanB,B,Bstar,phi,1.0_cp/dt,m,temp_F1,SF_CC,compute_norms)
           call update_intermediate_field_BCs(Bstar,phi,dt,m,temp_F1,temp_E,SF_CC)
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