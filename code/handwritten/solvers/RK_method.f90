       module RK_method_mod
       use current_precision_mod
       use mesh_extend_mod
       use SF_extend_mod
       use VF_extend_mod
       use TF_extend_mod
       use norms_extend_mod
       use string_mod
       use AB2_mod
       use compute_energy_mod
       use export_raw_processed_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_advect_mod
       use ops_norms_mod
       use apply_BCs_mod
       use PCG_solver_extend_mod
       use matrix_free_operators_mod
       use matrix_free_params_mod
       use RK_Params_mod
       use clean_divergence_mod
       use time_marching_params_mod

       implicit none
       private

       public :: RK_method_stage_1
       public :: RK_method_stage_2
       public :: RK_method_stage_3

       real(cp),parameter :: two_thirds = 2.0_cp/3.0_cp
       real(cp),parameter :: four_thirds = 4.0_cp/3.0_cp
       real(cp),parameter :: one_third = 1.0_cp/3.0_cp
       real(cp),parameter :: three_halfs = 3.0_cp/2.0_cp
       real(cp),parameter :: three_fourths = 3.0_cp/4.0_cp
       real(cp),parameter :: one_fourth = 1.0_cp/4.0_cp
       real(cp),parameter :: one_half = 1.0_cp/2.0_cp
       real(cp),parameter :: five_eigths = 5.0_cp/8.0_cp
       real(cp),parameter :: three_eigths = 3.0_cp/8.0_cp

       contains

       ! call update_MFP(PCG_VF,m,TMP%TS%dt*2.0_cp*RKP%beta%f(RKP%n)*PCG_VF%MFP%coeff_implicit,.true.)

       subroutine coupled_RK_method(M)
         implicit none
         type(MOONS),intent(inout) :: M
         real(cp) :: tau
         logical :: compute_norms
         compute_norms = M%C%SP%EF%unsteady_0D%export_now
         tau = 0.01_cp
         call add_all_induction_raw(M%GE%ind%F,M%GE%ind,&
         M%C%SP%TMP,M%C%SP,M%GE%mom%U_E,M%GE%ind%Btot)

         call RK_method_stage_1(PCG_VF,PCG_SF,X_prime_1,Xstar,Xn,&
         Fn,phi,m,tau,TMP,temp_F1,temp_CC,compute_norms)

         call RK_method_stage_1(PCG_VF,PCG_SF,X_prime_1,Xstar,Xn,&
         Fn,phi,m,tau,TMP,temp_F1,temp_CC,compute_norms)

         call add_all_induction_raw(M%GE%ind%F,M%GE%ind,&
         M%C%SP%TMP,M%C%SP,M%GE%mom%U_E,M%GE%ind%Btot)

         call RK_method_stage_2(PCG_VF,PCG_SF,phi_prime,temp_CC,&
         X_tilde,X_prime_1,X_prime_2,Xstar,temp_F1,&
         F_prime_1,Xn,m,TMP,compute_norms,tau)

         call RK_method_stage_2(PCG_VF,PCG_SF,phi_prime,temp_CC,&
         X_tilde,X_prime_1,X_prime_2,Xstar,temp_F1,&
         F_prime_1,Xn,m,TMP,compute_norms,tau)

         call add_all_induction_raw(M%GE%ind%F,M%GE%ind,&
         M%C%SP%TMP,M%C%SP,M%GE%mom%U_E,M%GE%ind%Btot)

         call RK_method_stage_3(PCG_VF,PCG_SF,phi_prime,q,&
         temp_CC,X_np1,X_hat,X_prime_2,&
         Xstar,temp_F1,F_prime_2,Xn,&
         X_tilde,m,TMP,compute_norms,tau)

         call RK_method_stage_3(PCG_VF,PCG_SF,phi_prime,q,&
         temp_CC,X_np1,X_hat,X_prime_2,&
         Xstar,temp_F1,F_prime_2,Xn,&
         X_tilde,m,TMP,compute_norms,tau)
       end subroutine

       subroutine RK_method_stage_1(PCG_VF,PCG_SF,X_prime_1,Xstar,&
         Xn,Fn,phi,m,tau,TMP,temp_F1,temp_CC,compute_norms)
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_VF
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: phi,temp_CC
         type(VF),intent(inout) :: X_prime_1,Xstar,temp_F1
         type(VF),intent(in) :: Fn,Xn
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         logical,intent(in) :: compute_norms
         real(cp),intent(in) :: tau
         call div(temp_CC,Fn,m)
         call solve(PCG_SF,phi,temp_CC,m,compute_norms)
         call grad(temp_F1,phi,m)
         call subtract(temp_F1,Fn)
         call multiply(temp_F1,-two_thirds*tau)
         call solve(PCG_VF,Xstar,temp_F1,m,compute_norms)
         call add(X_prime_1,Xstar,Xn)
       end subroutine

       subroutine RK_method_stage_2(PCG_VF,PCG_SF,phi_prime,temp_CC,&
         X_tilde,X_prime_1,X_prime_2,Xstar,temp_F1,&
         F_prime_1,Xn,m,TMP,compute_norms,tau)
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_VF
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: phi_prime,temp_CC
         type(VF),intent(inout) :: X_tilde,X_prime_1,X_prime_2,Xstar,temp_F1
         type(VF),intent(in) :: F_prime_1,Xn
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         logical,intent(in) :: compute_norms
         real(cp),intent(in) :: tau

         call div(temp_CC,F_prime_1,m)
         call solve(PCG_SF,phi_prime,temp_CC,m,compute_norms)

         call grad(temp_F1,phi_prime,m)
         call subtract(temp_F1,F_prime_1)
         call multiply(temp_F1,-one_third*tau)
         call add(temp_F1,Xn)
         call add(temp_F1,X_prime_1)
         call solve(PCG_VF,Xstar,temp_F1,m,compute_norms)
         call assign(X_prime_2,Xstar)
         call add_product(X_prime_2,X_prime_1,three_halfs)
         call add_product(X_prime_2,Xn,one_half)

         call subtract(temp_F1,X_prime_2,Xn)
         call multiply(temp_F1,three_fourths)
         call solve(PCG_VF,Xstar,temp_F1,m,compute_norms)
         call assign(X_tilde,Xstar)
         call add_product(X_tilde,X_prime_2,three_halfs)
         call add_product(X_tilde,X_prime_1,-three_fourths)
         call add_product(X_tilde,Xn,one_fourth)
       end subroutine

       subroutine RK_method_stage_3(PCG_VF,PCG_SF,phi_prime,q,&
         temp_CC,X_np1,X_hat,X_prime_2,&
         Xstar,temp_F1,F_prime_2,Xn,&
         X_tilde,m,TMP,compute_norms,tau)
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_VF
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(in) :: phi_prime
         type(SF),intent(inout) :: q,temp_CC
         type(VF),intent(inout) :: X_np1,X_hat,X_prime_2,Xstar,temp_F1
         type(VF),intent(in) :: F_prime_2,Xn,X_tilde
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         logical,intent(in) :: compute_norms
         real(cp),intent(in) :: tau

         call grad(temp_F1,phi_prime,m)
         call subtract(temp_F1,F_prime_2)
         call multiply(temp_F1,-three_fourths*tau)

         call add_product(temp_F1,Xn,five_eigths)
         call add_product(temp_F1,X_prime_2,three_eigths)
         call add_product(temp_F1,X_tilde,-1.0_cp)

         call solve(PCG_VF,Xstar,temp_F1,m,compute_norms)
         call assign(X_hat,Xstar)
         call add_product(X_hat,X_tilde,one_half)
         call add_product(X_hat,X_prime_2,three_fourths)
         call add_product(X_hat,Xn,one_fourth)

         call div(temp_CC,X_hat,m)
         call solve(PCG_SF,q,temp_CC,m,compute_norms)
         call grad(temp_F1,q,m)
         call subtract(X_np1,X_hat,temp_F1)
       end subroutine

       end module