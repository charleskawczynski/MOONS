       module time_marching_methods_SF_mod
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

       public :: O2_BDF_time_AB2_sources_SF
       public :: Euler_time_AB2_sources_SF
       public :: Euler_time_RK_sources_SF

       public :: Euler_time_no_diff_AB2_sources_SF
       public :: Euler_time_no_diff_Euler_sources_SF

       real(cp),parameter :: two_thirds = 2.0_cp/3.0_cp
       real(cp),parameter :: four_thirds = 4.0_cp/3.0_cp
       real(cp),parameter :: neg_one_third = -1.0_cp/3.0_cp
       real(cp),parameter :: three_halfs = 3.0_cp/2.0_cp

       contains

       subroutine O2_BDF_time_AB2_sources_SF(PCG_SF,X,Xnm1,F,Fnm1,m,&
         TMP,temp_F1,compute_norms)
         ! Solves:
         !
         !  3X^{*} - 4X^{n} + X^{n-1}
         ! -------------------------- + AX^{*} = AB2(F^{n},F^{n-1})
         !            2 dt
         !
         ! -->  (I + 2/3*dt A)X^{*} = 4/3X^{n} - 1/3 X^{n-1} + 2/3 dt AB2(F^{n},F^{n-1})
         !
         ! X^{n+1} = X^{*}
         !
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: X,Xnm1,temp_F1
         type(SF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         logical,intent(in) :: compute_norms
         call AB2(temp_F1,F,Fnm1)
         call multiply(temp_F1,two_thirds*TMP%TS%dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,X)
         call add_product(temp_F1,X,four_thirds)
         call add_product(temp_F1,Xnm1,neg_one_third)
         call assign(Xnm1,X)
         call update_MFP(PCG_SF,m,TMP%TS%dt*two_thirds*PCG_SF%MFP%coeff_implicit,TMP%n_step.le.2)
         call solve(PCG_SF,X,temp_F1,m,compute_norms) ! Solve for X
       end subroutine

       subroutine Euler_time_AB2_sources_SF(PCG_SF,X,Xnm1,F,Fnm1,m,&
         TMP,temp_F1,compute_norms)
         ! Solves:
         !
         !  X^{*} - X^{n}
         ! -------------- + AX^{*} = AB2(F^{n},F^{n-1})
         !        dt
         !
         ! -->  (I + dt 1 A)X^{*} = X^{n} + dt AB2(F^{n},F^{n-1})
         !
         ! X^{n+1} = X^{*}
         !
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: X,Xnm1
         type(SF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         type(SF),intent(inout) :: temp_F1
         logical,intent(in) :: compute_norms
         call AB2(temp_F1,F,Fnm1)
         call multiply(temp_F1,TMP%TS%dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,X)
         call add(temp_F1,X)
         call assign(Xnm1,X)
         call update_MFP(PCG_SF,m,TMP%TS%dt*1.0_cp*PCG_SF%MFP%coeff_implicit,TMP%n_step.le.2)
         call solve(PCG_SF,X,temp_F1,m,compute_norms) ! Solve for X
       end subroutine

       subroutine Euler_time_RK_sources_SF(PCG_SF,X,Xnm1,F,Fnm1,L,m,&
         TMP,RKP,temp_F1,compute_norms)
         ! Solves:
         !
         !  X^{*} - X^{n}
         ! -------------- + AX^{*} = AB2(F^{n},F^{n-1})
         !        dt
         !
         ! -->  (I + dt 1 A)X^{*} = X^{n} + dt AB2(F^{n},F^{n-1})
         !
         ! X^{n+1} = X^{*}
         !
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: X,Xnm1
         type(SF),intent(in) :: F,Fnm1,L
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         type(RK_Params),intent(in) :: RKP
         type(SF),intent(inout) :: temp_F1
         logical,intent(in) :: compute_norms
         call multiply(temp_F1,F      ,TMP%TS%dt*RKP%gamma%f(RKP%n))
         call add_product(temp_F1,Fnm1,TMP%TS%dt*RKP%zeta%f(RKP%n))
         call add_product(temp_F1,L   ,TMP%TS%dt*RKP%alpha%f(RKP%n))
         call assign_wall_Dirichlet(temp_F1,0.0_cp,X)
         call add(temp_F1,X)
         call assign(Xnm1,X)
         call update_MFP(PCG_SF,m,TMP%TS%dt*RKP%beta%f(RKP%n)*PCG_SF%MFP%coeff_implicit,.true.)
         call solve(PCG_SF,X,temp_F1,m,compute_norms) ! Solve for X
       end subroutine

       ! **********************************************************************
       ! ******************** DIFFUSION EXPLICIT TIME MARCHING ****************
       ! **********************************************************************

       subroutine Euler_time_no_diff_AB2_sources_SF(X,Xstar,F,Fnm1,TMP)
         ! Solves:
         !
         !  X^{n+1} - X^{n}
         ! ---------------- = AB2(F^{n},F^{n-1})
         !          dt
         !
         ! -->  X^{n+1} = X^{n} + dt AB2(F^{n},F^{n-1})
         !
         implicit none
         type(SF),intent(inout) :: X,Xstar
         type(SF),intent(in) :: F,Fnm1
         type(time_marching_params),intent(in) :: TMP
         call AB2(Xstar,F,Fnm1)
         call multiply(Xstar,TMP%TS%dt)
         call assign_wall_Dirichlet(Xstar,0.0_cp,X)
         call add(X,Xstar)
       end subroutine

       subroutine Euler_time_no_diff_Euler_sources_SF(X,Xstar,F,TMP)
         ! Solves:
         !
         !  X^{n+1} - X^{n}
         ! ---------------- = F^{n}
         !          dt
         !
         ! -->  X^{n+1} = X^{n} + dt F^{n}
         !
         implicit none
         type(SF),intent(inout) :: X,Xstar
         type(SF),intent(in) :: F
         type(time_marching_params),intent(in) :: TMP
         call multiply(Xstar,F,TMP%TS%dt)
         call assign_wall_Dirichlet(Xstar,0.0_cp,X)
         call add(X,Xstar)
       end subroutine

       end module