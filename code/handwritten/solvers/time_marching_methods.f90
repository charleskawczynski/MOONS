       module time_marching_methods_mod
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
       use FFT_solver_extend_mod
       use matrix_free_operators_mod
       use matrix_free_params_mod
       use RK_Params_mod
       use clean_divergence_mod
       use time_marching_params_mod

       implicit none
       private

       public :: Euler_time_Euler_sources
       public :: O2_BDF_time_AB2_sources
       public :: Euler_time_AB2_sources
       public :: Euler_time_AB2_sources_FFT
       public :: Euler_time_RK_sources

       public :: Euler_time_no_diff_AB2_sources
       public :: Euler_time_no_diff_AB2_sources_no_correction
       public :: Euler_time_no_diff_Euler_sources
       public :: Euler_time_no_diff_Euler_sources_no_correction

       real(cp),parameter :: two_thirds = 2.0_cp/3.0_cp
       real(cp),parameter :: four_thirds = 4.0_cp/3.0_cp
       real(cp),parameter :: neg_one_third = -1.0_cp/3.0_cp
       real(cp),parameter :: three_halfs = 3.0_cp/2.0_cp

       contains

       subroutine Euler_time_Euler_sources(PCG_VF,PCG_SF,X,Xstar,Xnm1,phi,F,m,&
         TMP,temp_F1,temp_CC,compute_norms)
         ! Solves:
         !
         !  X^{*} - X^{n}
         ! -------------- + AX^{*} = F^{n}
         !        dt
         !
         ! -->  (I + dt 1 A)X^{*} = X^{n} + dt F^{n}
         !
         ! lap(phi^{n+1}) = 1/dt div(X^{*})
         ! X^{n+1} = X^{*} - dt grad(phi^{n+1})
         !
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_VF
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: phi,temp_CC
         type(VF),intent(inout) :: X,Xstar,Xnm1
         type(VF),intent(in) :: F
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         type(VF),intent(inout) :: temp_F1
         logical,intent(in) :: compute_norms
         call assign(temp_F1,F)
         call multiply(temp_F1,TMP%TS%dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,X)
         call add(temp_F1,X)
         call assign(Xnm1,X)
         call update_MFP(PCG_VF,m,TMP%TS%dt*1.0_cp*PCG_VF%MFP%coeff_implicit,TMP%n_step.le.2)
         call solve(PCG_VF,Xstar,temp_F1,m,compute_norms) ! Solve for X*
         ! call clean_div(PCG_SF,X,Xstar,phi,1.0_cp/TMP%TS%dt,m,temp_F1,temp_CC,compute_norms)
         call clean_div(PCG_SF,X,Xstar,phi,1.0_cp,m,temp_F1,temp_CC,compute_norms)
       end subroutine

       subroutine O2_BDF_time_AB2_sources(PCG_VF,PCG_SF,X,Xstar,Xnm1,phi,F,Fnm1,m,&
         TMP,temp_F1,temp_CC,compute_norms)
         ! Solves:
         !
         !  3X^{*} - 4X^{n} + X^{n-1}
         ! -------------------------- + AX^{*} = AB2(F^{n},F^{n-1})
         !            2 dt
         !
         ! -->  (I + 2/3*dt A)X^{*} = 4/3X^{n} - 1/3 X^{n-1} + 2/3 dt AB2(F^{n},F^{n-1})
         !
         ! lap(phi^{n+1}) = 3/(2 dt) div(X^{*})
         ! X^{n+1} = X^{*} - 2/3 dt grad(phi^{n+1})
         !
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_VF
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: phi
         type(VF),intent(inout) :: X,Xstar,Xnm1
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         type(VF),intent(inout) :: temp_F1
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call AB2(temp_F1,F,Fnm1)
         call multiply(temp_F1,two_thirds*TMP%TS%dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,X)
         call add_product(temp_F1,X,four_thirds)
         call add_product(temp_F1,Xnm1,neg_one_third)
         call assign(Xnm1,X)
         call update_MFP(PCG_VF,m,TMP%TS%dt*two_thirds*PCG_VF%MFP%coeff_implicit,TMP%n_step.le.2)
         call solve(PCG_VF,Xstar,temp_F1,m,compute_norms) ! Solve for X*
         ! call clean_div(PCG_SF,X,Xstar,phi,three_halfs/TMP%TS%dt,m,temp_F1,temp_CC,compute_norms)
         call clean_div(PCG_SF,X,Xstar,phi,1.0_cp,m,temp_F1,temp_CC,compute_norms)
       end subroutine

       subroutine Euler_time_AB2_sources(PCG_VF,PCG_SF,X,Xstar,Xnm1,phi,F,Fnm1,m,&
         TMP,temp_F1,L,temp_CC,compute_norms)
         ! Solves:
         !
         !  X^{*} - X^{n}
         ! -------------- + AX^{*} = AB2(F^{n},F^{n-1})
         !        dt
         !
         ! -->  (I + dt 1 A)X^{*} = X^{n} + dt AB2(F^{n},F^{n-1})
         !
         ! lap(phi^{n+1}) = 1/dt div(X^{*})
         ! X^{n+1} = X^{*} - dt grad(phi^{n+1})
         !
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_VF
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: phi
         type(VF),intent(inout) :: X,Xstar,Xnm1,L
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         type(VF),intent(inout) :: temp_F1
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call AB2(temp_F1,F,Fnm1)
         call add(temp_F1,L)
         call multiply(temp_F1,TMP%TS%dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,X)
         call add(temp_F1,X)
         call assign(Xnm1,X)
         call update_MFP(PCG_VF,m,TMP%TS%dt*1.0_cp*PCG_VF%MFP%coeff_implicit,.true.)
         call solve(PCG_VF,Xstar,temp_F1,m,compute_norms) ! Solve for X*
         call clean_div(PCG_SF,X,Xstar,phi,1.0_cp/TMP%TS%dt,m,temp_F1,temp_CC,compute_norms)
       end subroutine

       subroutine Euler_time_AB2_sources_FFT(PCG_VF,FFT_SF,X,Xstar,Xnm1,phi,F,Fnm1,m,&
         TMP,temp_F1,L,temp_CC,compute_norms)
         ! Solves:
         !
         !  X^{*} - X^{n}
         ! -------------- + AX^{*} = AB2(F^{n},F^{n-1})
         !        dt
         !
         ! -->  (I + dt 1 A)X^{*} = X^{n} + dt AB2(F^{n},F^{n-1})
         !
         ! lap(phi^{n+1}) = 1/dt div(X^{*})
         ! X^{n+1} = X^{*} - dt grad(phi^{n+1})
         !
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_VF
         type(FFT_solver_SF),intent(inout) :: FFT_SF
         type(SF),intent(inout) :: phi
         type(VF),intent(inout) :: X,Xstar,Xnm1,L
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         type(VF),intent(inout) :: temp_F1
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call AB2(temp_F1,F,Fnm1)
         call add(temp_F1,L)
         call multiply(temp_F1,TMP%TS%dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,X)
         call add(temp_F1,X)
         call assign(Xnm1,X)
         call update_MFP(PCG_VF,m,TMP%TS%dt*1.0_cp*PCG_VF%MFP%coeff_implicit,.true.)
         call solve(PCG_VF,Xstar,temp_F1,m,compute_norms) ! Solve for X*
         call clean_div(FFT_SF,X,Xstar,phi,1.0_cp/TMP%TS%dt,m,temp_F1,temp_CC,compute_norms)
       end subroutine

       subroutine Euler_time_RK_sources(PCG_VF,PCG_SF,X,Xstar,Xnm1,phi,F,Fnm1,L,m,&
         TMP,RKP,temp_F1,temp_CC,compute_norms)
         ! Time discretization adopted from:
         ! Lundbladh, Anders, et al. "An efficient spectral method for
         ! simulation of incompressible flow over a flat plate."
         ! Trita-mek. Tech. Rep 11 (1999).
         !
         ! Solves:
         !
         !  X^{*} - X^{n}
         ! -------------- + AX^{*} = RK(F^{n})
         !        dt
         !
         ! -->  (I + dt 1 A)X^{*} = X^{n} + dt RK(F^{n})
         !
         ! lap(phi^{n+1}) = 1/dt div(X^{*})
         ! X^{n+1} = X^{*} - dt grad(phi^{n+1})
         !
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_VF
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: phi
         type(VF),intent(inout) :: X,Xstar,Xnm1
         type(VF),intent(in) :: F,Fnm1,L
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         type(RK_Params),intent(in) :: RKP
         type(VF),intent(inout) :: temp_F1
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call multiply(temp_F1,F      ,TMP%TS%dt*RKP%gamma%f(RKP%n))
         call add_product(temp_F1,Fnm1,TMP%TS%dt*RKP%zeta%f(RKP%n))
         ! call add_product(temp_F1,L   ,TMP%TS%dt*RKP%alpha%f(RKP%n))
         call add_product(temp_F1,L   ,TMP%TS%dt*2.0_cp*RKP%alpha%f(RKP%n))
         call assign_wall_Dirichlet(temp_F1,0.0_cp,X)
         call add(temp_F1,X)
         call assign(Xnm1,X)
         ! call update_MFP(PCG_VF,m,TMP%TS%dt*RKP%d%f(RKP%n)*PCG_VF%MFP%coeff_implicit,.true.)
         call update_MFP(PCG_VF,m,TMP%TS%dt*2.0_cp*RKP%beta%f(RKP%n)*PCG_VF%MFP%coeff_implicit,.true.)
         call solve(PCG_VF,Xstar,temp_F1,m,compute_norms) ! Solve for X*
         ! call clean_div(PCG_SF,X,Xstar,phi,1.0_cp/TMP%TS%dt,m,temp_F1,temp_CC,compute_norms)
         call clean_div(PCG_SF,X,Xstar,phi,1.0_cp,m,temp_F1,temp_CC,compute_norms)
         ! call clean_div(PCG_SF,X,Xstar,phi,1.0_cp/(TMP%TS%dt*RKP%gamma%f(RKP%n)),m,temp_F1,temp_CC,compute_norms)
       end subroutine

       subroutine RK_method_1(PCG_VF,PCG_SF,X,Xstar,Xnm1,phi,F,Fnm1,L,m,&
         TMP,RKP,temp_F1,temp_CC,compute_norms)
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_VF
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: phi
         type(VF),intent(inout) :: X,Xstar,Xnm1
         type(VF),intent(in) :: F,Fnm1,L
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         type(RK_Params),intent(in) :: RKP
         type(VF),intent(inout) :: temp_F1
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call multiply(temp_F1,F      ,two_thirds)
         call multiply(temp_F1,F      ,TMP%TS%dt*RKP%gamma%f(RKP%n))
         call add_product(temp_F1,Fnm1,TMP%TS%dt*RKP%zeta%f(RKP%n))
         ! call add_product(temp_F1,L   ,TMP%TS%dt*RKP%alpha%f(RKP%n))
         call add_product(temp_F1,L   ,TMP%TS%dt*2.0_cp*RKP%alpha%f(RKP%n))
         call assign_wall_Dirichlet(temp_F1,0.0_cp,X)
         call add(temp_F1,X)
         call assign(Xnm1,X)
         ! call update_MFP(PCG_VF,m,TMP%TS%dt*RKP%d%f(RKP%n)*PCG_VF%MFP%coeff_implicit,.true.)
         call update_MFP(PCG_VF,m,TMP%TS%dt*2.0_cp*RKP%beta%f(RKP%n)*PCG_VF%MFP%coeff_implicit,.true.)
         call solve(PCG_VF,Xstar,temp_F1,m,compute_norms) ! Solve for X*
         ! call clean_div(PCG_SF,X,Xstar,phi,1.0_cp/TMP%TS%dt,m,temp_F1,temp_CC,compute_norms)
         call clean_div(PCG_SF,X,Xstar,phi,1.0_cp,m,temp_F1,temp_CC,compute_norms)
         ! call clean_div(PCG_SF,X,Xstar,phi,1.0_cp/(TMP%TS%dt*RKP%gamma%f(RKP%n)),m,temp_F1,temp_CC,compute_norms)
       end subroutine

       ! **********************************************************************
       ! ******************** DIFFUSION EXPLICIT TIME MARCHING ****************
       ! **********************************************************************

       subroutine Euler_time_no_diff_AB2_sources(PCG_SF,X,Xstar,Xnm1,phi,F,Fnm1,m,TMP,&
         temp_F1,temp_CC,compute_norms)
         ! Solves:
         !
         !  X^{*} - X^{n}
         ! -------------- = AB2(F^{n},F^{n-1})
         !        dt
         !
         ! -->  X^{*} = X^{n} + dt AB2(F^{n},F^{n-1})
         !
         ! lap(phi^{n+1}) = 1/dt div(X^{*})
         ! X^{n+1} = X^{*} - dt grad(phi^{n+1})
         !
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: phi
         type(VF),intent(inout) :: X,Xnm1,Xstar
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         type(VF),intent(inout) :: temp_F1
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call AB2(Xstar,F,Fnm1)
         call multiply(Xstar,TMP%TS%dt)
         call assign_wall_Dirichlet(Xstar,0.0_cp,X)
         call add(Xstar,X)
         call assign(Xnm1,X)
         call clean_div(PCG_SF,X,Xstar,phi,1.0_cp,m,temp_F1,temp_CC,compute_norms)
         ! call clean_div(PCG_SF,X,Xstar,phi,1.0_cp/TMP%TS%dt,m,temp_F1,temp_CC,compute_norms)
       end subroutine

       subroutine Euler_time_no_diff_Euler_sources(PCG_SF,X,Xstar,Xnm1,phi,F,m,TMP,&
         temp_F1,temp_CC,compute_norms)
         ! Solves:
         !
         !  X^{*} - X^{n}
         ! -------------- = F^{n}
         !        dt
         !
         ! -->  X^{*} = X^{n} + dt AB2(F^{n},F^{n-1})
         !
         ! lap(phi^{n+1}) = 1/dt div(X^{*})
         ! X^{n+1} = X^{*} - dt grad(phi^{n+1})
         !
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: phi
         type(VF),intent(inout) :: X,Xnm1,Xstar
         type(VF),intent(in) :: F
         type(mesh),intent(in) :: m
         type(time_marching_params),intent(in) :: TMP
         type(VF),intent(inout) :: temp_F1
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call multiply(Xstar,F,TMP%TS%dt)
         call assign_wall_Dirichlet(Xstar,0.0_cp,X)
         call add(Xstar,X)
         call assign(Xnm1,X)
         call clean_div(PCG_SF,X,Xstar,phi,1.0_cp,m,temp_F1,temp_CC,compute_norms)
         ! call clean_div(PCG_SF,X,Xstar,phi,1.0_cp/TMP%TS%dt,m,temp_F1,temp_CC,compute_norms)
       end subroutine

       subroutine Euler_time_no_diff_AB2_sources_no_correction(X,Xstar,F,Fnm1,TMP)
         ! Solves:
         !
         !  X^{n+1} - X^{n}
         ! ---------------- = AB2(F^{n},F^{n-1})
         !          dt
         !
         ! -->  X^{n+1} = X^{n} + dt AB2(F^{n},F^{n-1})
         !
         implicit none
         type(VF),intent(inout) :: X,Xstar
         type(VF),intent(in) :: F,Fnm1
         type(time_marching_params),intent(in) :: TMP
         call AB2(Xstar,F,Fnm1)
         call multiply(Xstar,TMP%TS%dt)
         call assign_wall_Dirichlet(Xstar,0.0_cp,X)
         call add(X,Xstar)
         call apply_BCs(X)
       end subroutine

       subroutine Euler_time_no_diff_Euler_sources_no_correction(X,Xstar,F,TMP)
         ! Solves:
         !
         !  X^{n+1} - X^{n}
         ! ---------------- = F^{n}
         !          dt
         !
         ! -->  X^{n+1} = X^{n} + dt F^{n}
         !
         implicit none
         type(VF),intent(inout) :: X,Xstar
         type(VF),intent(in) :: F
         type(time_marching_params),intent(in) :: TMP
         call multiply(Xstar,F,TMP%TS%dt)
         call assign_wall_Dirichlet(Xstar,0.0_cp,X)
         call add(X,Xstar)
         call apply_BCs(X)
       end subroutine

       end module