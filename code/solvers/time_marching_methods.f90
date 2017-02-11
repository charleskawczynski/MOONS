       module time_marching_methods_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use norms_mod
       use AB2_mod
       use compute_energy_mod
       use export_raw_processed_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_advect_mod
       use ops_norms_mod
       use apply_BCs_mod
       use PCG_mod
       use matrix_free_operators_mod
       use matrix_free_params_mod
       use clean_divergence_mod
       use update_intermediate_field_BCs_mod

       implicit none
       private

       public :: O2_BD_time_AB2_sources_PCG_PCG
       public :: CD_time_AB2_sources_PCG_PCG

       contains

       subroutine O2_BD_time_AB2_sources_PCG_PCG(PCG_VF,PCG_SF,X,Xstar,Xnm1,phi,F,Fnm1,m,&
         dt,temp_F1,temp_F2,temp_CC,temp_CC_VF,compute_norms)
         ! Solves:
         !
         !  3X^{*} - 4X^{n} + X^{n-1}
         ! -------------------------- = AB2(F^{n},F^{n-1})
         !            2 dt
         !
         ! lap(phi^{n+1}) = div(X^{*})
         ! X^{n+1} = X^{*} - grad(phi^{n+1})
         !
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_VF
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: phi
         type(VF),intent(inout) :: X,Xstar,Xnm1,temp_CC_VF
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         type(VF),intent(inout) :: temp_F1,temp_F2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call AB2(temp_F1,F,Fnm1)
         call multiply(temp_F1,dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,X)
         call add_product(temp_F1,X,4.0_cp/3.0_cp)
         call add_product(temp_F1,Xnm1,-1.0_cp/3.0_cp)
         call assign(Xnm1,X)
         call solve(PCG_VF,Xstar,temp_F1,m,compute_norms) ! Solve for X*
         call clean_div(PCG_SF,X,Xstar,phi,m,temp_F2,temp_CC,compute_norms)
         call update_intermediate_field_BCs(Xstar,X,phi,m,temp_F1,temp_F2,temp_CC_VF)
       end subroutine

       subroutine CD_time_AB2_sources_PCG_PCG(PCG_VF,PCG_SF,X,Xstar,Xnm1,phi,F,Fnm1,m,&
         dt,temp_F1,temp_F2,temp_CC,temp_CC_VF,compute_norms)
         ! Solves:
         !
         !  X^{*} - X^{n}
         ! -------------- = AB2(F^{n},F^{n-1})
         !      2 dt
         !
         ! lap(phi^{n+1}) = div(X^{*})
         ! X^{n+1} = X^{*} - grad(phi^{n+1})
         !
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG_VF
         type(PCG_solver_SF),intent(inout) :: PCG_SF
         type(SF),intent(inout) :: phi
         type(VF),intent(inout) :: X,Xstar,Xnm1,temp_CC_VF
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         type(VF),intent(inout) :: temp_F1,temp_F2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call AB2(temp_F1,F,Fnm1)
         call multiply(temp_F1,dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,X)
         call add(temp_F1,X)
         call assign(Xnm1,X)
         call solve(PCG_VF,Xstar,temp_F1,m,compute_norms) ! Solve for X*
         call clean_div(PCG_SF,X,Xstar,phi,m,temp_F2,temp_CC,compute_norms)
         call update_intermediate_field_BCs(Xstar,X,phi,m,temp_F1,temp_F2,temp_CC_VF)
       end subroutine

       ! **********************************************************************
       ! *********************** EXPLICIT TIME MARCHING ***********************
       ! **********************************************************************

       subroutine CD_time_AB2_sources_PCG_SF(PCG,X,Xstar,phi,F,Fnm1,m,dt,&
         temp_F1,temp_F2,temp_CC,compute_norms)
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG
         type(SF),intent(inout) :: phi
         type(VF),intent(inout) :: X,Xstar
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         type(VF),intent(inout) :: temp_F1,temp_F2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call AB2(temp_F1,F,Fnm1)
         call multiply(temp_F1,dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,X)
         call add(temp_F1,X)
         call assign(Xnm1,X)
         call add(X,temp_F1)
         call assign(Xstar,X)
         call clean_div(PCG,X,Xstar,phi,m,temp_F2,temp_CC,compute_norms)
       end subroutine

       end module