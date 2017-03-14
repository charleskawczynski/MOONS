       module momentum_solver_mod
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
       use GS_poisson_mod
       use matrix_free_operators_mod
       use matrix_free_params_mod
       use clean_divergence_mod
       use update_intermediate_field_BCs_mod

       implicit none
       private

       ! Routine names are besed on the following convention:
       ! 1) Time marching scheme (Explicit Euler, Diffusion-Implicit, etc.)
       ! 2) PPE method (Gauss-Seidel, SOR, multigrid, FFT, etc.)
       ! 3) Advection term treatment (Divergence form, advective form)
       ! 4) Mean pressure gradient (included or not)

       public :: CN_AB2_PPE_PCG_mom_PCG
       public :: O2_BD_CN_AB2_PPE_PCG_mom_PCG

       public :: Euler_PCG_Donor
       public :: Euler_Donor_no_PPE

       contains

       subroutine CN_AB2_PPE_PCG_mom_PCG(mom_PCG,PPE_PCG,U,Ustar,Unm1,p,F,Fnm1,m,&
         dt,temp_F1,temp_F2,temp_CC,compute_norms)
         implicit none
         type(PCG_solver_VF),intent(inout) :: mom_PCG
         type(PCG_solver_SF),intent(inout) :: PPE_PCG
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,Ustar,Unm1
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         type(VF),intent(inout) :: temp_F1,temp_F2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call AB2(temp_F1,F,Fnm1)
         call multiply(temp_F1,dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,U)
         call add(temp_F1,U)
         call assign(Unm1,U)
         call solve(mom_PCG,Ustar,temp_F1,m,compute_norms) ! Solve for U*
         call clean_div(PPE_PCG,U,Ustar,p,1.0_cp/dt,m,temp_F2,temp_CC,compute_norms)
         call update_intermediate_field_BCs(Ustar,p,1.0_cp/dt,m,temp_F1,temp_CC)
       end subroutine

       subroutine O2_BD_CN_AB2_PPE_PCG_mom_PCG(mom_PCG,PPE_PCG,U,Ustar,Unm1,p,F,Fnm1,m,&
         dt,temp_F1,temp_F2,temp_CC,compute_norms)
         implicit none
         type(PCG_solver_VF),intent(inout) :: mom_PCG
         type(PCG_solver_SF),intent(inout) :: PPE_PCG
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,Ustar,Unm1
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         type(VF),intent(inout) :: temp_F1,temp_F2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call AB2(temp_F1,F,Fnm1)
         call multiply(temp_F1,dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,U)
         call add_product(temp_F1,U,4.0_cp/3.0_cp)
         call add_product(temp_F1,Unm1,-1.0_cp/3.0_cp)
         call assign(Unm1,U)
         call solve(mom_PCG,Ustar,temp_F1,m,compute_norms) ! Solve for U*
         call clean_div(PPE_PCG,U,Ustar,p,dt,m,temp_F2,temp_CC,compute_norms)
         call update_intermediate_field_BCs(Ustar,p,dt,m,temp_F1,temp_CC)
       end subroutine

       ! **********************************************************************
       ! **********************************************************************
       ! *********************** EXPLICIT TIME MARCHING ***********************
       ! **********************************************************************
       ! **********************************************************************

       subroutine Euler_PCG_Donor(PCG,U,Ustar,U_E,p,F,m,Re,dt,&
         temp_F1,temp_F2,temp_CC,temp_E,compute_norms)
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,Ustar
         type(TF),intent(inout) :: U_E
         type(VF),intent(in) :: F
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt,Re
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_E
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_U_divergence(temp_F1,U,U_E,m,.false.,temp_E,temp_CC)
         call multiply(temp_F1,-1.0_cp)
         call lap(temp_F2,U,m) ! O(dx^2) near boundaries
         call multiply(temp_F2,1.0_cp/Re)
         call add(temp_F1,temp_F2)
         call add(temp_F1,F)
         call multiply(temp_F1,dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,U)
         call add(temp_F1,U)
         call assign(Ustar,temp_F1)
         call clean_div(PCG,U,Ustar,p,1.0_cp,m,temp_F2,temp_CC,compute_norms)
       end subroutine

       subroutine Euler_Donor_no_PPE(U,F,Fnm1,dt,temp_F1)
         implicit none
         type(VF),intent(inout) :: U
         type(VF),intent(in) :: F,Fnm1
         real(cp),intent(in) :: dt
         type(VF),intent(inout) :: temp_F1
         call AB2(temp_F1,F,Fnm1)
         ! call assign(temp_F1,F)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,U)
         call multiply(temp_F1,dt)
         call add(temp_F1,U)
         call assign(U,temp_F1)
         call apply_BCs(U)
       end subroutine

       end module