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
       public :: CN_AB2_PPE_GS_mom_PCG

       public :: Euler_PCG_Donor
       public :: Euler_Donor_no_PPE
       public :: Euler_GS_Donor
       public :: Euler_GS_Donor_mpg

       contains

       subroutine CN_AB2_PPE_PCG_mom_PCG(mom_PCG,PPE_PCG,U,Ustar,Unm1,U_E,p,F,Fnm1,m,&
         MFP,dt,temp_F1,temp_F2,temp_CC,temp_CC_VF,temp_E,compute_norms)
         implicit none
         type(PCG_solver_VF),intent(inout) :: mom_PCG
         type(PCG_solver_SF),intent(inout) :: PPE_PCG
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,Ustar,Unm1,temp_CC_VF
         type(TF),intent(inout) :: U_E
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         type(matrix_free_params),intent(in) :: MFP
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_E
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_U(temp_F1,U,U_E,m,.false.,temp_E,temp_CC)
         call advect_U(temp_F2,Unm1,U_E,m,.true.,temp_E,temp_CC)
         call AB2_overwrite(temp_F1,temp_F2)
         call multiply(temp_F1,-dt) ! Because advect_div gives positive
         ! call lap(temp_F2,U,m)
         call lap_centered(temp_F2,U,m)
         call multiply(temp_F2,MFP%coeff_explicit)
         call add(temp_F1,temp_F2)
         call AB2(temp_F2,F,Fnm1)
         call multiply(temp_F2,dt)
         call add(temp_F1,temp_F2)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,U)
         call add(temp_F1,U)
         call assign(Unm1,U)

         call solve(mom_PCG,Ustar,temp_F1,m,compute_norms) ! Solve for U*
         call clean_div(PPE_PCG,U,Ustar,p,m,temp_F2,temp_CC,compute_norms)
         call update_intermediate_field_BCs(Ustar,U,p,m,temp_F1,temp_F2,temp_CC_VF)
       end subroutine

       subroutine CN_AB2_PPE_GS_mom_PCG(mom_PCG,PPE_GS,U,Ustar,Unm1,U_E,p,F,Fnm1,m,&
         Re,dt,temp_F1,temp_F2,temp_CC,temp_CC_VF,temp_E,compute_norms)
         implicit none
         type(PCG_solver_VF),intent(inout) :: mom_PCG
         type(GS_Poisson_SF),intent(inout) :: PPE_GS
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,Ustar,Unm1
         type(TF),intent(inout) :: U_E
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_E,temp_CC_VF
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_U(temp_F1,U,U_E,m,.false.,temp_E,temp_CC)
         call advect_U(temp_F2,Unm1,U_E,m,.true.,temp_E,temp_CC)
         call AB2_overwrite(temp_F1,temp_F2)
         call multiply(temp_F1,-1.0_cp) ! Because advect_div gives positive

         call lap(temp_F2,U,m)
         call multiply(temp_F2,0.5_cp/Re)
         call add(temp_F1,temp_F2)

         call AB2(temp_F2,F,Fnm1)
         call add(temp_F1,temp_F2)

         call multiply(temp_F1,dt)
         call add(temp_F1,U)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,U)
         call assign(Unm1,U)

         call solve(mom_PCG,Ustar,temp_F1,m,compute_norms)
         call clean_div(PPE_GS,U,Ustar,p,m,temp_F2,temp_CC,compute_norms)
         call update_intermediate_field_BCs(Ustar,U,p,m,temp_F1,temp_F2,temp_CC_VF)
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
         real(cp),intent(in) :: Re,dt
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_E
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_U(temp_F2,U,U_E,m,.false.,temp_E,temp_CC)
         call multiply(temp_F1,temp_F2,-1.0_cp) ! Because advect_div gives positive
         ! call laplacian_matrix_based(temp_F2,U,m) ! O(dx^1) near boundaries
         call lap_centered(temp_F2,U,m) ! Seems to work better for stitching, but O(dx^1) on boundaries
         ! call lap(temp_F2,U,m) ! O(dx^2) near boundaries
         call multiply(temp_F2,1.0_cp/Re)
         call add(temp_F1,temp_F2)
         call add(temp_F1,F)
         call multiply(temp_F1,dt)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,U)
         call add(U,temp_F1)
         call assign(Ustar,U)
         call clean_div(PCG,U,Ustar,p,m,temp_F2,temp_CC,compute_norms)
       end subroutine

       subroutine Euler_Donor_no_PPE(U,U_E,F,m,Re,dt,&
         temp_F1,temp_F2,temp_CC,temp_E)
         implicit none
         type(VF),intent(inout) :: U
         type(TF),intent(inout) :: U_E
         type(VF),intent(in) :: F
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_E
         type(SF),intent(inout) :: temp_CC
         call advect_U(temp_F2,U,U_E,m,.false.,temp_E,temp_CC)
         call multiply(temp_F1,temp_F2,-1.0_cp) ! Because advect_div gives positive
         call lap(temp_F2,U,m)
         ! call lap_centered(temp_F2,U,m) ! Seems to work better for stitching, but O(dx^1) on boundaries
         call multiply(temp_F2,1.0_cp/Re)
         call add(temp_F1,temp_F2)
         call add(temp_F1,F)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,U)
         call multiply(temp_F1,dt)
         call add(temp_F1,U)
         call assign(temp_F2,0.0_cp)
         call subtract(temp_F2%x,1.0_cp) ! mpg
         call multiply(temp_F2,dt)
         call subtract(U,temp_F1,temp_F2)
         call apply_BCs(U)
       end subroutine

       subroutine Euler_GS_Donor(GS,U,Ustar,U_E,p,F,m,Re,dt,&
         temp_F1,temp_F2,temp_CC,temp_E,compute_norms)
         implicit none
         type(GS_Poisson_SF),intent(inout) :: GS
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,Ustar
         type(TF),intent(inout) :: U_E
         type(VF),intent(in) :: F
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_E
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_U(temp_F2,U,U_E,m,.false.,temp_E,temp_CC)
         call multiply(temp_F1,temp_F2,-1.0_cp) ! Because advect_div gives positive
         ! call lap(temp_F2,U,m)
         call lap_centered(temp_F2,U,m) ! Seems to work better for stitching, but O(dx^1) on boundaries
         call multiply(temp_F2,1.0_cp/Re)
         call add(temp_F1,temp_F2)
         call add(temp_F1,F)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,U)
         call multiply(temp_F1,dt)
         call add(U,temp_F1)
         call assign(Ustar,U)
         call clean_div(GS,U,Ustar,p,m,temp_F2,temp_CC,compute_norms)
       end subroutine

       subroutine Euler_GS_Donor_mpg(GS,U,Ustar,U_E,p,F,mpg,m,Re,dt,&
         temp_F1,temp_F2,temp_CC,temp_E,compute_norms)
         implicit none
         type(GS_Poisson_SF),intent(inout) :: GS
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,Ustar
         type(TF),intent(inout) :: U_E
         type(VF),intent(in) :: F,mpg
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_E
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_U(temp_F2,U,U_E,m,.false.,temp_E,temp_CC)
         call multiply(temp_F1,temp_F2,-1.0_cp) ! Because advect_div gives positive
         call lap(temp_F2,U,m)
         call multiply(temp_F2,1.0_cp/Re)
         call add(temp_F1,temp_F2)
         call add(temp_F1,F)
         call assign_wall_Dirichlet(temp_F1,0.0_cp,U)
         call multiply(temp_F1,dt)
         call add(U,temp_F1)
         call assign(Ustar,U)
         call clean_div(GS,U,Ustar,p,mpg,m,temp_F2,temp_CC,compute_norms)
       end subroutine

       end module