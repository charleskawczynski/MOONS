       module momentum_solver_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use norms_mod
       use AB2_mod
       use export_raw_processed_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_physics_mod
       use apply_BCs_mod
       use SOR_mod
       use CG_mod
       use PCG_mod
       use GS_poisson_mod
       use matrix_free_operators_mod
       
       implicit none
       private
       
       ! Routine names are besed on the following convention:
       ! 1) Time marching scheme (Explicit Euler, Diffusion-Implicit, etc.)
       ! 2) PPE method (Gauss-Seidel, SOR, multigrid, FFT, etc.)
       ! 3) Advection term treatment (Divergence form, advective form)
       ! 4) Mean pressure gradient (included or not)

       public :: CN_AB2_PPE_CG_mom_CG  ! Tested on 12/30/2015
       public :: CN_AB2_PPE_CG_mom_PCG ! Tested on 12/30/2015
       public :: CN_AB2_PPE_GS_mom_CG
       public :: Euler_CG_Donor
       public :: Euler_GS_Donor
       public :: Euler_GS_Donor_mpg


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

       subroutine CN_AB2_PPE_CG_mom_CG(mom_CG,PPE_CG,U,Unm1,p,F,Fnm1,U_CC,m,&
         Re,dt,Nmax_PPE,Nmax_mom,Ustar,temp_F,temp_CC,temp_E1,temp_E2,compute_norms)
         implicit none
         type(CG_solver_VF),intent(inout) :: mom_CG
         type(CG_solver_SF),intent(inout) :: PPE_CG
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,U_CC,Unm1
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: Nmax_PPE,Nmax_mom
         type(VF),intent(inout) :: Ustar,temp_F,temp_E1,temp_E2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_div(temp_F,Unm1,Unm1,temp_E1,temp_E2,U_CC,m)
         call advect_div(Ustar,U,U,temp_E1,temp_E2,U_CC,m)
         call AB2_overwrite(Ustar,temp_F)
         call multiply(Ustar,-1.0_cp) ! Because advect_div gives positive

         call lap(temp_F,U,m)
         call multiply(temp_F,0.5_cp/Re)
         call add(Ustar,temp_F)

         call AB2(temp_F,F,Fnm1)
         call add(Ustar,temp_F)

         call multiply(Ustar,dt)
         call add(Ustar,U)
         call assign(Unm1,U)

         call solve(mom_CG,U,Ustar,m,Nmax_mom,compute_norms) ! Solve for Ustar

         call zeroWall_conditional(U,m)
         call div(temp_CC,U,m)
         call multiply(temp_CC,1.0_cp/dt)
         call zeroGhostPoints(temp_CC)
         call solve(PPE_CG,p,temp_CC,m,Nmax_PPE,compute_norms)
         call grad(temp_F,p,m)
         call multiply(temp_F,dt)
         call subtract(U,temp_F) ! U = Ustar - grad(p)
         call apply_BCs(U,m)
       end subroutine

       subroutine CN_AB2_PPE_CG_mom_PCG(mom_PCG,PPE_CG,U,Unm1,p,F,Fnm1,U_CC,m,&
         Re,dt,Nmax_PPE,Nmax_mom,Ustar,temp_F,temp_CC,temp_E1,temp_E2,compute_norms)
         implicit none
         type(PCG_solver_VF),intent(inout) :: mom_PCG
         type(CG_solver_SF),intent(inout) :: PPE_CG
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,U_CC,Unm1
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: Nmax_PPE,Nmax_mom
         type(VF),intent(inout) :: Ustar,temp_F,temp_E1,temp_E2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_div(temp_F,Unm1,Unm1,temp_E1,temp_E2,U_CC,m)
         call advect_div(Ustar,U,U,temp_E1,temp_E2,U_CC,m)
         call AB2_overwrite(Ustar,temp_F)
         call multiply(Ustar,-1.0_cp) ! Because advect_div gives positive

         call lap(temp_F,U,m)
         call multiply(temp_F,0.5_cp/Re)
         call add(Ustar,temp_F)

         call AB2(temp_F,F,Fnm1)
         call add(Ustar,temp_F)

         call multiply(Ustar,dt)
         call add(Ustar,U)
         call assign(Unm1,U)

         call solve(mom_PCG,U,Ustar,m,Nmax_mom,compute_norms) ! Solve for Ustar

         call zeroWall_conditional(U,m)
         call div(temp_CC,U,m)
         call multiply(temp_CC,1.0_cp/dt)
         call zeroGhostPoints(temp_CC)
         call solve(PPE_CG,p,temp_CC,m,Nmax_PPE,compute_norms)
         call grad(temp_F,p,m)
         call multiply(temp_F,dt)
         call subtract(U,temp_F) ! U = Ustar - grad(p)
         call apply_BCs(U,m)
       end subroutine

       subroutine CN_AB2_PPE_GS_mom_CG(mom_CG,PPE_GS,U,Unm1,p,F,Fnm1,U_CC,m,&
         Re,dt,Nmax_PPE,Nmax_mom,Ustar,temp_F,temp_CC,temp_E1,temp_E2,compute_norms)
         implicit none
         type(CG_solver_VF),intent(inout) :: mom_CG
         type(GS_poisson),intent(inout) :: PPE_GS
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,U_CC,Unm1
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: Nmax_PPE,Nmax_mom
         type(VF),intent(inout) :: Ustar,temp_F,temp_E1,temp_E2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_div(temp_F,Unm1,Unm1,temp_E1,temp_E2,U_CC,m)
         call advect_div(Ustar,U,U,temp_E1,temp_E2,U_CC,m)
         call AB2_overwrite(Ustar,temp_F)
         call multiply(Ustar,-1.0_cp) ! Because advect_div gives positive

         call lap(temp_F,U,m)
         call multiply(temp_F,0.5_cp/Re)
         call add(Ustar,temp_F)

         call AB2(temp_F,F,Fnm1)
         call add(Ustar,temp_F)

         call multiply(Ustar,dt)
         call add(Ustar,U)
         call zeroWall_conditional(Ustar,m,U)
         call assign(Unm1,U)

         call solve(mom_CG,U,Ustar,m,Nmax_mom,compute_norms)

         call div(temp_CC,U,m)
         call divide(temp_CC,dt)
         call zeroGhostPoints(temp_CC)
         call solve(PPE_GS,p,temp_CC,m,Nmax_PPE,compute_norms)
         call grad(temp_F,p,m)
         call multiply(temp_F,dt)
         call subtract(U,temp_F)
         call apply_BCs(U,m)
       end subroutine

       ! **********************************************************************
       ! **********************************************************************
       ! *********************** EXPLICIT TIME MARCHING ***********************
       ! **********************************************************************
       ! **********************************************************************

       subroutine Euler_CG_Donor(CG,U,p,F,U_CC,m,Re,dt,n,&
         Ustar,temp_F,temp_CC,temp_E1,temp_E2,compute_norms)
         implicit none
         type(CG_solver_SF),intent(inout) :: CG
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,U_CC
         type(VF),intent(in) :: F
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: n
         type(VF),intent(inout) :: Ustar,temp_F,temp_E1,temp_E2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_div(temp_F,U,U,temp_E1,temp_E2,U_CC,m)
         call assign_negative(Ustar,temp_F)
         call lap(temp_F,U,m)
         call multiply(temp_F,1.0_cp/Re)
         call add(Ustar,temp_F)
         call add(Ustar,F)
         call zeroWall_conditional(Ustar,m,U)
         call multiply(Ustar,dt)
         call add(Ustar,U)
         call div(temp_CC,Ustar,m)
         call divide(temp_CC,dt)
         call zeroGhostPoints(temp_CC)
         call solve(CG,p,temp_CC,m,n,compute_norms)
         call grad(temp_F,p,m)
         call multiply(temp_F,dt)
         call subtract(U,Ustar,temp_F)
         call apply_BCs(U,m)
       end subroutine

       subroutine Euler_GS_Donor(GS,U,p,F,U_CC,m,Re,dt,n,&
         Ustar,temp_F,temp_CC,temp_E1,temp_E2,compute_norms)
         implicit none
         type(GS_poisson),intent(inout) :: GS
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,U_CC
         type(VF),intent(in) :: F
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: n
         type(VF),intent(inout) :: Ustar,temp_F,temp_E1,temp_E2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_div(temp_F,U,U,temp_E1,temp_E2,U_CC,m)
         call assign_negative(Ustar,temp_F)
         call lap(temp_F,U,m)
         call divide(temp_F,Re)
         call add(Ustar,temp_F)
         call add(Ustar,F)
         call zeroWall_conditional(Ustar,m,U)
         call multiply(Ustar,dt)
         call add(Ustar,U)
         call div(temp_CC,Ustar,m)
         call divide(temp_CC,dt)
         call zeroGhostPoints(temp_CC)
         call solve(GS,p,temp_CC,m,n,compute_norms)
         call grad(temp_F,p,m)
         call multiply(temp_F,dt)
         call subtract(U,Ustar,temp_F)
         call apply_BCs(U,m)
       end subroutine

       subroutine Euler_GS_Donor_mpg(GS,U,p,F,U_CC,mpg,m,Re,dt,n,&
         Ustar,temp_F,temp_CC,temp_E1,temp_E2,compute_norms)
         implicit none
         type(GS_poisson),intent(inout) :: GS
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,U_CC
         type(VF),intent(in) :: F,mpg
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: n
         type(VF),intent(inout) :: Ustar,temp_F,temp_E1,temp_E2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_div(temp_F,U,U,temp_E1,temp_E2,U_CC,m)
         call assign_negative(Ustar,temp_F)
         call lap(temp_F,U,m)
         call divide(temp_F,Re)
         call add(Ustar,temp_F)
         call add(Ustar,F)
         call zeroWall_conditional(Ustar,m,U)
         call multiply(Ustar,dt)
         call add(Ustar,U)
         call div(temp_CC,Ustar,m)
         call divide(temp_CC,dt)
         call zeroGhostPoints(temp_CC)
         call solve(GS,p,temp_CC,m,n,compute_norms)
         call grad(temp_F,p,m)
         call subtract(temp_F,mpg)
         call multiply(temp_F,dt)
         call subtract(U,Ustar,temp_F)
         call apply_BCs(U,m)
       end subroutine

       end module