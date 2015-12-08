       module momentum_solver_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use norms_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_physics_mod
       use apply_BCs_mod
       use SOR_mod
       use CG_mod
       use GS_poisson_mod
       
       implicit none
       private
       
       ! Routine names are besed on the following convention:
       ! 1) Time marching scheme (Explicit Euler, Diffusion-Implicit, etc.)
       ! 2) PPE method (Gauss-Seidel, SOR, multigrid, FFT, etc.)
       ! 3) Advection term treatment (Divergence form, advective form)
       ! 4) Mean pressure gradient (included or not)

       public :: Euler_CG_Donor
       public :: Euler_GS_Donor_mpg
       public :: Euler_GS_Donor


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

       subroutine Euler_CG_Donor(CG,U,p,F,U_CC,m,Re,dt,n,&
         Ustar,temp_F,temp_CC,temp_E1,temp_E2,compute_norms)
         implicit none
         type(CG_solver),intent(inout) :: CG
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,U_CC
         type(VF),intent(in) :: F
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: n
         type(VF),intent(inout) :: Ustar,temp_F,temp_E1,temp_E2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call faceAdvectDonor(temp_F,U,U,temp_E1,temp_E2,U_CC,m)
         call assignMinus(Ustar,temp_F)
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
         call solve(CG,p,temp_CC,m,n,compute_norms)
         call grad(temp_F,p,m)
         call multiply(temp_F,dt)
         call subtract(U,Ustar,temp_F)
         call applyAllBCs(U,m)
       end subroutine

       subroutine Euler_GS_Donor_mpg(GS,U,p,F,U_CC,mpg,m,Re,dt,n,&
         Ustar,temp_F,temp_CC,temp_E1,temp_E2,compute_norms)
         implicit none
         type(GS_solver),intent(inout) :: GS
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,U_CC
         type(VF),intent(in) :: F,mpg
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: n
         type(VF),intent(inout) :: Ustar,temp_F,temp_E1,temp_E2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call faceAdvectDonor(temp_F,U,U,temp_E1,temp_E2,U_CC,m)
         call assignMinus(Ustar,temp_F)
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
         call applyAllBCs(U,m)
       end subroutine

       subroutine Euler_GS_Donor(GS,U,p,F,U_CC,m,Re,dt,n,&
         Ustar,temp_F,temp_CC,temp_E1,temp_E2,compute_norms)
         implicit none
         type(GS_solver),intent(inout) :: GS
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,U_CC
         type(VF),intent(in) :: F
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: n
         type(VF),intent(inout) :: Ustar,temp_F,temp_E1,temp_E2
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call faceAdvectDonor(temp_F,U,U,temp_E1,temp_E2,U_CC,m)
         call assignMinus(Ustar,temp_F)
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
         call applyAllBCs(U,m)
       end subroutine

       end module