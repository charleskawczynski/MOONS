       module mom_methods_mod
       use simParams_mod
       use grid_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use norms_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_physics_mod
       use applyBCs_mod
       use SOR_mod
       
       implicit none
       private
       
       public :: solve


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       interface solve;    module procedure Euler_SOR_Donor;   end interface

       contains

       subroutine Euler_SOR_Donor(SOR,U,p,F,U_CC,mpg,g,Re,dt,ss_ppe,&
         Ustar,temp_F,temp_CC,temp_E1,temp_E2,printNorms)
         implicit none
         type(SORSolver),intent(inout) :: SOR
         type(VF),intent(inout) :: p
         type(VF),intent(in) :: U,F,mpg,U_CC
         type(VF),intent(inout) :: Ustar,temp_F,temp_CC,temp_E1,temp_E2
         type(grid),intent(in) :: g
         logical,intent(in) :: printNorms
         type(solverSettings),intent(inout) :: ss_ppe
         type(norms),intent(inout) :: norm_PPE
         real(cp),intent(in) :: Re,dt
         ! Advection terms
         call faceAdvectDonor(temp_F,U,U,temp_E1,temp_E2,U_CC,g)
         call assignMinus(Ustar,temp_F) ! Ustar = -TempVF

         ! Diffusion terms
         call lap(temp_F,U,g)
         call divide(temp_F,Re)
         call add(Ustar,temp_F)

         ! Source Terms (e.g. N j x B)
         call add(Ustar,F)

         call ZWCB(Ustar,g) ! Zero wall coincident forcing (may be bad for neumann BCs)

         ! Ustar = U + dt*Ustar
         call multiply(Ustar,dt)
         call add(Ustar,U)

         ! Pressure Poisson equation ∇²p = div(u)/dt
         call div(temp_CC,Ustar,g)
         call divide(temp_CC,dt) ! O(dt) pressure treatment
         call zeroGhostPoints(temp_CC)
         call solve(SOR,p,temp_CC,g,ss_ppe,norm_PPE,printNorms)

         ! Compute pressure gradient, temp_F = ∇p
         call grad(temp_F,p,g)

         ! Add mean pressure gradient
         call subtract(temp_F,mpg)

         ! Pressure correction U^{n+1} = Ustar - dt ∇p
         call multiply(temp_F,dt)
         call subtract(U,Ustar,temp_F)

         call applyAllBCs(U,g)
       end subroutine

       end module