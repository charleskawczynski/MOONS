       module mom_methods_mod
       use mesh_mod
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
       
       ! Routine names are besed on the following convention:
       ! 1) Time marching scheme (Explicit Euler, Diffusion-Implicit, etc.)
       ! 2) PPE method (Gauss-Seidel, SOR, multigrid, FFT, etc.)
       ! 3) Advection term treatment (Divergence form, advective form)
       ! 4) Mean pressure gradient (included or not)

       public :: timeMarchingScheme_PPEMethod_Advection
       public :: Euler_SOR_Donor


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

       subroutine Euler_SOR_Donor_mpg(SOR,U,p,F,U_CC,mpg,m,Re,dt,ss_ppe,&
         Ustar,temp_F,temp_CC,temp_E1,temp_E2,printNorms)
         implicit none
         type(SORSolver),intent(inout) :: SOR
         type(VF),intent(inout) :: p
         type(VF),intent(in) :: U,F,mpg,U_CC
         type(VF),intent(inout) :: Ustar,temp_F,temp_CC,temp_E1,temp_E2
         type(mesh),intent(in) :: m
         logical,intent(in) :: printNorms
         type(solverSettings),intent(inout) :: ss_ppe
         type(norms),intent(inout) :: norm_PPE
         real(cp),intent(in) :: Re,dt
         
         call faceAdvectDonor(temp_F,U,U,temp_E1,temp_E2,U_CC,m) ! Advection terms
         call assignMinus(Ustar,temp_F)                          ! Ustar = -TempVF

         call lap(temp_F,U,m)                                    ! Diffusion terms
         call divide(temp_F,Re)
         call add(Ustar,temp_F)

         call add(Ustar,F)                                       ! Source Terms (e.g. N j x B)

         call zeroWall(Ustar,m,U)                                ! Zero forces on non-Neumann walls

         call multiply(Ustar,dt)                                 ! Ustar = U + dt*Ustar
         call add(Ustar,U)

         call div(temp_CC,Ustar,m)                               ! PPE: ∇²p = div(u)/dt
         call divide(temp_CC,dt)                                 ! O(dt) pressure treatment
         call zeroGhostPoints(temp_CC)
         call solve(SOR,p,temp_CC,m,ss_ppe,norm_PPE,printNorms)

         call grad(temp_F,p,m)                                   ! Pressure gradient, temp_F = ∇p

         call subtract(temp_F,mpg)                               ! Add mean pressure gradient

         call multiply(temp_F,dt)                                ! Pressure correction U^{n+1} = Ustar - dt ∇p
         call subtract(U,Ustar,temp_F)

         call applyAllBCs(U,m)
       end subroutine

       subroutine Euler_SOR_Donor(SOR,U,p,F,U_CC,m,Re,dt,ss_ppe,&
         Ustar,temp_F,temp_CC,temp_E1,temp_E2,printNorms)
         implicit none
         type(SORSolver),intent(inout) :: SOR
         type(VF),intent(inout) :: p
         type(VF),intent(in) :: U,F,U_CC
         type(VF),intent(inout) :: Ustar,temp_F,temp_CC,temp_E1,temp_E2
         type(mesh),intent(in) :: m
         logical,intent(in) :: printNorms
         type(solverSettings),intent(inout) :: ss_ppe
         type(norms),intent(inout) :: norm_PPE
         real(cp),intent(in) :: Re,dt
         
         call faceAdvectDonor(temp_F,U,U,temp_E1,temp_E2,U_CC,m) ! Advection terms
         call assignMinus(Ustar,temp_F)                          ! Ustar = -TempVF

         call lap(temp_F,U,m)                                    ! Diffusion terms
         call divide(temp_F,Re)
         call add(Ustar,temp_F)

         call add(Ustar,F)                                       ! Source Terms (e.g. N j x B)

         call zeroWall(Ustar,m,U)                                ! Zero forces on non-Neumann walls

         call multiply(Ustar,dt)                                 ! Ustar = U + dt*Ustar
         call add(Ustar,U)

         call div(temp_CC,Ustar,m)                               ! PPE: ∇²p = div(u)/dt
         call divide(temp_CC,dt)                                 ! O(dt) pressure treatment
         call zeroGhostPoints(temp_CC)
         call solve(SOR,p,temp_CC,m,ss_ppe,norm_PPE,printNorms)

         call grad(temp_F,p,m)                                   ! Pressure gradient, temp_F = ∇p

         call multiply(temp_F,dt)                                ! Pressure correction U^{n+1} = Ustar - dt ∇p
         call subtract(U,Ustar,temp_F)

         call applyAllBCs(U,m)
       end subroutine

       end module