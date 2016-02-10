       module momentum_solver_mod
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
       use apply_stitches_mod
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

       public :: CN_AB2_PPE_PCG_mom_PCG
       public :: CN_AB2_PPE_GS_mom_PCG

       public :: Euler_PCG_Donor
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

       subroutine CN_AB2_PPE_PCG_mom_PCG(mom_PCG,PPE_PCG,U,Unm1,U_E,p,F,Fnm1,m,&
         Re,dt,Nmax_PPE,Nmax_mom,Ustar,temp_F,temp_CC,temp_E,compute_norms)
         implicit none
         type(PCG_solver_VF),intent(inout) :: mom_PCG
         type(PCG_solver_SF),intent(inout) :: PPE_PCG
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,Unm1
         type(TF),intent(inout) :: U_E
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: Nmax_PPE,Nmax_mom
         type(VF),intent(inout) :: Ustar,temp_F,temp_E
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_U(Ustar,U,U_E,m,.false.,temp_E,temp_CC)
         call advect_U(temp_F,Unm1,U_E,m,.true.,temp_E,temp_CC)
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
         call solve(PPE_PCG,p,temp_CC,m,Nmax_PPE,compute_norms)
         call grad(temp_F,p,m)
         call multiply(temp_F,dt)
         call subtract(U,temp_F) ! U = Ustar - grad(p)
         call apply_BCs(U,m)
       end subroutine

       subroutine CN_AB2_PPE_GS_mom_PCG(mom_PCG,PPE_GS,U,Unm1,U_E,p,F,Fnm1,m,&
         Re,dt,Nmax_PPE,Nmax_mom,Ustar,temp_F,temp_CC,temp_E,compute_norms)
         implicit none
         type(PCG_solver_VF),intent(inout) :: mom_PCG
         type(GS_poisson),intent(inout) :: PPE_GS
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U,Unm1
         type(TF),intent(inout) :: U_E
         type(VF),intent(in) :: F,Fnm1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: Nmax_PPE,Nmax_mom
         type(VF),intent(inout) :: Ustar,temp_F,temp_E
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_U(Ustar,U,U_E,m,.false.,temp_E,temp_CC)
         call advect_U(temp_F,Unm1,U_E,m,.true.,temp_E,temp_CC)
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

         call solve(mom_PCG,U,Ustar,m,Nmax_mom,compute_norms)

         call div(temp_CC,U,m)
         call multiply(temp_CC,1.0_cp/dt)
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

       subroutine Euler_PCG_Donor(PCG,U,U_E,p,F,m,Re,dt,n,nstep,energy_budget,&
         Ustar,temp_F1,temp_F2,temp_CC,temp_E,compute_norms)
         implicit none
         type(PCG_solver_SF),intent(inout) :: PCG
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U
         type(TF),intent(inout) :: U_E
         type(VF),intent(in) :: F
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: n,nstep
         type(VF),intent(inout) :: Ustar,temp_F1,temp_F2,temp_E
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         real(cp),dimension(5),intent(inout) :: energy_budget ! dudt,adv,pres,diff,external
         integer :: i_stop
         character(len=1) :: n_domains
         n_domains = '2'
         i_stop = 10**8
         if (nstep.eq.i_stop) call export_processed(m,U,'out/LDC/','U_initial_'//n_domains,1)
         call advect_U(temp_F1,U,U_E,m,.false.,temp_E,temp_CC)
         if (nstep.eq.i_stop) call export_processed(m,temp_F1,'out/LDC/','advect_'//n_domains,1)
              call compute_energy(energy_budget(2),U,temp_F1,m,temp_F2,temp_CC,compute_norms)
         call multiply(Ustar,temp_F1,-1.0_cp) ! Because advect_div gives positive
         call lap(temp_F1,U,m)
         if (nstep.eq.i_stop) call export_processed(m,temp_F1,'out/LDC/','lap_'//n_domains,1)
         call multiply(temp_F1,1.0_cp/Re)
              call compute_energy(energy_budget(4),U,temp_F1,m,temp_F2,temp_CC,compute_norms)
         call add(Ustar,temp_F1)
         call add(Ustar,F)
              call compute_energy(energy_budget(5),U,F,m,temp_F2,temp_CC,compute_norms)
         call zeroWall_conditional(Ustar,m,U)
         call multiply(Ustar,dt)
         call add(Ustar,U)
         if (nstep.eq.i_stop) call export_processed(m,Ustar,'out/LDC/','Ustar_'//n_domains,1)
              if (compute_norms) call assign(temp_F1,U)
         call assign(U,Ustar)
              if (compute_norms) call assign(Ustar,temp_F1)
         call div(temp_CC,U,m)
         call multiply(temp_CC,1.0_cp/dt)
         call zeroGhostPoints(temp_CC)
         ! call apply_stitches(temp_CC,m)
         if (nstep.eq.i_stop) call export_processed(m,temp_CC,'out/LDC/','PPE_source_'//n_domains,1)
         call solve(PCG,p,temp_CC,m,n,compute_norms)
         if (nstep.eq.i_stop) call export_processed(m,p,'out/LDC/','p_solution_'//n_domains,1)
         call grad(temp_F1,p,m)
              call compute_energy(energy_budget(3),Ustar,temp_F1,m,temp_F2,temp_CC,compute_norms)
         call multiply(temp_F1,dt)
         call subtract(U,temp_F1)
              if (compute_norms) then
                call subtract(temp_F1,U,Ustar)
                call multiply(temp_F1,1.0_cp/dt)
              endif
              call compute_energy(energy_budget(1),U,temp_F1,m,temp_F2,temp_CC,compute_norms)
         call apply_BCs(U,m)
         if (nstep.eq.i_stop) call div(temp_CC,U,m)
         if (nstep.eq.i_stop) call export_raw(m,temp_CC,'out/LDC/','divU_'//n_domains,0)
         if (nstep.eq.i_stop) call export_processed(m,U,'out/LDC/','Unp1_'//n_domains,1)
         if (nstep.eq.i_stop) stop 'Done'
       end subroutine

       subroutine Euler_GS_Donor(GS,U,U_E,p,F,m,Re,dt,n,&
         Ustar,temp_F,temp_CC,temp_E,compute_norms)
         implicit none
         type(GS_poisson),intent(inout) :: GS
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U
         type(TF),intent(inout) :: U_E
         type(VF),intent(in) :: F
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: n
         type(VF),intent(inout) :: Ustar,temp_F,temp_E
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_U(temp_F,U,U_E,m,.false.,temp_E,temp_CC)
         call multiply(Ustar,temp_F,-1.0_cp) ! Because advect_div gives positive
         call lap(temp_F,U,m)
         call multiply(temp_F,1.0_cp/Re)
         call add(Ustar,temp_F)
         call add(Ustar,F)
         call zeroWall_conditional(Ustar,m,U)
         call multiply(Ustar,dt)
         call add(Ustar,U)
         call div(temp_CC,Ustar,m)
         call multiply(temp_CC,1.0_cp/dt)
         call zeroGhostPoints(temp_CC)
         call solve(GS,p,temp_CC,m,n,compute_norms)
         call grad(temp_F,p,m)
         call multiply(temp_F,dt)
         call subtract(U,Ustar,temp_F)
         call apply_BCs(U,m)
         ! call apply_stitches(U,m)
       end subroutine

       subroutine Euler_GS_Donor_mpg(GS,U,U_E,p,F,mpg,m,Re,dt,n,&
         Ustar,temp_F,temp_CC,temp_E,compute_norms)
         implicit none
         type(GS_poisson),intent(inout) :: GS
         type(SF),intent(inout) :: p
         type(VF),intent(inout) :: U
         type(TF),intent(inout) :: U_E
         type(VF),intent(in) :: F,mpg
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re,dt
         integer,intent(in) :: n
         type(VF),intent(inout) :: Ustar,temp_F,temp_E
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         call advect_U(temp_F,U,U_E,m,.false.,temp_E,temp_CC)
         call multiply(Ustar,temp_F,-1.0_cp) ! Because advect_div gives positive
         call lap(temp_F,U,m)
         call multiply(temp_F,1.0_cp/Re)
         call add(Ustar,temp_F)
         call add(Ustar,F)
         call zeroWall_conditional(Ustar,m,U)
         call multiply(Ustar,dt)
         call add(Ustar,U)
         call div(temp_CC,Ustar,m)
         call multiply(temp_CC,1.0_cp/dt)
         call zeroGhostPoints(temp_CC)
         call solve(GS,p,temp_CC,m,n,compute_norms)
         call grad(temp_F,p,m)
         call subtract(temp_F,mpg)
         call multiply(temp_F,dt)
         call subtract(U,Ustar,temp_F)
         call apply_BCs(U,m)
       end subroutine

       end module