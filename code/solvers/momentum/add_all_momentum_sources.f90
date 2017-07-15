       module add_all_momentum_sources_mod
       use current_precision_mod
       use sim_params_mod
       use VF_mod
       use IO_tools_mod
       use time_marching_params_mod

       use energy_mod
       use momentum_mod
       use induction_mod
       use momentum_sources_mod
       implicit none

       private
       public :: add_all_momentum_sources

       contains

       subroutine add_all_momentum_sources(F,Fnm1,L,mom,TMP,SP,ind,nrg)
         implicit none
         type(VF),intent(inout) :: F,Fnm1,L
         type(energy),intent(inout) :: nrg
         type(momentum),intent(inout) :: mom
         type(induction),intent(inout) :: ind
         type(time_marching_params),intent(in) :: TMP
         type(sim_params),intent(in) :: SP

         if (SP%MT%mean_pressure_grad%add) then
           call compute_add_MPG(mom%U,TMP,&
           SP%MT%mean_pressure_grad%scale,SP%mpg_dir)
         endif

         call assign(Fnm1,F)
         if (TMP%RKP%RK_active) call assign(L,0.0_cp)
         call assign(F,0.0_cp) ! DO NOT REMOVE THIS, FOLLOW THE COMPUTE_ADD PROCEDURE BELOW

         if (SP%MT%pressure_grad%add) then
           call compute_add_pressure_grad(F,mom%m,mom%p,&
           SP%MT%pressure_grad%scale,mom%temp_F1)
         endif
         if (SP%MT%advection_divergence%add) then
           call compute_add_advection_divergence(F,mom%m,mom%U,mom%U_E,&
           SP%MT%advection_divergence%scale,mom%temp_F1,mom%temp_E,mom%temp_CC)
         endif
         if (SP%MT%advection_convection%add) then
           call compute_add_advection_convection(F,mom%m,mom%U,mom%U_E,&
           SP%MT%advection_convection%scale,mom%temp_F1,mom%temp_F2,&
           mom%temp_F3,mom%temp_CC)
         endif
         if (SP%MT%diffusion%add) then
           if (TMP%RKP%RK_active) then
             call compute_add_diffusion(L,mom%m,mom%U,&
             SP%MT%diffusion%scale,mom%temp_F1)
           else
             call compute_add_diffusion(F,mom%m,mom%U,&
             SP%MT%diffusion%scale,mom%temp_F1)
           endif
         endif
         if (SP%MT%advection_base_flow%add) then ! For linear stability analysis
           call compute_add_advection_base_flow(F,mom%m,mom%TS%U_ave,mom%U,mom%U_E,&
           SP%MT%advection_base_flow%scale,mom%temp_F1,mom%temp_F2,&
           mom%temp_F3,mom%temp_CC)
         endif
         if (SP%MT%JCrossB%add) then
           call compute_add_JCrossB(F,mom%temp_F1,ind%B,ind%B0,ind%J,ind%m,&
                                   ind%MD_fluid,SP%MT%JCrossB%scale,&
                                   SP%finite_Rem,ind%temp_CC,&
                                   ind%temp_F1,ind%temp_F1_TF,&
                                   ind%temp_F2_TF)
         endif
         if (SP%MT%Q2D_JCrossB%add) then
           call compute_add_Q2D_JCrossB(F,mom%temp_F1,mom%U,SP%MT%Q2D_JCrossB%scale)
         endif
         if (SP%MT%Buoyancy%add) then
           call compute_add_buoyancy(F,mom%temp_F1,nrg%T,nrg%gravity,&
                                     SP%MT%Buoyancy%scale,nrg%m,nrg%MD,&
                                     nrg%temp_F,nrg%temp_CC1_VF)
         endif
         if (SP%MT%Gravity%add) then
           call compute_add_gravity(F,mom%temp_F1,nrg%gravity,SP%MT%Gravity%scale,nrg%m,&
                                    nrg%MD,nrg%temp_F,nrg%temp_CC1_VF)
         endif

       end subroutine

       end module