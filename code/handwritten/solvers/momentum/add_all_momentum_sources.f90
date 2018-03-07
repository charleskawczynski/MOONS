       module add_all_momentum_sources_mod
       use current_precision_mod
       use sim_params_mod
       use VF_extend_mod
       use TF_extend_mod
       use IO_tools_mod
       use time_marching_params_mod

       use energy_mod
       use momentum_mod
       use momentum_terms_mod
       use induction_mod
       use induction_sources_mod
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
           SP%MT%mean_pressure_grad%scale,SP%SCP%mpg_dir)
         endif

         call assign(Fnm1,F)
         call assign(L,0.0_cp) ! DO NOT REMOVE THIS, FOLLOW THE COMPUTE_ADD PROCEDURE BELOW
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
           call compute_add_diffusion(F,mom%m,mom%U,SP%MT%diffusion%scale,mom%temp_F1)
         endif
         if (SP%MT%diffusion_linear%add) then
           call compute_add_diffusion(L,mom%m,mom%U,SP%MT%diffusion%scale,mom%temp_F1)
         endif
         if (SP%MT%advection_base_flow%add) then ! For linear stability analysis
           call compute_add_advection_base_flow(F,mom%m,mom%TS%U_ave,mom%U,mom%U_E,&
           SP%MT%advection_base_flow%scale,mom%temp_F1,mom%temp_F2,&
           mom%temp_F3,mom%temp_CC)
         endif
         if (SP%MT%JCrossB%add) then ! scale is included in ind%jCrossB
           call extract_add_JCrossB(F,mom%temp_F1,ind%jCrossB,ind%MD_fluid,1.0_cp)
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

         if (SP%MT%zero_source_components_any) then
           if (SP%MT%zero_source_components(1)) call assign(F%x,0.0_cp)
           if (SP%MT%zero_source_components(2)) call assign(F%y,0.0_cp)
           if (SP%MT%zero_source_components(3)) call assign(F%z,0.0_cp)
         endif

       end subroutine

!        subroutine add_all_momentum_raw_func(F,mom_temp_F3,ind_temp_F1_TF,ind_J,ind_m,&
!          mom_temp_CC,mom_m,ind_MD_fluid,ind_temp_CC,ind_Btot,nrg_MD,mom_temp_F2,&
!          mom_p,nrg_T,mom_U,ind_temp_F2_TF,ind_temp_F1,L,mom_temp_E,nrg_temp_F,&
!          mom_U_E,mom_temp_F1,nrg_gravity,nrg_m,TMP,nrg_temp_CC1_VF,MT,SP)
!          implicit none
!          type(VF),intent(inout) :: F
!          type(VF),intent(inout) :: mom_U
!          type(TF),intent(inout) :: mom_U_E
!          type(mesh),intent(in) :: mom_m
!          type(time_marching_params),intent(in) :: TMP
!          type(sim_params),intent(in) :: SP
!          type(momentum_terms),intent(in) :: MT

!          if (MT%mean_pressure_grad%add) then
!            call compute_add_MPG(mom_U,TMP,&
!            MT%mean_pressure_grad%scale,SP%SCP%mpg_dir)
!          endif

!          call assign(F,0.0_cp) ! DO NOT REMOVE THIS, FOLLOW THE COMPUTE_ADD PROCEDURE BELOW

!          if (MT%pressure_grad%add) then
!            call compute_add_pressure_grad(F,mom_m,mom_p,&
!            MT%pressure_grad%scale,mom_temp_F1)
!          endif
!          if (MT%advection_divergence%add) then
!            call compute_add_advection_divergence(F,mom_m,mom_U,mom_U_E,&
!            MT%advection_divergence%scale,mom_temp_F1,mom_temp_E,mom_temp_CC)
!          endif
!          if (MT%advection_convection%add) then
!            call compute_add_advection_convection(F,mom_m,mom_U,mom_U_E,&
!            MT%advection_convection%scale,mom_temp_F1,mom_temp_F2,&
!            mom_temp_F3,mom_temp_CC)
!          endif
!          if (MT%diffusion%add) then
!            call compute_add_diffusion(F,mom_m,mom_U,MT%diffusion%scale,mom_temp_F1)
!          endif
!          if (MT%diffusion_linear%add) then
!            call compute_add_diffusion(L,mom_m,mom_U,MT%diffusion%scale,mom_temp_F1)
!          endif
!          ! if (MT%advection_base_flow%add) then ! For linear stability analysis
!          !   call compute_add_advection_base_flow(F,mom_m,mom%TS%U_ave,mom_U,mom_U_E,&
!          !   MT%advection_base_flow%scale,mom_temp_F1,mom_temp_F2,&
!          !   mom_temp_F3,mom_temp_CC)
!          ! endif
!          if (MT%JCrossB%add) then
!            call compute_add_JCrossB(F,mom_temp_F1,ind_Btot,ind_J,ind_m,&
!                                    ind_MD_fluid,MT%JCrossB%scale,&
!                                    ind_temp_CC,ind_temp_F1,ind_temp_F1_TF,&
!                                    ind_temp_F2_TF)
!          endif
!          if (MT%Q2D_JCrossB%add) then
!            call compute_add_Q2D_JCrossB(F,mom_temp_F1,mom_U,MT%Q2D_JCrossB%scale)
!          endif
!          if (MT%Buoyancy%add) then
!            call compute_add_buoyancy(F,mom_temp_F1,nrg_T,nrg_gravity,&
!                                      MT%Buoyancy%scale,nrg_m,nrg_MD,&
!                                      nrg_temp_F,nrg_temp_CC1_VF)
!          endif
!          if (MT%Gravity%add) then
!            call compute_add_gravity(F,mom_temp_F1,nrg_gravity,MT%Gravity%scale,nrg_m,&
!                                     nrg_MD,nrg_temp_F,nrg_temp_CC1_VF)
!          endif

!        end subroutine

       end module