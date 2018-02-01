       module add_all_induction_sources_mod
       use current_precision_mod
       use sim_params_mod
       use VF_extend_mod
       use mesh_extend_mod
       use mesh_domain_extend_mod
       use TF_extend_mod
       use IO_tools_mod
       use time_marching_params_mod

       use energy_mod
       use momentum_mod
       use induction_mod
       use induction_terms_mod
       use induction_aux_mod
       use induction_sources_mod
       implicit none

       private
       public :: add_all_induction_sources

       contains

       subroutine add_all_induction_sources(F,Fnm1,L,ind,TMP,SP,mom)
         implicit none
         type(VF),intent(inout) :: F,Fnm1,L
         type(momentum),intent(in) :: mom
         type(induction),intent(inout) :: ind
         type(time_marching_params),intent(in) :: TMP
         type(sim_params),intent(in) :: SP

         call assign(Fnm1,F)
         call assign(L,0.0_cp)
         call assign(F,0.0_cp) ! DO NOT REMOVE THIS, FOLLOW THE COMPUTE_ADD PROCEDURE BELOW

         if (SP%IT%advection%add) then
           call embedVelocity_E(ind%U_E,mom%U_E,ind%MD_fluid)
           call add_curl_U_cross_B(F,ind%m,ind%U_E,ind%Btot,ind%curlUCrossB,&
            SP%IT%advection%scale,ind%temp_E_TF,ind%temp_E)
         endif
         if (SP%IT%diffusion_linear%add) then
           call add_curl_J(L,ind%m,ind%J,ind%sigmaInv_edge,SP%IT%diffusion%scale,ind%temp_F2,ind%temp_E)
         endif
         if (SP%IT%diffusion%add) then
           call add_curl_J(F,ind%m,ind%J,ind%sigmaInv_edge,SP%IT%diffusion%scale,ind%temp_F2,ind%temp_E)
         endif
         if (SP%IT%unsteady_B0%add) then
           call add_unsteady_B0(F,ind%B0,ind%dB0dt,SP%IT%B_applied%scale,SP%IT%unsteady_B0%scale,TMP)
         endif
         if (SP%IT%constant_dB0dt%add) then
           call add(F%x,SP%IT%constant_dB0dt%scale)
         endif
       end subroutine

       subroutine add_all_induction_raw(F,ind,TMP,SP,mom_U_E,ind_B)
         implicit none
         type(VF),intent(inout) :: F
         type(VF),intent(in) :: ind_B
         type(TF),intent(in) :: mom_U_E
         type(induction),intent(inout) :: ind
         type(time_marching_params),intent(in) :: TMP
         type(sim_params),intent(in) :: SP
         call add_all_induction_raw_func(F,mom_U_E,ind%U_E,ind%temp_E_TF,ind%J,&
         ind%B0,ind_B,ind%curlUCrossB,ind%temp_F2,ind%temp_E,ind%sigmaInv_edge,&
         ind%dB0dt,ind%m,TMP,ind%MD_fluid,SP%IT)
       end subroutine

       subroutine add_all_induction_raw_func(F,mom_U_E,ind_U_E,ind_temp_E_TF,&
         ind_J,ind_B0,ind_Btot,ind_curlUCrossB,ind_temp_F2,ind_temp_E,&
         ind_sigmaInv_edge,ind_dB0dt,ind_m,TMP,ind_MD_fluid,IT)
         implicit none
         type(VF),intent(inout) :: F
         type(TF),intent(inout) :: ind_U_E,ind_temp_E_TF
         type(TF),intent(in) :: mom_U_E
         type(VF),intent(inout) :: ind_curlUCrossB,ind_temp_E,ind_temp_F2,ind_B0,ind_dB0dt
         type(VF),intent(in) :: ind_J,ind_Btot,ind_sigmaInv_edge
         type(mesh),intent(in) :: ind_m
         type(time_marching_params),intent(in) :: TMP
         type(mesh_domain),intent(in) :: ind_MD_fluid
         type(induction_terms),intent(in) :: IT

         call assign(F,0.0_cp) ! DO NOT REMOVE THIS, FOLLOW THE COMPUTE_ADD PROCEDURE BELOW

         if (IT%advection%add) then
           call embedVelocity_E(ind_U_E,mom_U_E,ind_MD_fluid)
           call add_curl_U_cross_B(F,ind_m,ind_U_E,ind_Btot,&
           ind_curlUCrossB,IT%advection%scale,ind_temp_E_TF,ind_temp_E)
         endif
         if (IT%diffusion%add) then
           call add_curl_J(F,ind_m,ind_J,ind_sigmaInv_edge,IT%diffusion%scale,ind_temp_F2,ind_temp_E)
         endif
         if (IT%unsteady_B0%add) then
           call add_unsteady_B0(F,ind_B0,ind_dB0dt,IT%B_applied%scale,IT%unsteady_B0%scale,TMP)
         endif
         if (IT%constant_dB0dt%add) then
           call add(F%x,IT%constant_dB0dt%scale)
         endif
       end subroutine

       end module