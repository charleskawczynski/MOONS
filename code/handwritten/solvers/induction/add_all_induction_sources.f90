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
           call add_constant_dB0dt(F%x,ind%dB0dt%x,SP%IT%constant_dB0dt%scale)
         endif

         if (SP%IT%zero_source_components_any) then
           if (SP%IT%zero_source_components(1)) call assign(F%x,0.0_cp)
           if (SP%IT%zero_source_components(2)) call assign(F%y,0.0_cp)
           if (SP%IT%zero_source_components(3)) call assign(F%z,0.0_cp)
         endif

       end subroutine

       end module