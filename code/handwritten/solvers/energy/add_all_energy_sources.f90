       module add_all_energy_sources_mod
       use current_precision_mod
       use sim_params_mod
       use SF_extend_mod
       use IO_tools_mod
       use time_marching_params_mod
       use ops_embedExtract_mod

       use energy_mod
       use momentum_mod
       use induction_mod
       use energy_aux_mod
       use energy_sources_mod
       implicit none

       private
       public :: add_all_energy_sources

       contains

       subroutine add_all_energy_sources(F,Fnm1,L,nrg,TMP,SP,ind,mom)
         implicit none
         type(SF),intent(inout) :: F,Fnm1,L
         type(energy),intent(inout) :: nrg
         type(momentum),intent(in) :: mom
         type(induction),intent(inout) :: ind
         type(time_marching_params),intent(in) :: TMP
         type(sim_params),intent(in) :: SP

         if (SP%ET%KE_diffusion%add.or.SP%ET%viscous_dissipation%add) then
           call embedCC(nrg%U_CC,mom%U_CC,nrg%MD)
         endif

         call assign(Fnm1,F)
         if (TMP%RKP%RK_active) call assign(L,0.0_cp)
         call assign(F,0.0_cp) ! DO NOT REMOVE THIS, FOLLOW THE COMPUTE_ADD PROCEDURE BELOW

         if (SP%ET%advection%add) then
           call embedFace(nrg%U_F,mom%U,nrg%MD)
           call add_advection(F,nrg%T,nrg%U_F,SP%ET%advection%scale,&
           nrg%m,nrg%temp_CC1,nrg%temp_F)
         endif
         if (SP%ET%diffusion%add) then
           if (TMP%RKP%RK_active) then
             call add_diffusion(L,nrg%T,SP%ET%diffusion%scale,nrg%m,nrg%temp_CC1)
           else
             call add_diffusion(F,nrg%T,SP%ET%diffusion%scale,nrg%m,nrg%temp_CC1)
           endif
         endif

         if (SP%ET%KE_diffusion%add) then
           call add_KE_diffusion(F,nrg%U_CC,SP%ET%KE_diffusion%scale,&
           nrg%m,nrg%temp_CC1,nrg%temp_CC2,nrg%temp_CC1_VF)
         endif
         if (SP%ET%viscous_dissipation%add) then
           call add_viscous_dissipation(F,nrg%U_CC,SP%ET%viscous_dissipation%scale,&
           nrg%m,nrg%temp_CC1,nrg%temp_CC_TF)
         endif
         if (SP%ET%joule_heating%add) then
           call add_joule_heating(F,ind%J,ind%sigmaInv_CC,SP%ET%joule_heating%scale,&
           nrg%m,nrg%temp_CC1,nrg%temp_F,nrg%temp_CC1_VF)
         endif
         if (SP%ET%volumetric_heating%add) then
           call add_volumetric_heating_Nuclear(F,nrg%m,&
           SP%ET%volumetric_heating%scale,nrg%temp_CC1)
         endif

       end subroutine

       end module