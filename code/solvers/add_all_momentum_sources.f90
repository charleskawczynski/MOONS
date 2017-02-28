       module add_all_momentum_sources_mod
       use current_precision_mod
       use sim_params_mod
       use VF_mod
       use IO_tools_mod

       use energy_mod
       use momentum_mod
       use induction_mod
       use momentum_sources_mod
       implicit none

       private
       public :: add_all_momentum_sources

       contains

       subroutine add_all_momentum_sources(F,Fnm1,nrg,mom,ind,SP)
         implicit none
         type(VF),intent(inout) :: F,Fnm1
         type(energy),intent(inout) :: nrg
         type(momentum),intent(inout) :: mom
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         real(cp) :: scale

         if (SP%MF%mean_pressure_grad) then
           scale = 1.0_cp
           call compute_Add_MPG(mom%U,mom%SP%VS%U%TMP,scale,SP%mpg_dir)
         endif

         call assign(Fnm1,F)
         call assign(F,0.0_cp) ! DO NOT REMOVE THIS, FOLLOW THE COMPUTE_ADD PROCEDURE BELOW

         if (SP%MF%advection_divergence) then
           scale = -1.0_cp
           scale = -1.0_cp/SP%DP%Rem ! For Rem ne 1 in Bandaru
           call compute_add_advection_divergence(F,mom%m,mom%U,mom%U_E,scale,mom%temp_F1,mom%temp_E,mom%temp_CC)
         endif
         if (SP%MF%advection_convection) then
           scale = -1.0_cp
           scale = -1.0_cp/SP%DP%Rem ! For Rem ne 1 in Bandaru
           call compute_add_advection_convection(F,mom%m,mom%U,mom%U_E,scale,&
                mom%temp_F1,mom%temp_F2,mom%temp_F3,mom%temp_CC)
         endif
         if (SP%MF%diffusion) then ! Requires special treatment since potentially implicit
           scale = mom%SP%VS%U%MFP%coeff_explicit
           call compute_add_diffusion(F,mom%m,mom%U,scale,mom%temp_F1)
         endif
         if (SP%MF%JCrossB) then
           scale = mom%SP%DP%N
           scale = mom%SP%DP%N*mom%SP%DP%Rem ! For Rem ne 1 in Bandaru (look at J definition)
           call compute_add_JCrossB(F,mom%temp_F1,ind%B,ind%B0,ind%J,ind%m,&
                                   ind%MD_fluid,scale,&
                                   ind%SP%finite_Rem,ind%temp_CC,&
                                   ind%temp_F1,ind%temp_F1_TF,&
                                   ind%temp_F2_TF)
         endif
         if (SP%MF%Q2D_JCrossB) then
           scale = -1.0_cp/mom%SP%DP%tau
           call compute_add_Q2D_JCrossB(F,mom%temp_F1,mom%U,mom%SP%DP%tau)
         endif
         if (SP%MF%Buoyancy) then
           scale = mom%SP%DP%Gr/mom%SP%DP%Re**2.0_cp
           call compute_add_buoyancy(F,mom%temp_F1,nrg%T,nrg%gravity,&
                                     scale,nrg%m,nrg%MD,&
                                     nrg%temp_F,nrg%temp_CC1_VF)
         endif
         if (SP%MF%Gravity) then
           scale = 1.0_cp/mom%SP%DP%Fr**2.0_cp
           call compute_add_gravity(F,mom%temp_F1,nrg%gravity,scale,nrg%m,&
                                    nrg%MD,nrg%temp_F,nrg%temp_CC1_VF)
         endif

       end subroutine

       end module