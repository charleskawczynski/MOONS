       module induction_sources_mod
       use current_precision_mod
       use sim_params_mod
       use mesh_mod
       use VF_mod
       use TF_mod
       use mesh_domain_mod
       use IO_tools_mod
       use induction_aux_mod
       use ops_advect_mod
       use ops_discrete_mod
       use assign_B0_vs_t_mod
       use time_marching_params_mod
       implicit none

       private
       public :: add_curl_U_cross_B
       public :: add_curl_J
       public :: add_unsteady_B0

       contains

       subroutine add_curl_U_cross_B(F,m,U_E_mom,U_E,B0,B,curlUCrossB,MD_fluid,scale,finite_Rem,temp_F,temp_E_TF,temp_E)
         implicit none
         type(VF),intent(inout) :: F,temp_F,curlUCrossB,temp_E
         type(VF),intent(inout) :: B,B0
         type(TF),intent(in) :: U_E_mom
         type(TF),intent(inout) :: U_E,temp_E_TF
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: scale
         type(mesh_domain),intent(in) :: MD_fluid
         logical,intent(in) :: finite_Rem
         call embedVelocity_E(U_E,U_E_mom,MD_fluid)
         if (finite_Rem) then
               call add(temp_F,B,B0) ! Since finite Rem
               call advect_B(curlUCrossB,U_E,temp_F,m,temp_E_TF,temp_E)
         else; call advect_B(curlUCrossB,U_E,B0    ,m,temp_E_TF,temp_E)
         endif
         call add_product(F,curlUCrossB,scale)
       end subroutine

       subroutine add_curl_J(F,m,J,sigmaInv_edge,scale,temp_F2,temp_E)
         implicit none
         type(VF),intent(inout) :: F,temp_F2,temp_E
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: J,sigmaInv_edge
         real(cp),intent(in) :: scale
         call multiply(temp_E,J,sigmaInv_edge)
         call curl(temp_F2,temp_E,m)
         call add_product(F,temp_F2,scale)
       end subroutine

       subroutine add_unsteady_B0(F,B0,dB0dt,scale,TMP)
         implicit none
         type(VF),intent(inout) :: F,B0,dB0dt
         type(time_marching_params),intent(in) :: TMP
         real(cp),intent(in) :: scale
         call assign_B0_vs_t(B0,TMP)
         call assign_dB0_dt_vs_t(dB0dt,TMP)
         call add_product(F,dB0dt,scale) ! (-) since added to RHS
       end subroutine

       end module