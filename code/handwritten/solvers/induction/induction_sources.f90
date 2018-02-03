       module induction_sources_mod
       use current_precision_mod
       use sim_params_mod
       use mesh_extend_mod
       use SF_extend_mod
       use VF_extend_mod
       use TF_extend_mod
       use mesh_domain_extend_mod
       use ops_interp_mod
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
       public :: compute_jCrossB

       contains

       subroutine add_curl_U_cross_B(F,m,U_E,B,curlUCrossB,scale,temp_E_TF,temp_E)
         implicit none
         type(VF),intent(inout) :: F,curlUCrossB,temp_E
         type(VF),intent(in) :: B
         type(TF),intent(in) :: U_E
         type(TF),intent(inout) :: temp_E_TF
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: scale
         call advect_B(curlUCrossB,U_E,B,m,temp_E_TF,temp_E)
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

       subroutine add_unsteady_B0(F,B0,dB0dt,scale_B0,scale_dB0_dt,TMP,t_plasma)
         implicit none
         type(VF),intent(inout) :: F,B0,dB0dt
         type(time_marching_params),intent(in) :: TMP
         real(cp),intent(in) :: scale_B0,scale_dB0_dt,t_plasma
         call assign_B0_vs_t(B0,TMP,t_plasma)
         call multiply(B0,scale_B0)
         call assign_dB0_dt_vs_t(dB0dt,TMP,t_plasma)
         call add_product(F,dB0dt,scale_dB0_dt) ! (-) since added to RHS
       end subroutine

       subroutine compute_JCrossB(jCrossB,B,J,m,scale,&
         temp_CC,temp_F1_TF,temp_F2_TF)
         ! computes:  scale J x B
         implicit none
         type(VF),intent(inout) :: jCrossB
         type(VF),intent(in) :: B,J
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: scale
         type(SF),intent(inout) :: temp_CC
         type(TF),intent(inout) :: temp_F1_TF,temp_F2_TF
         call edge2Face_no_diag(temp_F1_TF,J,m)
         call face2Face_no_diag(temp_F2_TF,B,m,temp_CC)
         call cross_product(jCrossB,temp_F1_TF,temp_F2_TF)
         call multiply(jCrossB,scale) ! Since J includes Rem
       end subroutine

       end module