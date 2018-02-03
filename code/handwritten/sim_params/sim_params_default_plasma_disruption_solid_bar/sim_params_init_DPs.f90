     module sim_params_init_DPs_mod
     use current_precision_mod
     use constants_mod
     use dimensionless_params_extend_mod
     use sim_params_mod
     use sim_params_aux_mod
     implicit none

     private
     public :: sim_params_init_DPs

     contains

     subroutine sim_params_init_DPs(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       call delete(SP%DP)

       SP%DP%dtime                   = 1.0_cp*pow(-4)
       SP%DP%t_start                 = 0.0_cp

       SP%DP%t_final                 = 7.0_cp
       SP%DP%t_plasma                     = 0.0047_cp
       ! SP%DP%t_plasma                     = 0.00790207929237_cp
       ! SP%DP%t_final                 = 3.0_cp*SP%DP%dtime ! for testing

       SP%DP%Re                      = 11769054.2652_cp
       ! SP%DP%N                       = 1.0_cp
       SP%DP%Ha                      = 2645.75131106_cp
       SP%DP%Rem                     = 1.1131984703
       ! SP%DP%Q                       = 8.0_cp*pow(-1)
       ! SP%DP%Ha                      = 5.0_cp*pow(2)
       ! SP%DP%Ha                      = 10.0_cp*pow(3)
       ! SP%DP%Ha                      = 15.0_cp*pow(3)
       ! SP%DP%N                       = 1.0_cp/SP%DP%Q
       SP%DP%c_w(1:6)                = 0.0_cp
       SP%DP%c_w( 5 )                = 1.0_cp
       SP%DP%c_w( 6 )                = 1.0_cp
       SP%DP%Robin_coeff             = 0.0_cp
       SP%DP%Robin_coeff(5:6)        = -1.0_cp/SP%DP%c_w(5:6)
       ! SP%DP%c_w_coeff                = (2.0_cp*SP%DP%c_w/dh_nhat-1.0_cp)/(2.0_cp*SP%DP%c_w/dh_nhat+1.0_cp)
       ! SP%DP%sig_local_over_sig_f    = 1.0_cp*pow(-3)
       SP%DP%sig_local_over_sig_f    = 1.0_cp
       SP%DP%Gr                      = 0.0_cp
       SP%DP%Pr                      = 0.01_cp
       SP%DP%Fr                      = 1.0_cp
       SP%DP%Ec                      = 0.0_cp

       ! SP%DP%Ha                      = (1.0_cp/SP%DP%Q*SP%DP%Re)**0.5_cp
       SP%DP%N                       = SP%DP%Ha**2.0_cp/SP%DP%Re
       SP%DP%Ha                      = (SP%DP%N*SP%DP%Re)**0.5_cp
       SP%DP%Al                      = SP%DP%N/SP%DP%Rem
       SP%DP%Pe                      = SP%DP%Pr*SP%DP%Re
       SP%DP%tau                     = SP%DP%Re/SP%DP%Ha
       SP%DP%L_eta                   = SP%DP%Re**(-0.75_cp)
       SP%DP%U_eta                   = SP%DP%Re**(-0.25_cp)
       SP%DP%t_eta                   = SP%DP%Re**(-0.50_cp)
       SP%DP%KE_scale                = 1.0_cp
       SP%DP%ME_scale                = SP%DP%Al
       SP%DP%JE_scale                = SP%DP%N*2.0_cp ! x2 because (J^2/sigma) not (1/2 J^2/sigma)
     end subroutine

     end module