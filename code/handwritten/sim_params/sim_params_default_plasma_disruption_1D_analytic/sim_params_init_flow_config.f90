     module sim_params_init_flow_config_mod
     use current_precision_mod
     use constants_mod
     use string_mod
     use dir_tree_mod
     use var_extend_mod
     use var_set_extend_mod
     use path_extend_mod
     use equation_term_extend_mod
     use export_frequency_params_extend_mod
     use export_frequency_extend_mod
     use time_marching_params_extend_mod
     use sim_params_mod
     use sim_params_aux_mod
     implicit none

     private
     public :: sim_params_init_flow_config

     contains

     subroutine sim_params_init_flow_config(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       logical,parameter :: T = .true.
       logical,parameter :: F = .false.

       SP%GP%periodic_dir            = (/1,1,0/)
       SP%GP%apply_BC_order          = (/5,6,1,2,3,4/) ! good for periodic in y?

       ! call init_IC_BC(var      ,IC   ,BC)
       call init_IC_BC(SP%VS%T    ,0    ,0 )
       call init_IC_BC(SP%VS%U    ,0    ,0 )
       call init_IC_BC(SP%VS%P    ,0    ,0 )
       call init_IC_BC(SP%VS%B    ,0    ,1 )
       call init_IC_BC(SP%VS%B0   ,1    ,0 )
       call init_IC_BC(SP%VS%phi  ,0    ,0 )
       call init_IC_BC(SP%VS%rho  ,0    ,0 )

       ! The following is needed only if curl-curl(B) is used, opposed to J in solver.
       ! if (SP%SCP%finite_Rem) SP%VS%B%MFP%coeff_explicit = SP%VS%B%MFP%coeff_explicit/SP%DP%Rem

       ! IF A TERM IS REPEATED, THE ONLY TERM THAT COUNTS IS THE LAST ONE DEFINED TO BE "TRUE"
       ! Sources to add to momentum equation. NOTE: scale is not set if add=false
       call init(SP%MT%pressure_grad       ,F,-1.0_cp                   )
       call init(SP%MT%diffusion           ,T,SP%VS%U%MFP%coeff_explicit)
       call init(SP%MT%diffusion_linear    ,F,SP%VS%U%MFP%coeff_explicit)
       call init(SP%MT%advection_convection,F,-1.0_cp/SP%DP%Rem         )
       call init(SP%MT%advection_convection,T,-1.0_cp                   )
       call init(SP%MT%advection_divergence,F,-1.0_cp                   )
       call init(SP%MT%advection_divergence,F,-1.0_cp/SP%DP%Rem         ) ! For Rem ne 1 in Bandaru
       call init(SP%MT%advection_base_flow ,F,-1.0_cp                   )
       call init(SP%MT%mean_pressure_grad  ,F,1.0_cp                    )
       call init(SP%MT%JCrossB             ,F,SP%DP%N*SP%DP%Rem         ) ! For Rem ne 1 in Bandaru (look at J definition in Bandaru)
       call init(SP%MT%JCrossB             ,T,SP%DP%N                   )
       call init(SP%MT%Q2D_JCrossB         ,F,-1.0_cp/SP%DP%tau         )
       call init(SP%MT%Buoyancy            ,F,SP%DP%Gr/SP%DP%Re**2.0_cp )
       call init(SP%MT%Gravity             ,F,1.0_cp/SP%DP%Fr**2.0_cp   )

       ! Terms computed in induction module... NOTE: scale is not set if add=false
       call init(SP%IT%B_applied       ,T, 1.0_cp           ) ! B0 = scale*B0
       call init(SP%IT%current         ,T, 1.0_cp/SP%DP%Rem ) ! J = scale curl(B)
       call init(SP%IT%advection       ,F, 1.0_cp/SP%DP%Rem ) ! For Rem ne 1 in Bandaru
       call init(SP%IT%advection       ,T, 1.0_cp           )
       call init(SP%IT%diffusion       ,T, -SP%VS%B%MFP%beta) ! since LHS and J includes scale
       call init(SP%IT%diffusion_linear,F, -SP%VS%B%MFP%beta) ! since LHS and J includes scale
       call init(SP%IT%unsteady_B0     ,F, -1.0_cp          ) ! since RHS
       call init(SP%IT%constant_dB0dt  ,T, +1.0_cp          ) ! since RHS, positive since dB0dt is neg on LHS

       ! Sources to add to energy equation. NOTE: scale is not set if add=false
       call init(SP%ET%advection          , F,-1.0_cp           )
       call init(SP%ET%diffusion          , F,1.0_cp/SP%DP%Pe   )
       call init(SP%ET%KE_diffusion       , F,-SP%DP%Ec/SP%DP%Re)
       call init(SP%ET%viscous_dissipation, F, SP%DP%Ec/SP%DP%Re)
       call init(SP%ET%joule_heating      , F,SP%DP%Ec*SP%DP%N  )
       call init(SP%ET%volumetric_heating , F,1.0_cp            )
     end subroutine

     end module