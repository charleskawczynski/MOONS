       module temporal_convergence_test_mod
       use current_precision_mod
       use constants_mod
       use mesh_extend_mod
       use GF_mod
       use SF_extend_mod
       use VF_extend_mod
       use TF_extend_mod
       use dir_tree_mod
       use string_mod
       use path_extend_mod
       use export_raw_processed_mod
       use ops_discrete_mod
       use ops_norms_mod
       use PCG_mod
       use apply_BCs_mod
       use matrix_free_operators_mod
       use matrix_free_params_mod
       use preconditioners_mod
       use iter_solver_params_mod
       use ops_mirror_field_mod
       use sim_params_mod
       use BC_funcs_mod
       use update_intermediate_field_BCs_mod

       implicit none
       private

       public :: temporal_convergence_test

       contains

       subroutine temporal_convergence_test(U,P,m,DT,SP)
         implicit none
         type(VF),intent(in) :: U
         type(SF),intent(in) :: P
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(sim_params),intent(in) :: SP
         real(cp) :: e,dtime
         dtime = SP%VS%U%TMP%dt
         write(*,*) ' *********************************************************** '
         write(*,*) ' ************* BEGIN TEMPORAL CONVERGENCE TEST ************* '
         write(*,*) ' *********************************************************** '
         write(*,*) 't = ',SP%VS%U%TMP%t
         call Ln(e,U,2.0_cp,m); write(*,*) 'dt,e(U) = ',dtime,e
         call Ln(e,P,2.0_cp,m); write(*,*) 'dt,e(P) = ',dtime,e
         write(*,*) ' *********************************************************** '
         write(*,*) ' ************** END TEMPORAL CONVERGENCE TEST ************** '
         write(*,*) ' *********************************************************** '
       end subroutine

       end module