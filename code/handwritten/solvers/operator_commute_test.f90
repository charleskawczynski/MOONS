       module operator_commute_test_mod
       use current_precision_mod
       use constants_mod
       use mesh_extend_mod
       use GF_mod
       use SF_extend_mod
       use VF_mod
       use TF_mod
       use dir_tree_mod
       use string_mod
       use path_extend_mod
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

       public :: operator_commute_test

       contains

       subroutine operator_commute_test(U,p,m,DT)
         ! This performs the test to ensure that
         ! G(L(X)) = L(G(X))
         ! where G,L are the gradient and linear (laplacian/curl-curl) operators
         implicit none
         type(VF),intent(in) :: U
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         write(*,*) ' ************************************************************* '
         write(*,*) ' *********** BEGIN OPERATOR INTERCHANGABILITY TEST *********** '
         write(*,*) ' ************************************************************* '
         call grad_laplacian(U,p,m,DT)
         call curl_grad_equal_zero(U,p,m,DT) ! Just need to make sure zero by identity
         write(*,*) ' ************************************************************ '
         write(*,*) ' ************ END OPERATOR INTERCHANGABILITY TEST *********** '
         write(*,*) ' ************************************************************ '
       end subroutine

       subroutine grad_laplacian(U,p,m,DT)
         implicit none
         type(VF),intent(in) :: U
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(SF) :: phi,temp_SF
         type(VF) :: temp_VF,temp_LG,temp_GL
         type(TF) :: temp_TF
         real(cp) :: e
         call init(phi,P);             call assign(phi,0.0_cp)
         call init(temp_SF,P);         call assign(temp_SF,0.0_cp)
         call init(temp_VF,U);         call assign(temp_VF,0.0_cp)
         call init(temp_LG,U);         call assign(temp_LG,0.0_cp)
         call init(temp_GL,U);         call assign(temp_GL,0.0_cp)
         call init_CC_Edge(temp_TF,m); call assign(temp_TF,0.0_cp)
         call random_noise(phi)
         call assign_ghost_XPeriodic(phi,0.0_cp)

         call lap_centered(temp_SF,phi,m,temp_VF)
         call grad(temp_LG,temp_SF,m)

         call grad(temp_VF,phi,m)
         call lap_centered(temp_GL,temp_VF,m,temp_TF)

         call subtract(temp_GL,temp_LG)
         call assign_ghost_XPeriodic(temp_GL,0.0_cp)

         call export_raw(m,temp_GL,str(DT%test%field),'temp_GL',0)

         call Ln(e,temp_GL,2.0_cp,m)
         write(*,*) 'error grad_laplacian = ',e
         call delete(phi)
         call delete(temp_SF)
         call delete(temp_VF)
         call delete(temp_LG)
         call delete(temp_GL)
         call delete(temp_TF)
       end subroutine

       subroutine curl_grad_equal_zero(U,p,m,DT)
         implicit none
         type(VF),intent(in) :: U
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(SF) :: phi
         type(VF) :: temp_F,temp_E
         real(cp) :: e
         call init(phi,P);         call assign(phi,0.0_cp)
         call init(temp_F,U);      call assign(temp_F,0.0_cp)
         call init_Edge(temp_E,m); call assign(temp_E,0.0_cp)

         call random_noise(phi)

         call grad(temp_F,phi,m)
         call curl(temp_E,temp_F,m)
         call assign_ghost_XPeriodic(temp_E,0.0_cp)

         call export_raw(m,temp_E,str(DT%test%field),'temp_E',0)

         call Ln(e,temp_E,2.0_cp,m)
         write(*,*) 'error curl_grad_equal_zero = ',e
         call delete(phi)
         call delete(temp_F)
         call delete(temp_E)
       end subroutine

       end module