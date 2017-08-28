       module Taylor_Green_Vortex_test_mod
       use current_precision_mod
       use constants_mod
       use mesh_mod
       use GF_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use dir_tree_mod
       use string_mod
       use path_mod
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

       public :: Taylor_Green_Vortex_test

       contains

       subroutine Taylor_Green_Vortex_test(U,p,m,DT,SP)
         implicit none
         type(VF),intent(in) :: U
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(sim_params),intent(in) :: SP
         type(VF) :: U_analytic
         type(SF) :: P_error,P_analytic,p_actual,p_mod
         real(cp) :: e,dtime,Re
         Re = SP%DP%Re
         dtime = SP%VS%U%TMP%dt
         write(*,*) ' ************************************************************ '
         write(*,*) ' ************** BEGIN TAYLOR GREEN VORTEX TEST ************** '
         write(*,*) ' ************************************************************ '
         call init(U_analytic,U)
         call init(P_analytic,p)
         call init(p_actual,p)
         call init(p_mod,p)
         call init(P_error,p)
         call export_processed(m,U,str(DT%test%field),'U_numerical',0)
         call export_processed(m,P,str(DT%test%field),'P_numerical',0)
         call export_raw(m,P,str(DT%test%field),'P_numerical',0)
         call compute_analytic_solution(U_analytic,P_analytic,m,DT,SP)
         call subtract(U_analytic,U)
         call subtract(P_error,P_analytic,p)
         call export_raw(m,U_analytic,str(DT%test%field),'U_error',0)
         call export_raw(m,P_error,str(DT%test%field),'P_error',0)
         call lap(p_mod,p,m)
         call multiply(p_mod,-SP%VS%U%MFP%alpha*dtime/Re)
         call export_raw(m,p_mod,str(DT%test%field),'P_modify',0)
         call add(p_actual,p,p_mod)
         call export_raw(m,p_actual,str(DT%test%field),'p_actual',0)
         call subtract(P_error,P_analytic,p_actual)
         call export_raw(m,P_error,str(DT%test%field),'P_error_actual',0)
         write(*,*) 't = ',SP%VS%U%TMP%t
         call Ln(e,U_analytic,2.0_cp,m); write(*,*) 'dt,e(U) = ',dtime,e
         call Ln(e,P_analytic,2.0_cp,m); write(*,*) 'dt,e(P) = ',dtime,e
         call delete(U_analytic)
         call delete(P_analytic)
         call delete(p_actual)
         call delete(p_mod)
         call delete(P_error)
         write(*,*) ' ************************************************************ '
         write(*,*) ' *************** END TAYLOR GREEN VORTEX TEST *************** '
         write(*,*) ' ************************************************************ '
       end subroutine

       subroutine compute_analytic_solution(U,p,m,DT,SP)
         implicit none
         type(VF),intent(inout) :: U
         type(SF),intent(inout) :: p
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(sim_params),intent(in) :: SP
         real(cp) :: Re,t
         Re = SP%DP%Re
         t = SP%VS%U%TMP%t
         ! Define u
         call Taylor_Green_Vortex_U(U%x%BF(1)%GF,m%B(1)%g,U%x%DL,Re,t)
         call Taylor_Green_Vortex_V(U%y%BF(1)%GF,m%B(1)%g,U%y%DL,Re,t)
         call assign(U%z,0.0_cp)
         call Taylor_Green_Vortex_P(P%BF(1)%GF  ,m%B(1)%g,P%DL  ,Re,t)
         call export_processed(m,U,str(DT%test%field),'U_TGV_analytic',0)
         call export_processed(m,P,str(DT%test%field),'P_TGV_analytic',0)
       end subroutine

       end module