       module Poisson_test_mod
       use current_precision_mod
       use constants_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use dir_tree_mod
       use string_mod
       use path_mod
       use export_raw_processed_mod
       use ops_discrete_mod
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

       public :: Poisson_test

       contains

       subroutine Poisson_test(U,p,m,DT,SP)
         implicit none
         type(VF),intent(in) :: U
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(sim_params),intent(in) :: SP
         write(*,*) ' ************************************************************ '
         write(*,*) ' ******************** BEGIN POISSON TEST ******************** '
         write(*,*) ' ************************************************************ '
         write(*,*) ' ------------------------- TEST 1 --------------------------- '
         call Poisson_test_CC(p,m,DT,SP)     ! Tests residual drop for CC data
         write(*,*) ' ------------------------- TEST 2 --------------------------- '
         call Poisson_test_Face(U,m,DT,SP)   ! Tests residual drop for Face data
         write(*,*) ' ------------------------- TEST 3 --------------------------- '
         ! call Poisson_test_cleanU(U,p,m,DT,SP) ! Tests full cleaning procedure w/ intermediate field
         write(*,*) ' ************************************************************ '
         write(*,*) ' ********************** END POISSON TEST ******************** '
         write(*,*) ' ************************************************************ '
       end subroutine

       subroutine Poisson_test_CC(p,m,DT,SP)
         implicit none
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(sim_params),intent(in) :: SP
         type(SF) :: phi,temp_CC
         type(VF) :: temp_F
         type(PCG_solver_SF) :: PCG
         type(iter_solver_params) :: ISP
         type(matrix_free_params) :: MFP
         call init(ISP,10000,pow(-15),pow(-15),1,.true.,str(DT%test%field),'Poisson_test_CC')
         call init(phi,p)
         call assign(phi,0.0_cp)
         call init_CC(temp_CC,m)
         call init_Face(temp_F,m)

         phi%all_Neumann = .true.
         call print_BCs(phi,'phi')

         call init(PCG,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_Lap_SF,m,&
         ISP,MFP,phi,temp_F,str(DT%test%residual),'Poisson_test_CC',.false.,.false.)

         call random_noise(phi) ! Does not work for periodic BCs, but does work for Dirichlet BCs
         call cosine_waves(phi,m,(/2.0_cp/PI,2.0_cp,2.0_cp/),(/0.0_cp,0.0_cp,0.0_cp/))
         call apply_BCs(phi)
         call assign(temp_CC,phi)
         call assign(phi,0.0_cp)
         call export_processed(m,temp_CC,str(DT%test%field),'source_CC',1)

         call solve(PCG,phi,temp_CC,m,.false.)

         call export_processed(m,phi,str(DT%test%field),'phi_CC',1)
         call delete(phi)
         call delete(temp_CC)
         call delete(temp_F)
         call delete(PCG)
         call delete(ISP)
         call delete(MFP)
       end subroutine

       subroutine Poisson_test_Face(X,m,DT,SP)
         implicit none
         type(VF),intent(in) :: X
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(sim_params),intent(in) :: SP
         type(VF) :: U,temp_F,temp_E
         type(PCG_solver_VF) :: PCG
         type(iter_solver_params) :: ISP
         type(matrix_free_params) :: MFP
         call init(ISP,10000,pow(-15),pow(-15),1,.true.,str(DT%test%field),'Poisson_test_Face')
         call init(U,X)
         call assign(U,0.0_cp)
         call init_Edge(temp_E,m)
         call init_Face(temp_F,m)

         call print_BCs(U,'Face')

         call init(PCG,Lap_uniform_VF,Lap_uniform_VF_explicit,prec_Lap_VF,m,&
         ISP,MFP,U,temp_E,str(DT%test%residual),'Poisson_test_Face',.false.,.false.)

         call random_noise(U) ! Does not work for periodic BCs, but does work for Dirichlet BCs
         call sine_waves(U,m,(/2.0_cp/PI,1.0_cp,2.0_cp/),(/0.0_cp,-0.5_cp,0.0_cp/))
         call apply_BCs(U)
         call assign(temp_F,U)
         call assign(U,0.0_cp)
         call export_processed(m,temp_F,str(DT%test%field),'source_Face',1)

         call solve(PCG,U,temp_F,m,.false.)

         call export_processed(m,U,str(DT%test%field),'U_Face',1)
         call export_raw(m,U,str(DT%test%field),'U_Face',0)
         call export_raw(m,PCG%r,str(DT%test%field),'Residual_Face',0)
         call delete(U)
         call delete(temp_E)
         call delete(temp_F)
         call delete(PCG)
         call delete(ISP)
         call delete(MFP)
       end subroutine

       subroutine Poisson_test_cleanU(X,p,m,DT,SP)
         implicit none
         type(VF),intent(in) :: X
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(sim_params),intent(in) :: SP
         type(VF) :: U,Ustar,temp_F1,temp_F2,temp_E,temp_CC
         type(SF) :: divU,phi
         type(PCG_solver_VF) :: PCG_U
         type(PCG_solver_SF) :: PCG_P
         type(iter_solver_params) :: ISP
         type(matrix_free_params) :: MFP
         call init(ISP,10000,pow(-15),pow(-15),1,.true.,str(DT%test%field),'CC data')
         call init(U,X)
         call init(Ustar,X)
         call init(phi,p)
         call init_Face(temp_F1,m)
         call init_Face(temp_F2,m)
         call init_Edge(temp_E,m)
         call init_CC(temp_CC,m)
         call init_CC(divU,m)
         call random_noise(Ustar)
         call apply_BCs(Ustar)
         call div(divU,Ustar,m)
         call init(PCG_P,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_Lap_SF,m,&
         ISP,MFP,phi,temp_E,str(DT%test%residual),'phi',.false.,.false.)
         call solve(PCG_P,phi,divU,m,.false.)
         call update_intermediate_field_BCs(Ustar,U,phi,m,temp_F1,temp_F2,temp_CC)
         call export_processed(m,U    ,str(DT%test%field),'U',1)
         call export_processed(m,Ustar,str(DT%test%field),'Ustar',1)
         call delete(U)
         call delete(Ustar)
         call delete(divU)
         call delete(phi)
         call delete(temp_E)
         call delete(PCG_P)
         call delete(PCG_U)
         call delete(ISP)
         call delete(MFP)
       end subroutine

       end module