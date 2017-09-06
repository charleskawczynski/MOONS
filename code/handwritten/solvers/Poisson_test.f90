       module Poisson_test_mod
       use current_precision_mod
       use constants_mod
       use mesh_extend_mod
       use SF_extend_mod
       use VF_mod
       use TF_mod
       use dir_tree_mod
       use string_mod
       use path_extend_mod
       use path_extend_mod
       use export_raw_processed_mod
       use ops_discrete_mod
       use ops_aux_mod
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

       subroutine Poisson_test(U,p,m,DT)
         implicit none
         type(VF),intent(in) :: U
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         write(*,*) ' ************************************************************ '
         write(*,*) ' ******************** BEGIN POISSON TEST ******************** '
         write(*,*) ' ************************************************************ '
         write(*,*) ' ------------------------- TEST 1 --------------------------- '
         call Poisson_test_CC(p,m,DT)     ! Tests residual drop for CC data
         write(*,*) ' ------------------------- TEST 2 --------------------------- '
         ! call Poisson_test_Face(U,m,DT)   ! Tests residual drop for Face data
         write(*,*) ' ************************************************************ '
         write(*,*) ' ********************** END POISSON TEST ******************** '
         write(*,*) ' ************************************************************ '
       end subroutine

       subroutine Poisson_test_CC(p,m,DT)
         implicit none
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(SF) :: phi,temp_CC
         type(TF) :: temp_F
         type(PCG_solver_SF) :: PCG
         type(iter_solver_params) :: ISP
         type(matrix_free_params) :: MFP
         logical,parameter :: T = .true.
         call init(ISP,2000,pow(-20),pow(-18),1,T,T,str(DT%test%field),'Poisson_test_CC')
         call init(phi,p)
         call assign(phi,0.0_cp)
         call init_CC(temp_CC,m)
         call init_Face(temp_F,m)
         call Neumann_BCs(phi,m)

         phi%all_Neumann = .true.
         call print_BCs(phi,'phi')

         call init(PCG,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_Lap_SF,m,&
         ISP,MFP,phi,phi,temp_F,str(DT%test%residual),'Poisson_test_CC',.false.,.false.)

         call random_noise(phi) ! Does not work for periodic BCs, but does work for Dirichlet BCs
         ! call cosine_waves(phi,m,(/2.0_cp/PI,2.0_cp/PI,2.0_cp/),(/0.0_cp,0.0_cp,0.0_cp/))
         call assign(temp_CC,phi)
         call assign_ghost_XPeriodic(temp_CC,0.0_cp)
         call assign(phi,0.0_cp)
         call export_raw(m,temp_CC,str(DT%test%field),'source_CC',0)

         call solve(PCG,phi,temp_CC,m,.false.)
         call export_raw(m,PCG%r,str(DT%test%residual),'residual_CC',0)

         call export_raw(m,phi,str(DT%test%field),'phi_CC',0)
         call delete(phi)
         call delete(temp_CC)
         call delete(temp_F)
         call delete(PCG)
         call delete(ISP)
         call delete(MFP)
       end subroutine

       subroutine Poisson_test_Face(X,m,DT)
         implicit none
         type(VF),intent(in) :: X
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(VF) :: U,temp_F
         type(TF) :: temp_CC_edge
         type(PCG_solver_VF) :: PCG
         type(iter_solver_params) :: ISP
         type(matrix_free_params) :: MFP
         logical,parameter :: T = .true.
         call init(ISP,10000,pow(-15),pow(-15),1,T,T,str(DT%test%field),'Poisson_test_Face')
         call init(U,X)
         call assign(U,0.0_cp)
         call init_CC_edge(temp_CC_edge,m)
         call init_Face(temp_F,m)

         call print_BCs(U,'Face')

         call init(PCG,Lap_uniform_VF,Lap_uniform_VF_explicit,prec_Lap_VF,m,&
         ISP,MFP,U,U,temp_CC_edge,str(DT%test%residual),'Poisson_test_Face',.false.,.false.)

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
         call delete(temp_CC_edge)
         call delete(temp_F)
         call delete(PCG)
         call delete(ISP)
         call delete(MFP)
       end subroutine

       end module