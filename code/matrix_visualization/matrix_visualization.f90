       module matrix_visualization_mod
       use current_precision_mod
       use mesh_mod
       use grid_mod
       use grid_init_mod
       use mesh_quality_params_mod
       use IO_export_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use dir_tree_mod
       use string_mod
       use path_mod
       use export_raw_processed_mod
       use ops_discrete_mod
       use PCG_mod
       use matrix_free_operators_mod
       use matrix_free_params_mod
       use preconditioners_mod
       use iter_solver_params_mod
       use ops_mirror_field_mod
       use sim_params_mod
       use BC_funcs_mod

       implicit none
       private

       public :: export_matrix_visualization

       contains

       subroutine export_matrix_visualization(DT)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(mesh) :: m
         type(TF) :: temp_dummy
         type(iter_solver_params) :: ISP
         type(matrix_free_params) :: MFP
         logical :: test_symmetry,export_operator
         export_operator = .true.
         test_symmetry = .true.
         call init(ISP,10000,pow(-15),pow(-15),1,.true.,.true.,str(DT%ISP),'matrix_visualization')
         call matrix_visualization_mesh(m)
         MFP%coeff_implicit_time_split = 0.1_cp
         call export_mesh(m,str(DT%matrix_visualization),'m',1)

         call export_laplacian_CC_SF(DT,m,temp_dummy,ISP,MFP,test_symmetry,export_operator)

         call export_laplacian_CC_VF(DT,m,temp_dummy,ISP,MFP,test_symmetry,export_operator)
         call export_laplacian_Face_VF(DT,m,temp_dummy,ISP,MFP,test_symmetry,export_operator)

         call export_curlcurl_CC_VF(DT,m,temp_dummy,ISP,MFP,test_symmetry,export_operator)
         call export_curlcurl_Face_VF(DT,m,temp_dummy,ISP,MFP,test_symmetry,export_operator)

         call delete(m)
         call delete(temp_dummy)
         call delete(ISP)
         call delete(MFP)
       end subroutine

       subroutine matrix_visualization_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         type(mesh_quality_params) :: MQP
         type(grid) :: g
         real(cp),dimension(3) :: hmin,hmax,beta
         integer,dimension(3) :: N
         integer :: i
         call init(MQP,.false.,2.0_cp,50)
         call delete(m)
         N = 3; hmin = -0.5_cp; hmax = 0.5_cp
         beta = 1.1_cp
         i = 1; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 2; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         i = 3; call grid_Roberts_B(g,hmin(i),hmax(i),N(i),beta(i),i,MQP)
         call add(m,g)
         call init_props(m)
         call patch(m)
         call delete(g)
       end subroutine

       subroutine prep_BCs_SF(X,m)
         implicit none
         type(SF),intent(inout) :: X
         type(mesh),intent(inout) :: m
         call init_BC_mesh(X,m)
         call Dirichlet_BCs(X,m)
       end subroutine

       subroutine prep_BCs_VF(X,m)
         implicit none
         type(VF),intent(inout) :: X
         type(mesh),intent(inout) :: m
         call init_BC_mesh(X,m)
         call Dirichlet_BCs(X,m)
       end subroutine

       subroutine export_laplacian_CC_SF(DT,m,temp_dummy,ISP,MFP,test_symmetry,export_operator)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(mesh),intent(inout) :: m
         type(TF),intent(inout) :: temp_dummy
         type(iter_solver_params),intent(inout) :: ISP
         type(matrix_free_params),intent(inout) :: MFP
         logical,intent(in) :: test_symmetry,export_operator
         type(PCG_solver_SF) :: PCG
         type(SF) :: X
         call init_CC(X,m); call assign(X,1.0_cp)
         call init_Face(temp_dummy,m); call assign(temp_dummy,1.0_cp)
         call prep_BCs_SF(X,m)
         call init(PCG,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_Lap_SF,m,&
         ISP,MFP,X,temp_dummy,str(DT%matrix_visualization),'Lap_CC',&
         test_symmetry,export_operator)
         call delete(temp_dummy)
         call delete(X)
         call delete(PCG)
       end subroutine

       subroutine export_laplacian_CC_VF(DT,m,temp_dummy,ISP,MFP,test_symmetry,export_operator)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(mesh),intent(inout) :: m
         type(TF),intent(inout) :: temp_dummy
         type(iter_solver_params),intent(inout) :: ISP
         type(matrix_free_params),intent(inout) :: MFP
         logical,intent(in) :: test_symmetry,export_operator
         type(PCG_solver_VF) :: PCG
         type(VF) :: X
         call init_CC(X,m); call assign(X,1.0_cp)
         call init_Face(temp_dummy,m); call assign(temp_dummy,1.0_cp)
         call prep_BCs_VF(X,m)
         call init(PCG,Lap_uniform_VF,Lap_uniform_VF_explicit,prec_Lap_VF,m,&
         ISP,MFP,X,temp_dummy,str(DT%matrix_visualization),'Lap_CC',&
         test_symmetry,export_operator)
         call delete(temp_dummy)
         call delete(X)
         call delete(PCG)
       end subroutine

       subroutine export_laplacian_Face_VF(DT,m,temp_dummy,ISP,MFP,test_symmetry,export_operator)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(mesh),intent(inout) :: m
         type(TF),intent(inout) :: temp_dummy
         type(iter_solver_params),intent(inout) :: ISP
         type(matrix_free_params),intent(inout) :: MFP
         logical,intent(in) :: test_symmetry,export_operator
         type(PCG_solver_VF) :: PCG
         type(VF) :: X
         call init_Face(X,m); call assign(X,1.0_cp)
         call init_CC_Edge(temp_dummy,m); call assign(temp_dummy,1.0_cp)
         call prep_BCs_VF(X,m)
         call init(PCG,mom_diffusion,mom_diffusion_explicit,prec_mom_VF,m,&
         ISP,MFP,X,temp_dummy,str(DT%matrix_visualization),'Lap_Face',&
         test_symmetry,export_operator)
         call delete(temp_dummy)
         call delete(X)
         call delete(PCG)
       end subroutine

       subroutine export_curlcurl_CC_VF(DT,m,temp_dummy,ISP,MFP,test_symmetry,export_operator)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(mesh),intent(inout) :: m
         type(TF),intent(inout) :: temp_dummy
         type(iter_solver_params),intent(inout) :: ISP
         type(matrix_free_params),intent(inout) :: MFP
         logical,intent(in) :: test_symmetry,export_operator
         type(PCG_solver_VF) :: PCG
         type(VF) :: X
         call init_CC(X,m); call assign(X,1.0_cp)
         call init_CC(temp_dummy,m); call assign(temp_dummy,1.0_cp)
         call prep_BCs_VF(X,m)
         call init(PCG,ind_diffusion,ind_diffusion_explicit,prec_ind_VF,m,&
         ISP,MFP,X,temp_dummy,str(DT%matrix_visualization),'curlcurl_CC',&
         test_symmetry,export_operator)
         call delete(temp_dummy)
         call delete(X)
         call delete(PCG)
       end subroutine

       subroutine export_curlcurl_Face_VF(DT,m,temp_dummy,ISP,MFP,test_symmetry,export_operator)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(mesh),intent(inout) :: m
         type(TF),intent(inout) :: temp_dummy
         type(iter_solver_params),intent(inout) :: ISP
         type(matrix_free_params),intent(inout) :: MFP
         logical,intent(in) :: test_symmetry,export_operator
         type(PCG_solver_VF) :: PCG
         type(VF) :: X
         call init_Face(X,m); call assign(X,1.0_cp)
         call init_Edge(temp_dummy,m); call assign(temp_dummy,1.0_cp)
         call prep_BCs_VF(X,m)
         call init(PCG,ind_diffusion,ind_diffusion_explicit,prec_ind_VF,m,&
         ISP,MFP,X,temp_dummy,str(DT%matrix_visualization),'curlcurl_Face',&
         test_symmetry,export_operator)
         call delete(temp_dummy)
         call delete(X)
         call delete(PCG)
       end subroutine

       end module