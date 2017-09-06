       module vorticity_streamfunction_mod
       use current_precision_mod
       use mesh_extend_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use dir_tree_mod
       use string_mod
       use path_extend_mod
       use path_extend_mod
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

       public :: export_vorticity_streamfunction

       contains

       subroutine compute_vorticity_streamfunction(PCG,psi,omega,U,m,compute_norms)
         implicit none
         type(PCG_solver_VF),intent(inout) :: PCG
         type(VF),intent(inout) :: psi,omega
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         logical,intent(in) :: compute_norms
         call curl(omega,U,m)
         call multiply(omega,-1.0_cp)
         call solve(PCG,psi,omega,m,compute_norms)
       end subroutine

       subroutine export_vorticity_streamfunction(U,m,DT,SP)
         implicit none
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(sim_params),intent(in) :: SP
         type(VF) :: U_temp
         type(mesh) :: m_temp
         write(*,*) ' COMPUTING VORTICITY-STREAMFUNCTION:'
         if (SP%EL%export_symmetric) then
         call mirror_field(m_temp,U_temp,m,U,SP%MP)
         call export_vorticity_streamfunction_wrapper(U_temp,m_temp,DT,SP)
         call delete(U_temp)
         call delete(m_temp)
         else; call export_vorticity_streamfunction_wrapper(U,m,DT,SP)
         endif
       end subroutine

       subroutine export_vorticity_streamfunction_wrapper(U,m,DT,SP)
         implicit none
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(sim_params),intent(in) :: SP
         type(VF) :: psi,omega
         type(TF) :: temp_dummy
         type(PCG_solver_VF) :: PCG
         type(iter_solver_params) :: ISP
         type(matrix_free_params) :: MFP
         real(cp),dimension(6) :: c_w,Robin_coeff
         call init(ISP,10000,pow(-15),pow(-15),1,.true.,.true.,str(DT%ISP),'vorticity_streamfunction')

         call init_Edge(omega,m)
         call init_Edge(psi,m)
         call init_Node_Face(temp_dummy,m)
         call init_BC_mesh(psi%x,m)
         call init_BC_mesh(psi%y,m)
         call init_BC_mesh(psi%z,m)
         call Dirichlet_BCs(psi,m)
         Robin_coeff = 0.0_cp
         c_w = 0.0_cp
         call init_BC_props(psi,c_w,Robin_coeff)
         call make_periodic(psi,m,SP%GP%periodic_dir)

         ! Make sure that Lap_uniform_VF does not
         call init(PCG,Lap_uniform_VF,Lap_uniform_VF_explicit,prec_Lap_VF,m,&
         ISP,MFP,psi,psi,temp_dummy,str(DT%U%residual),'streamfunction',.false.,.false.)

         call compute_vorticity_streamfunction(PCG,psi,omega,U,m,.true.)

         call export_processed(m,psi  ,str(DT%U%field),'streamfunction',1)
         call export_processed(m,omega,str(DT%U%field),'vorticity'     ,1)

         call delete(omega)
         call delete(temp_dummy)
         call delete(psi)
         call delete(PCG)
         call delete(ISP)
         call delete(MFP)
       end subroutine

       end module