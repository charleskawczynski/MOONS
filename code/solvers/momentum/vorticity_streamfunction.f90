       module vorticity_streamfunction_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
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

       subroutine export_vorticity_streamfunction(U,m,DT)
         implicit none
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         type(VF) :: psi,omega,temp_dummy
         type(PCG_solver_VF) :: PCG
         type(iter_solver_params) :: ISP
         type(matrix_free_params) :: MFP
         call init(ISP,1000,10.0_cp**(-12.0_cp),10.0_cp**(-15.0_cp),1,str(DT%ISP),'vorticity_streamfunction')

         call init_Edge(omega,m)
         call init_Edge(psi,m)
         call init_Edge(temp_dummy,m)

         call init_BC_mesh(psi%x,m);    call init_BCs(psi%x,0.0_cp)
         call init_BC_mesh(psi%y,m);    call init_BCs(psi%y,0.0_cp)
         call init_BC_mesh(psi%z,m);    call init_BCs(psi%z,0.0_cp)
         call init_BC_Dirichlet(psi%x); call init_BC_props(psi%x)
         call init_BC_Dirichlet(psi%y); call init_BC_props(psi%y)
         call init_BC_Dirichlet(psi%z); call init_BC_props(psi%z)

         ! Make sure that Lap_uniform_VF does not
         call init(PCG,Lap_uniform_VF,Lap_uniform_VF_explicit,prec_Lap_VF,m,&
         ISP,MFP,psi,temp_dummy,str(DT%U_r),'streamfunction',.false.,.false.)

         call compute_vorticity_streamfunction(PCG,psi,omega,U,m,.true.)
         call export_processed(m,psi  ,str(DT%U_f),'streamfunction',1)
         call export_processed(m,omega,str(DT%U_f),'vorticity'     ,1)

         call delete(omega)
         call delete(temp_dummy)
         call delete(psi)
         call delete(PCG)
       end subroutine

       end module