       module unit_test_mod
       use current_precision_mod
       use mesh_mod
       use BCs_mod
       use apply_BCs_mod
       use SF_mod
       use VF_mod
       use IO_SF_mod
       use IO_tools_mod
       use geometries_mod
       use ops_discrete_mod
       use ops_aux_mod
       use CG_mod
       use PCG_mod
       use preconditioners_mod
       use matrix_free_operators_mod
       use matrix_free_params_mod
       use export_raw_processed_mod

       implicit none

       contains

       subroutine unit_test(dir)
         implicit none
         character(len=*),intent(in) :: dir ! directory
         type(mesh) :: m
         type(SF) :: phi,divB
         type(VF) :: B,temp_F,Btemp

         type(matrix_free_params) :: MFP
         type(CG_solver_SF) :: CG
         type(PCG_solver_SF) :: PCG
         integer :: i

         call cube(m) ! in mesh_generate.f90
         ! call cube_uniform(m) ! in mesh_generate.f90
         call export(m,dir,'mesh')

         call init_Edge(B,m)
         call init_Edge(Btemp,m)
         call init_Edge(temp_F,m)
         call init_Node(divB,m)
         call init_Node(phi,m)

         call init_BC_mesh(phi,m)
         call init_Dirichlet(phi%RF(1)%b)
         call init(phi%RF(1)%b,0.0_cp)
         phi%all_Neumann = .false.
         write(*,*) 'allNeumann = ',phi%all_Neumann

         call init_BC_mesh(B%x,m)
         call init_BC_mesh(B%y,m)
         call init_BC_mesh(B%z,m)
         call init_Dirichlet(B%x%RF(1)%b); call init_BCs(B%x,0.0_cp)
         call init_Dirichlet(B%y%RF(1)%b); call init_BCs(B%y,0.0_cp)
         call init_Dirichlet(B%z%RF(1)%b); call init_BCs(B%z,0.0_cp)
         call init_Neumann(B%x%RF(1)%b,1); call init_Neumann(B%x%RF(1)%b,2)
         call init_Neumann(B%y%RF(1)%b,3); call init_Neumann(B%y%RF(1)%b,4)
         call init_Neumann(B%z%RF(1)%b,5); call init_Neumann(B%z%RF(1)%b,6)

         call noise(B)
         call zeroGhostPoints(B)
         B%x%RF(1)%f(2,:,:) = 0.0_cp; B%x%RF(1)%f(B%x%RF(1)%s(1)-1,:,:) = 0.0_cp
         B%y%RF(1)%f(:,2,:) = 0.0_cp; B%y%RF(1)%f(:,B%y%RF(1)%s(2)-1,:) = 0.0_cp
         B%z%RF(1)%f(:,:,2) = 0.0_cp; B%z%RF(1)%f(:,:,B%z%RF(1)%s(3)-1) = 0.0_cp
         call apply_BCs(B,m)
         call div(divB,B,m)
         write(*,*) 'sum(divB) = ',sum(divB)
         call zeroGhostPoints(divB)
         call export_raw(m,B,dir,'Bstar_PCG',0)
         call export_raw(m,divB,dir,'divB_PCG',0)

         call init(PCG,Lap_uniform_props,Lap_uniform_props_explicit,prec_lap_SF,&
         m,MFP,phi,temp_F,dir,'phi',.true.,.false.,.false.)
         call assign(phi,0.0_cp)
         call solve(PCG,phi,divB,m,phi%numEl,.true.)
         write(*,*) 'sum(phi) = ',sum(phi)
         call grad(temp_F,phi,m)
         write(*,*) 'sum(gradPhi_x) = ',sum(temp_F%x)
         write(*,*) 'sum(gradPhi_y) = ',sum(temp_F%y)
         write(*,*) 'sum(gradPhi_z) = ',sum(temp_F%z)
         call subtract(B,temp_F)
         call apply_BCs(B,m)
         call div(divB,B,m)
         call zeroGhostPoints(divB)
         write(*,*) 'max(divB_clean) = ',max(divB)
         call export_raw(m,temp_F,dir,'gradPhi_PCG',0)
         call export_raw(m,divB,dir,'divB_clean_PCG',0)
         call export_raw(m,phi,dir,'phi_PCG',0)
         call export_processed(m,phi,dir,'phi_PCG',0)
         call export_raw(m,B,dir,'B_clean_PCG',0)

         call noise(B)
         call zeroGhostPoints(B)
         B%x%RF(1)%f(2,:,:) = 0.0_cp; B%x%RF(1)%f(B%x%RF(1)%s(1)-1,:,:) = 0.0_cp
         B%y%RF(1)%f(:,2,:) = 0.0_cp; B%y%RF(1)%f(:,B%y%RF(1)%s(2)-1,:) = 0.0_cp
         B%z%RF(1)%f(:,:,2) = 0.0_cp; B%z%RF(1)%f(:,:,B%z%RF(1)%s(3)-1) = 0.0_cp
         call apply_BCs(B,m)
         call div(divB,B,m)
         write(*,*) 'sum(divB) = ',sum(divB)
         call zeroGhostPoints(divB)
         call export_raw(m,B,dir,'Bstar_CG',0)
         call export_raw(m,divB,dir,'divB_CG',0)
         call init(CG,Lap_uniform_props,Lap_uniform_props_explicit,&
         m,MFP,phi,temp_F,dir,'phi',.true.,.false.)
         call assign(phi,0.0_cp)
         call solve(CG,phi,divB,m,phi%numEl,.true.)
         write(*,*) 'sum(phi) = ',sum(phi)
         call grad(temp_F,phi,m)
         write(*,*) 'sum(gradPhi_x) = ',sum(temp_F%x)
         write(*,*) 'sum(gradPhi_y) = ',sum(temp_F%y)
         write(*,*) 'sum(gradPhi_z) = ',sum(temp_F%z)
         call subtract(B,temp_F)
         call apply_BCs(B,m)
         call div(divB,B,m)
         call zeroGhostPoints(divB)
         write(*,*) 'max(divB_clean) = ',max(divB)
         call export_raw(m,temp_F,dir,'gradPhi_CG',0)
         call export_raw(m,divB,dir,'divB_clean_CG',0)
         call export_raw(m,phi,dir,'phi_CG',0)
         call export_processed(m,phi,dir,'phi_CG',0)
         call export_raw(m,B,dir,'B_clean_CG',0)

         call delete(m)
         call delete(PCG)
         call delete(CG)
         call delete(B)
         call delete(Btemp)
         call delete(temp_F)
         call delete(phi)
         call delete(divB)
       end subroutine

       end module
