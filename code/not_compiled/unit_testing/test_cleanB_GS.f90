       module unit_test_mod
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
       use matrix_free_operators_mod
       use matrix_free_params_mod
       use export_raw_processed_mod

       implicit none
#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       contains

       subroutine unit_test(dir)
         implicit none
         character(len=*),intent(in) :: dir ! directory
         type(mesh) :: m
         type(SF) :: phi,divB
         type(VF) :: B,temp_F

         type(matrix_free_params) :: MFP
         type(GS_Poisson) :: GS
         integer :: i

         ! call cube(m) ! in mesh_generate.f90
         call cube_uniform(m) ! in mesh_generate.f90

         call init_Face(B,m)
         call init_Face(temp_F,m)
         call init_CC(divB,m)
         call init_CC(phi,m)

         call init_BC_mesh(phi,m)
         call init_Neumann(phi%RF(1)%b)
         call init(phi%RF(1)%b,0.0_cp)

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
         call add(B,0.5_cp)
         call zeroGhostPoints(B)
         call apply_BCs(B,m)
         call div(divB,B,m)
         write(*,*) 'sum(divB) = ',sum(divB)

         call zeroGhostPoints(divB)
         write(*,*) 'allNeumann = ',phi%all_Neumann

         call export(m,dir,'mesh')
         call export_raw(m,B,dir,'Bstar',0)
         call export_raw(m,divB,dir,'divB',0)

         call init(GS,phi,m,dir,'phi')
         call solve(GS,phi,divB,m,2000,.true.)
         call subtract_physical_mean(phi)
         write(*,*) 'sum(phi) = ',sum(phi)
         call apply_BCs(phi,m)
         call grad(temp_F,phi,m)
         write(*,*) 'sum(gradPhi_x) = ',sum(temp_F%x)
         write(*,*) 'sum(gradPhi_y) = ',sum(temp_F%y)
         write(*,*) 'sum(gradPhi_z) = ',sum(temp_F%z)
         call subtract(B,temp_F)
         call apply_BCs(B,m)
         call div(divB,B,m)
         call zeroGhostPoints(divB)

         call export_raw(m,temp_F,dir,'gradPhi',0)
         call export_raw(m,divB,dir,'divB_clean_CG',0)
         call export_raw(m,phi,dir,'phi',0)
         call export_processed(m,phi,dir,'phi',0)
         call export_raw(m,B,dir,'B_clean',0)

         call delete(GS)
         call delete(m)
         call delete(B)
         call delete(temp_F)
         call delete(phi)
         call delete(divB)
       end subroutine

       end module
