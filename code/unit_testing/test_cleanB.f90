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
       use PCG_mod
       use preconditioners_mod
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
         type(VF) :: B,temp_F,Btemp

         type(matrix_free_params) :: MFP
         type(PCG_solver_SF) :: PCG
         type(SF) :: Minv

         ! call cube(m) ! in mesh_generate.f90
         call cube_uniform(m) ! in mesh_generate.f90

         call init_Face(B,m)
         call init_Face(Btemp,m)
         call init_Face(temp_F,m)
         call init_CC(divB,m)
         call init_CC(phi,m)

         call init_BC_mesh(phi,m)
         call init_Neumann(phi%RF(1)%b)
         ! call init_Dirichlet(phi%RF(1)%b)
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
         ! call sineWaves(B%x,m,(/2.0_cp,2.0_cp,2.0_cp/)) ! For Serial vs Parallel test
         ! call sineWaves(B%y,m,(/2.0_cp,2.0_cp,2.0_cp/)) ! For Serial vs Parallel test
         ! call sineWaves(B%z,m,(/2.0_cp,2.0_cp,2.0_cp/)) ! For Serial vs Parallel test
         ! call cosineWaves(B%x,m,(/2.0_cp,2.0_cp,2.0_cp/)) ! For Serial vs Parallel test
         ! call cosineWaves(B%y,m,(/2.0_cp,2.0_cp,2.0_cp/)) ! For Serial vs Parallel test
         ! call cosineWaves(B%z,m,(/2.0_cp,2.0_cp,2.0_cp/)) ! For Serial vs Parallel test

         call zeroGhostPoints(B)
         if (B%is_Face) then
           ! Set Bn = 0 on boundaries
           B%x%RF(1)%f(2,:,:) = 0.0_cp; B%x%RF(1)%f(B%x%RF(1)%s(1)-1,:,:) = 0.0_cp
           B%y%RF(1)%f(:,2,:) = 0.0_cp; B%y%RF(1)%f(:,B%y%RF(1)%s(2)-1,:) = 0.0_cp
           B%z%RF(1)%f(:,:,2) = 0.0_cp; B%z%RF(1)%f(:,:,B%z%RF(1)%s(3)-1) = 0.0_cp

           call init(phi%RF(1)%b,B%x%RF(1)%f(2,:,:),1)
           call init(phi%RF(1)%b,B%y%RF(1)%f(:,2,:),3)
           call init(phi%RF(1)%b,B%z%RF(1)%f(:,:,2),5)
           call init(phi%RF(1)%b,B%x%RF(1)%f(B%x%RF(1)%s(1)-1,:,:),2)
           call init(phi%RF(1)%b,B%y%RF(1)%f(:,B%y%RF(1)%s(2)-1,:),4)
           call init(phi%RF(1)%b,B%z%RF(1)%f(:,:,B%z%RF(1)%s(3)-1),6)

         else; stop 'Error: B should be located on cell face or edge in test_cleanB.f90'
         endif
         call apply_BCs(B,m)
         call div(divB,B,m)
         write(*,*) 'sum(divB) = ',sum(divB)

         call zeroGhostPoints(divB)
         ! phi%all_Neumann = .true.
         write(*,*) 'allNeumann = ',phi%all_Neumann

         call export(m,dir,'mesh')
         call export_raw(m,B,dir,'Bstar',0)
         call export_raw(m,divB,dir,'divB',0)

         call init(Minv,phi)
         ! call prec_lap_SF(Minv,m)
         call prec_Identity_SF(Minv)
         call init(PCG,Lap_uniform_props,Lap_uniform_props_explicit,Minv,&
         m,MFP,phi,temp_F,dir,'phi',.true.,.false.)
         call delete(Minv)

         call solve(PCG,phi,divB,m,phi%numEl,.true.)
         call grad(temp_F,phi,m)
         call subtract(B,temp_F)
         call apply_BCs(B,m)
         call div(divB,B,m)
         call zeroGhostPoints(divB)
         write(*,*) 'sum(phi) = ',sum(phi)
         write(*,*) 'sum(gradPhi_x) = ',sum(temp_F%x)
         write(*,*) 'sum(gradPhi_y) = ',sum(temp_F%y)
         write(*,*) 'sum(gradPhi_z) = ',sum(temp_F%z)
         write(*,*) 'max(divB_clean) = ',max(divB)

         call export_raw(m,temp_F,dir,'gradPhi',0)
         call export_raw(m,divB,dir,'divB_clean_CG',0)
         call export_raw(m,phi,dir,'phi',0)
         call export_processed(m,phi,dir,'phi',0)
         call export_raw(m,B,dir,'B_clean',0)

         call delete(m)
         call delete(PCG)
         call delete(B)
         call delete(Btemp)
         call delete(temp_F)
         call delete(phi)
         call delete(divB)
       end subroutine

       end module
