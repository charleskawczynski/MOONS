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
       use GS_poisson_mod
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
         type(SF) :: u,f,u_sol
         type(VF) :: temp_F
         type(matrix_free_params) :: MFP
         type(CG_solver_SF) :: CG
         ! type(GS_poisson) :: GS
         integer :: i

         ! call cube(m) ! in mesh_generate.f90
         call cube_uniform(m) ! in mesh_generate.f90
         call print(m)

         call init_CC(u,m)
         call init_CC(u_sol,m)
         call init_Face(temp_F,m)
         call init_CC(f,m)

         call init_BC_mesh(u,m)
         call init_Neumann(u%RF(1)%b)
         call init_Dirichlet(u%RF(1)%b,2)
         call init(u%RF(1)%b,0.0_cp)

         do i=1,f%RF(1)%s(1)
         f%RF(1)%f(i,:,:) = m%g(1)%c(1)%hc(i)
         enddo
         do i=1,u_sol%RF(1)%s(1)
         u_sol%RF(1)%f(i,:,:) = m%g(1)%c(1)%hc(i)**3.0_cp/6.0_cp - 1.0_cp/6.0_cp
         enddo

         call apply_BCs(u,m)
         call zeroGhostPoints(f)
         write(*,*) 'allNeumann = ',u%all_Neumann
         u%all_Neumann = .false.

         call export(m,dir,'mesh')
         call export_raw(m,u,dir,'u',0)
         call export_raw(m,f,dir,'f',0)

         call init(CG,Laplacian_uniform_props,m,MFP,u,temp_F,dir,'u',.true.,.true.)
         call assign(u,0.0_cp)
         do i=1,10
         call solve(CG,u,f,m,10,.true.)
         enddo
         call export_raw(m,u,dir,'u_CG',0)
         call export_processed(m,u,dir,'u_CG',1)

         ! call init(GS,u,m,dir,'u')
         ! call assign(u,0.0_cp)
         ! call solve(GS,u,f,m,10000,.true.)
         ! call export_raw(m,u,dir,'u_GS',0)

         call export_raw(m,u_sol,dir,'u_sol',0)

         call delete(m)
         call delete(u)
         call delete(u_sol)
         call delete(f)
         call delete(CG)
         ! call delete(GS)
       end subroutine

       end module
