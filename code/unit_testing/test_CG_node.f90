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
       real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

       contains

       subroutine unit_test(dir)
         implicit none
         character(len=*),intent(in) :: dir ! directory
         type(mesh) :: m
         type(SF) :: u,f,u_sol
         type(VF) :: temp_F
         type(matrix_free_params) :: MFP
         type(CG_solver_SF) :: CG
         integer :: i

         ! call cube(m) ! in mesh_generate.f90
         call cube_uniform(m) ! in mesh_generate.f90
         call print(m)

         call init_Node(u,m)
         call init_Node(u_sol,m)
         call init_Edge(temp_F,m)
         call init_Node(f,m)

         call init_BC_mesh(u,m)
         call init_Neumann(u%RF(1)%b)
         call init_Dirichlet(u%RF(1)%b,1)
         call init(u%RF(1)%b,0.0_cp)
         call init(u%RF(1)%b,1.0_cp,1)

         do i=1,f%RF(1)%s(1)
         f%RF(1)%f(i,:,:) = -(2.0_cp*PI)**2.0_cp*cos(2.0_cp*PI*m%g(1)%c(1)%hn(i))
         enddo
         ! call sineWaves(f,m,(/2.0_cp,2.0_cp,2.0_cp/)) ! For Serial vs Parallel test
         ! call noise(f)

         do i=1,u_sol%RF(1)%s(1)
         u_sol%RF(1)%f(i,:,:) = (m%g(1)%c(1)%hn(i)**3.0_cp)/6.0_cp - 1.0_cp/6.0_cp
         enddo
         call apply_BCs(u,m)
         call zeroGhostPoints(f)
         write(*,*) 'allNeumann before = ',u%all_Neumann
         u%all_Neumann = .false.
         write(*,*) 'allNeumann after = ',u%all_Neumann

         call export(m,dir,'mesh')
         call export_raw(m,u,dir,'u',0)
         call export_raw(m,f,dir,'f',0)

         call init(CG,Lap_uniform_props,Lap_uniform_props_explicit,m,MFP,u,temp_F,dir,'u',.true.,.false.)
         call assign(u,0.0_cp)
         call solve(CG,u,f,m,u%numEl,.true.)
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
