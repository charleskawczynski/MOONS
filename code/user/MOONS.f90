       module MOONS_mod
       use current_precision_mod
       use IO_tools_mod
       use IO_import_mod
       use IO_export_mod

       use version_mod
       use mesh_mod
       use mesh_domain_mod
       use mesh_generate_mod
       use VF_mod
       use string_mod
       use path_mod
       use dir_tree_mod
       use export_analytic_mod
       use vorticity_streamfunction_mod

       use iter_solver_params_mod
       use time_marching_params_mod
       use sim_params_mod
       use export_raw_processed_symmetry_mod

       use energy_mod
       use momentum_mod
       use induction_mod
       use MHDSolver_mod

       implicit none

       private
       public :: MOONS

       contains

       subroutine MOONS(dir_target)
         implicit none
         character(len=*),intent(in) :: dir_target
         ! ********************** BIG VARIABLES *************************
         type(momentum) :: mom
         type(induction) :: ind
         type(energy) :: nrg
         type(mesh) :: mesh_mom,mesh_ind
         ! type(mesh) :: mesh_ind_interior
         ! ********************** MEDIUM VARIABLES **********************
         type(mesh_domain) :: MD_fluid,MD_sigma
         type(dir_tree) :: DT
         type(sim_params) :: SP
         ! ************************************************************** Parallel + directory + input parameters

#ifdef fopenmp
         call omp_set_num_threads(12) ! Set number of openMP threads
#endif

         call init(DT,dir_target)  ! Initialize + make directory tree

         call init(SP,DT)             ! Initializes simulation parameters
         call display(SP,str(DT%params),'sim_params_initial')
         call print_version()
         call export_version(str(DT%LDC))

         ! ************************************************************** Initialize mesh + domains
         if (SP%restart_all) then
           call import(mesh_mom,str(DT%restart),'mesh_mom')
           call import(mesh_ind,str(DT%restart),'mesh_ind')
           call import(MD_sigma,str(DT%restart),'MD_sigma')
         else
           call mesh_generate(mesh_mom,mesh_ind,MD_sigma,SP)
           if (SP%EL%export_meshes) call export(mesh_mom,str(DT%restart),'mesh_mom')
           if (SP%EL%export_meshes) call export(mesh_ind,str(DT%restart),'mesh_ind')
           if (SP%EL%export_meshes) call export(MD_sigma,str(DT%restart),'MD_sigma')
         endif
         ! call init(mesh_ind_interior,MD_sigma%m_R2)

         if (SP%VS%U%SS%initialize) then; call init_props(mesh_mom); call patch(mesh_mom); endif
         if (SP%VS%B%SS%initialize) then; call init_props(mesh_ind); call patch(mesh_ind); endif
         if (SP%VS%U%SS%initialize) then; call init_apply_BC_order(mesh_mom,SP%GP%apply_BC_order); endif
         if (SP%VS%B%SS%initialize) then; call init_apply_BC_order(mesh_ind,SP%GP%apply_BC_order); endif

         if (SP%VS%U%SS%initialize.and.SP%VS%B%SS%initialize) then
           call init(MD_fluid,mesh_mom,mesh_ind) ! Domain,interior,exterior
           call init_props(MD_fluid%m_R1); call patch(MD_fluid%m_R1)
           call init_props(MD_fluid%m_R2); call patch(MD_fluid%m_R2)
           call init_props(MD_sigma%m_R1); call patch(MD_sigma%m_R1)
           call init_props(MD_sigma%m_R2); call patch(MD_sigma%m_R2)
         endif

         ! ******************** EXPORT GRIDS **************************** Export mesh (to plot)
         if (SP%EL%export_meshes) call export_mesh(mesh_mom,str(DT%meshes),'mesh_mom',1)
         if (SP%EL%export_meshes) call export_mesh(MD_sigma%m_R2,str(DT%meshes),'mesh_MD_sigma',1)
         if (SP%EL%export_meshes) call export_mesh(mesh_ind,str(DT%meshes),'mesh_ind',1)
         if (SP%stop_after_mesh_export) then
           stop 'Exported meshes. Turn off stop_after_mesh_export in sim_params.f90 to run sim.'
         endif

         ! Initialize energy,momentum,induction
         if (SP%VS%U%SS%initialize) call init(mom,mesh_mom,SP,DT)
         if (SP%VS%T%SS%initialize) call init(nrg,mesh_ind,SP,DT,MD_fluid)
         if (SP%VS%B%SS%initialize) call init(ind,mesh_ind,SP,DT,MD_fluid,MD_sigma)

         ! Clean up constructor copies
         call delete(mesh_mom)
         call delete(mesh_ind)
         ! call delete(mesh_ind_interior)
         call delete(MD_fluid)
         call delete(MD_sigma)

         ! ********************* EXPORT RAW ICs *************************
         if (SP%EL%export_ICs.and.SP%VS%U%SS%initialize) call export_tec(nrg,DT)
         if (SP%EL%export_ICs.and.SP%VS%T%SS%initialize) call export_tec(ind,DT)
         if (SP%EL%export_ICs.and.SP%VS%B%SS%initialize) call export_tec(mom,DT,mom%temp_F1)

         if (SP%VS%U%SS%initialize) call print(nrg%m)
         if (SP%VS%T%SS%initialize) call print(mom%m)
         if (SP%VS%B%SS%initialize) call print(ind%m)

         ! ******************** PREP TIME START/STOP ********************
         if (SP%stop_before_solve) then
           stop 'Exported ICs. Turn off stop_before_solve in sim_params.f90 to run sim.'
         endif
         if (.not.SP%post_process_only) call MHDSolver(nrg,mom,ind,DT,SP,SP%coupled)

         ! if (post_process_only) then
           write(*,*) ' *********************** POST PROCESSING ***********************'
           write(*,*) ' *********************** POST PROCESSING ***********************'
           write(*,*) ' *********************** POST PROCESSING ***********************'
           write(*,*) ' COMPUTING VORTICITY-STREAMFUNCTION:'
           ! call export_vorticity_streamfunction(mom%U,mom%m,DT)
           ! if (solveMomentum.and.solveInduction) then
             write(*,*) ' COMPUTING ENERGY BUDGETS:'
             write(*,*) '       KINETIC ENERGY BUDGET - STARTED'
             ! call compute_E_K_Budget(mom,ind%B,ind%B0,ind%J,ind%MD_fluid,ind%Rem,DT)
             write(*,*) '       KINETIC ENERGY BUDGET - COMPLETE'
             write(*,*) '       MAGNETIC ENERGY BUDGET - STARTED'
             ! call compute_E_M_budget(ind,mom%U,ind%MD_fluid,mom%Re,mom%Ha,DT)
             write(*,*) '       MAGNETIC ENERGY BUDGET - COMPLETE'
           ! endif

           if (SP%EL%export_analytic) call export_SH(mom%m,mom%U%x,SP%DP%Ha,0.0_cp,-1.0_cp,1,DT)
         ! else
           write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
           write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
           write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
         ! endif

         ! ******************* DELETE ALLOCATED DERIVED TYPES ***********

         call delete(nrg)
         call delete(mom)
         call delete(ind)
         call delete(DT)
         call delete(SP)

         write(*,*) ' ******************** LAST LINE EXECUTED ********************'
       end subroutine

       end module
