       module MOONS_mod
       use current_precision_mod
       use IO_tools_mod
       use IO_SF_mod
       use IO_VF_mod

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
       use inputFile_mod

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
         type(mesh) :: mesh_mom,mesh_ind,mesh_ind_interior
         ! ********************** MEDIUM VARIABLES **********************
         type(mesh_domain) :: MD_fluid,MD_sigma
         type(dir_tree) :: DT
         type(sim_params) :: SP
         type(iter_solver_params) :: ISP_U,ISP_B,ISP_T,ISP_P,ISP_phi
         type(time_marching_params) :: TMP_U,TMP_B,TMP_T,coupled
         ! ********************** SMALL VARIABLES ***********************
         real(cp) :: Re,Ha,Gr,Fr,Pr,Ec,Rem
         logical :: finite_Rem,include_vacuum
         real(cp) :: tw,sig_local_over_sig_f

         ! ************************************************************** Parallel + directory + input parameters
         call omp_set_num_threads(12) ! Set number of openMP threads

         call init(DT,dir_target)  ! Initialize + make directory tree

         call init(SP)             ! Initializes simulation parameters

         ! Initializes solver parameters, dimensionless groups
         call readInputFile(SP,DT,Re,Ha,Gr,Fr,Pr,Ec,Rem,finite_Rem,&
         coupled,TMP_U,TMP_B,TMP_T,ISP_U,ISP_B,ISP_T,ISP_P,ISP_phi,tw,&
         sig_local_over_sig_f,include_vacuum)

         call init(nrg%ISP_T,ISP_T); call init(nrg%TMP,TMP_T)
         call init(mom%ISP_U,ISP_U); call init(mom%TMP,TMP_U)
         call init(ind%ISP_B,ISP_B); call init(ind%TMP,TMP_B)
         call init(mom%ISP_P,ISP_P)
         call init(ind%ISP_phi,ISP_phi)

         call print_version(); call export_version(str(DT%LDC))

         ! ************************************************************** Initialize mesh + domains
         if (SP%restart_all) then
           call import(mesh_mom,str(DT%restart),'mesh_mom')
           call import(mesh_ind,str(DT%restart),'mesh_ind')
           call import(MD_sigma ,str(DT%restart),'MD_sigma')
         else
           call mesh_generate(mesh_mom,mesh_ind,MD_sigma,DT,Ha,tw,include_vacuum)
           call export(mesh_mom,str(DT%restart),'mesh_mom')
           call export(mesh_ind,str(DT%restart),'mesh_ind')
           call export(MD_sigma ,str(DT%restart),'MD_sigma')
         endif
         call init(mesh_ind_interior,MD_sigma%m_R2)

         call initProps(mesh_mom);     call patch(mesh_mom)
         call initProps(mesh_ind);     call patch(mesh_ind)

         call init(MD_fluid,mesh_mom,mesh_ind) ! Domain,interior,exterior

         call initProps(MD_fluid%m_R1); call patch(MD_fluid%m_R1)
         call initProps(MD_fluid%m_R2); call patch(MD_fluid%m_R2)
         call initProps(MD_sigma%m_R1); call patch(MD_sigma%m_R1)
         call initProps(MD_sigma%m_R2); call patch(MD_sigma%m_R2)

         ! call print(MD_fluid,'MD_fluid')
         ! call print(MD_sigma,'MD_sigma')
         ! stop 'Done in MOONS.f90'

         ! ******************** EXPORT GRIDS **************************** Export mesh (to plot)
         if (SP%export_meshes) call export_mesh(mesh_mom,str(DT%meshes),'mesh_mom',1)
         if (SP%export_meshes) call export_mesh(MD_sigma%m_R2,str(DT%meshes),'mesh_MD_sigma',1)
         if (SP%export_meshes) call export_mesh(mesh_ind,str(DT%meshes),'mesh_ind',1)
         if (SP%stop_after_mesh_export) then
           stop 'Exported meshes. Turn off stop_after_mesh_export in sim_params.f90 to run sim.'
         endif

         ! Initialize energy,momentum,induction
         call init(mom,mesh_mom,SP,TMP_U,ISP_U,ISP_P,Re,Ha,Gr,Fr,DT)
         if (SP%solveEnergy) then
          call init(nrg,mesh_ind,SP,MD_fluid,TMP_T,ISP_T,Re,Pr,Ec,Ha,DT)
         endif
         if (SP%solveInduction) then
           call init(ind,mesh_ind,SP,MD_fluid,MD_sigma,include_vacuum,&
                     sig_local_over_sig_f,finite_Rem,Rem,TMP_B,ISP_B,ISP_phi,DT)
         endif

         ! Clean up constructor copies
         call delete(mesh_mom)
         call delete(mesh_ind)
         call delete(mesh_ind_interior)
         call delete(MD_fluid)
         call delete(MD_sigma)

         ! ********************* EXPORT RAW ICs *************************
           ! if (SP%export_ICs) call export_tec(nrg,DT)
           if (SP%export_ICs) call export_tec(ind,DT)
           if (SP%export_ICs) call export_tec(mom,DT,mom%temp_F)

         call print(mom%m)
         call print(ind%m)

         ! ******************** PREP TIME START/STOP ********************
         if (SP%stop_before_solve) then
           stop 'Exported ICs. Turn off stop_before_solve in sim_params.f90 to run sim.'
         endif
         if (.not.SP%post_process_only) call MHDSolver(nrg,mom,ind,DT,SP,coupled)

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

           ! if (SP%solveEnergy) then;    call export_tec(nrg,DT);   call export(nrg,DT); endif
           ! if (SP%solveInduction) then; call export_tec(ind,DT);   call export(ind,DT); endif
           ! if (SP%solveMomentum) then;  call export_tec(mom,DT);   call export(mom,DT); endif

           if (SP%export_analytic) call export_SH(mom%m,mom%U%x,Ha,0.0_cp,-1.0_cp,1,DT)
         ! else
           write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
           write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
           write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
         ! endif

         ! ******************* DELETE ALLOCATED DERIVED TYPES ***********
         ! if (SP%solveEnergy)    call export(nrg,DT)
         ! if (SP%solveInduction) call export(ind,DT)
         ! if (SP%solveMomentum)  call export(mom,DT)

         call delete(nrg)
         call delete(mom)
         call delete(ind)
         call delete(DT)

         call delete(coupled)
         call delete(TMP_U); call delete(TMP_B); call delete(TMP_T)
         call delete(ISP_U); call delete(ISP_B); call delete(ISP_T)
         call delete(ISP_P); call delete(ISP_phi)
       end subroutine

       end module
