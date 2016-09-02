       module MOONS_mod
       use current_precision_mod
       use IO_tools_mod
       use IO_SF_mod
       use IO_VF_mod
       use IO_auxiliary_mod

       use version_mod
       use mesh_mod
       use domain_mod
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
         type(domain) :: D_fluid,D_sigma
         type(dir_tree) :: DT
         type(sim_params) :: SP
         ! ********************** SMALL VARIABLES ***********************
         type(iter_solver_params) :: ISP_U,ISP_B,ISP_T,ISP_P,ISP_phi
         type(time_marching_params) :: TMP_U,TMP_B,TMP_T,coupled
         real(cp) :: Re,Ha,Gr,Fr,Pr,Ec,Rem
         real(cp) :: dt_eng,dt_mom,dt_ind
         real(cp) :: tol_nrg,tol_mom,tol_PPE,tol_induction,tol_cleanB
         integer :: n_dt_start,n_step,n_dt_stop,N_mom,N_PPE,N_induction,N_cleanB,N_nrg
         logical :: finite_Rem
         integer :: un
         logical :: include_vacuum
         real(cp) :: tw,sig_local_over_sig_f

         ! ************************************************************** Parallel + directory + input parameters
         call omp_set_num_threads(12) ! Set number of openMP threads

         call init(DT,dir_target)  ! Initialize + make directory tree

         call init(SP)             ! Initializes simulation parameters
         call readInputFile(Re,Ha,Gr,Fr,Pr,Ec,Rem,finite_Rem,&
         coupled,TMP_U,TMP_B,TMP_T,ISP_U,ISP_B,ISP_T,ISP_P,ISP_phi,tw,&
         sig_local_over_sig_f,include_vacuum)

         call print_version(); call export_version(str(DT%LDC))

         ! ************************************************************** Initialize mesh + domains
         if (SP%restart_all) then
           call import(mesh_mom,str(DT%restart),'mesh_mom')
           call import(mesh_ind,str(DT%restart),'mesh_ind')
           call import(D_sigma,str(DT%restart),'D_sigma')
         else
           call mesh_generate(mesh_mom,mesh_ind,D_sigma,Ha,tw,include_vacuum)
           call export(mesh_mom,str(DT%restart),'mesh_mom')
           call export(mesh_ind,str(DT%restart),'mesh_ind')
           call export(D_sigma,str(DT%restart),'D_sigma')
         endif

         call init(mesh_ind_interior,D_sigma%m_tot)
         call initProps(mesh_mom);     call patch(mesh_mom)
         call initProps(mesh_ind);     call patch(mesh_ind)

         call init(D_fluid,mesh_mom,mesh_ind) ! Domain,interior,exterior

         call initProps(D_fluid%m_in);   call patch(D_fluid%m_in)
         call initProps(D_fluid%m_tot);  call patch(D_fluid%m_tot)
         call initProps(D_sigma%m_in);   call patch(D_sigma%m_in)
         call initProps(D_sigma%m_tot);  call patch(D_sigma%m_tot)

         ! ******************** EXPORT GRIDS **************************** Export mesh (to plot)
         ! if (.not.SP%quick_start) then
           if (SP%exportGrids) call export_mesh(mesh_mom,str(DT%U),'mesh_mom',1)
           if (SP%exportGrids) call export_mesh(mesh_ind,str(DT%B),'mesh_ind',1)
         ! endif

         ! Initialize energy,momentum,induction
         call init(mom,mesh_mom,SP,TMP_U,ISP_U,ISP_P,Re,Ha,Gr,Fr,DT)
         if (SP%solveEnergy) then
          call init(nrg,mesh_ind,SP,D_fluid,TMP_T,ISP_T,Re,Pr,Ec,Ha,DT)
         endif
         if (SP%solveInduction) then
           call init(ind,mesh_ind,SP,D_fluid,D_sigma,include_vacuum,&
                     sig_local_over_sig_f,finite_Rem,Rem,TMP_B,ISP_B,ISP_phi,DT)
         endif

         ! ********************* EXPORT RAW ICs *************************
         if (.not.SP%quick_start) then
           ! if (SP%exportICs) call export_tec(nrg,DT)
           if (SP%exportICs) call export_tec(ind,DT)
           if (SP%exportICs) call export_tec(mom,DT,mom%temp_F)
         endif

         call print(mesh_mom)
         call print(mesh_ind)

         ! ******************** PREP TIME START/STOP ********************
         if (SP%restart_all) then; call import(coupled,openToRead(str(DT%restart),'TMP_coupled'))
         else;                     call export(coupled,newAndOpen(str(DT%restart),'TMP_coupled'))
         endif

         if (SP%stopBeforeSolve) stop 'Exported ICs. Turn off stopAfterExportICs in simParams.f90 to run sim'
         if (.not.SP%post_process_only) call MHDSolver(nrg,mom,ind,DT,SP,coupled)

         ! if (post_process_only) then
           write(*,*) ' *********************** POST PROCESSING ***********************'
           write(*,*) ' *********************** POST PROCESSING ***********************'
           write(*,*) ' *********************** POST PROCESSING ***********************'
           write(*,*) ' COMPUTING VORTICITY-STREAMFUNCTION:'
           call export_vorticity_streamfunction(mom%U,mom%m,DT)
           ! if (solveMomentum.and.solveInduction) then
             write(*,*) ' COMPUTING ENERGY BUDGETS:'
             write(*,*) '       KINETIC ENERGY BUDGET - STARTED'
             call compute_E_K_Budget(mom,ind%B,ind%B0,ind%J,ind%D_fluid,ind%Rem,DT)
             write(*,*) '       KINETIC ENERGY BUDGET - COMPLETE'
             write(*,*) '       MAGNETIC ENERGY BUDGET - STARTED'
             call compute_E_M_budget(ind,mom%U,ind%D_fluid,mom%Re,mom%Ha,DT)
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
         call delete(D_fluid)
         call delete(D_sigma)
         call delete(DT)

         call delete(mesh_mom)
         call delete(mesh_ind)
         call delete(mesh_ind_interior)
       end subroutine

       end module
