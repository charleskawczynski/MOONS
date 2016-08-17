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

       use init_Bfield_mod, only : restartB ! For restart
       use init_Ufield_mod, only : restartU ! For restart

       use simParams_mod
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
         ! ********************** SMALL VARIABLES ***********************
         real(cp) :: Re,Ha,Gr,Fr,Pr,Ec,Rem,dt_eng,dt_mom,dt_ind
         real(cp) :: tol_nrg,tol_mom,tol_PPE,tol_induction,tol_cleanB
         logical :: finite_Rem
         integer :: n_dt_start,n_step,n_dt_stop,N_mom,N_PPE,N_induction,N_cleanB,N_nrg
         integer :: un

         ! ************************************************************** Parallel + directory + input parameters
         call omp_set_num_threads(12) ! Set number of openMP threads

         call init(DT,dir_target)  ! Initialize + make directory tree

         call readInputFile(Re,Ha,Gr,Fr,Pr,Ec,Rem,finite_Rem,&
         dt_eng,dt_mom,dt_ind,n_dt_start,n_dt_stop,N_nrg,tol_nrg,N_mom,tol_mom,&
         N_PPE,tol_PPE,N_induction,tol_induction,N_cleanB,tol_cleanB)

         call print_version(); call export_version(str(DT%LDC))

         ! ************************************************************** Initialize mesh + domains
         if (restart_all) then
           call import(mesh_mom,str(DT%restart),'mesh_mom')
           call import(mesh_ind,str(DT%restart),'mesh_ind')
           call import(D_sigma,str(DT%restart),'D_sigma')
         else
           call mesh_generate(mesh_mom,mesh_ind,D_sigma)
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
         ! if (.not.quick_start) then
           if (exportGrids) call export_mesh(mesh_mom,str(DT%U),'mesh_mom',1)
           if (exportGrids) call export_mesh(mesh_ind,str(DT%B),'mesh_ind',1)
         ! endif

         ! Initialize energy,momentum,induction
         call init(mom,mesh_mom,N_mom,tol_mom,N_PPE,tol_PPE,dt_mom,Re,Ha,Gr,Fr,DT)
         if (solveEnergy) call init(nrg,mesh_ind,D_fluid,N_nrg,tol_nrg,dt_eng,Re,Pr,Ec,Ha,DT)
         if (solveInduction) then
           call init(ind,mesh_ind,D_fluid,D_sigma,finite_Rem,Rem,dt_ind,&
           N_induction,tol_induction,N_cleanB,tol_cleanB,DT)
         endif

         ! ********************* EXPORT RAW ICs *************************
         if (.not.quick_start) then
           ! if (exportICs) call export_tec(nrg,DT)
           if (exportICs) call export_tec(ind,DT)
           if (exportICs) call export_tec(mom,DT,mom%temp_F)
         endif

         call print(mesh_mom)
         call print(mesh_ind)

         ! ******************** PREP TIME START/STOP ********************
         if (restart_all) then
         un = openToRead(str(DT%restart),'n_dt_start,n_step,n_dt_stop')
         read(un,*) n_dt_start,n_step,n_dt_stop
         call closeAndMessage(un,'n_dt_start,n_step,n_dt_stop',str(DT%restart))
         else
         un = newAndOpen(str(DT%restart),'n_dt_start,n_step,n_dt_stop')
         write(un,*) n_dt_start,n_step,n_dt_stop
         call closeAndMessage(un,'n_dt_start,n_step,n_dt_stop',str(DT%restart))
         endif

         if (stopBeforeSolve) stop 'Exported ICs. Turn off stopAfterExportICs in simParams.f90 to run sim'
         if (.not.post_process_only) call MHDSolver(nrg,mom,ind,DT,n_dt_start,n_step,n_dt_stop)

         if (post_process_only) then
           write(*,*) ' *********************** POST PROCESSING ***********************'
           write(*,*) ' *********************** POST PROCESSING ***********************'
           write(*,*) ' *********************** POST PROCESSING ***********************'
           write(*,*) ' COMPUTING ENERGY BUDGETS: '
           if (solveMomentum.and.solveInduction) call compute_E_K_Budget(mom,ind%B,ind%B0,ind%J,ind%D_fluid,DT)
           write(*,*) '       KINETIC ENERGY BUDGET - COMPLETE'
           if (solveMomentum.and.solveInduction) call compute_E_M_budget(ind,mom%U,ind%D_fluid,DT)
           write(*,*) '       MAGNETIC ENERGY BUDGET - COMPLETE'

           if (export_analytic) call export_SH(mom%m,mom%U%x,Ha,0.0_cp,-1.0_cp,1,DT)
         else
           write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
           write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
           write(*,*) ' ******************** COMPUTATIONS COMPLETE ********************'
         endif

         ! ******************* DELETE ALLOCATED DERIVED TYPES ***********
         ! if (solveEnergy)    call export(nrg,DT)
         ! if (solveInduction) call export(ind,DT)
         ! if (solveMomentum)  call export(mom,DT)

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
