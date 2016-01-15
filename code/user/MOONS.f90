       module MOONS_mod
       use simParams_mod
       use IO_tools_mod
       use IO_SF_mod
       use IO_VF_mod
       use IO_auxiliary_mod
       use version_mod
       use grid_mod
       use mesh_mod
       use domain_mod

       use mesh_generate_mod
       use ops_embedExtract_mod
       use ops_interp_mod
       use rundata_mod
       use VF_mod

       use init_Bfield_mod ! For restart
       use init_Ufield_mod ! For restart

       use solverSettings_mod

       use energy_mod
       use momentum_mod
       use induction_mod
       use induction_aux_mod
       use inputFile_mod

       use MHDSolver_mod
       use omp_lib

       implicit none

       private

       public :: MOONS

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

       subroutine create_directory(dir)
         implicit none
         character(len=*),intent(in) :: dir
         write(*,*) 'MOONS output directory = ',dir
         call rmDir(dir) ! Delete directory (does not work yet...)
         call makeDir(dir)
         call makeDir(dir,'Tfield')
         call makeDir(dir,'Ufield')
         call makeDir(dir,'Ufield','\transient')
         call makeDir(dir,'Ufield','\energy')
         call makeDir(dir,'Bfield')
         call makeDir(dir,'Bfield','\transient')
         call makeDir(dir,'Bfield','\energy')
         call makeDir(dir,'Jfield')
         call makeDir(dir,'Jfield','\transient')
         call makeDir(dir,'Jfield','\energy')
         call makeDir(dir,'material')
         call makeDir(dir,'parameters')
      end subroutine

       subroutine MOONS(dir)
         implicit none
         character(len=*),intent(in) :: dir ! Output directory
         ! ********************** BIG VARIABLES *************************
         type(momentum) :: mom
         type(induction) :: ind
         type(energy) :: nrg
         type(mesh) :: mesh_mom,mesh_ind
         ! ********************** MEDIUM VARIABLES **********************
         type(rundata) :: rd
         type(solverSettings) :: ss_MHD
         type(domain) :: D_fluid,D_sigma
         ! ********************** SMALL VARIABLES ***********************
         real(cp) :: Re,Ha,Gr,Fr,Pr,Ec,Rem
         real(cp) :: tol_mom,tol_PPE,tol_induction,tol_cleanB
         logical :: finite_Rem
         real(cp) :: dt_eng,dt_mom,dt_ind
         integer :: NmaxMHD,N_mom,N_PPE,N_induction,N_cleanB,N_energy
         integer :: n_mhd
         ! **************************************************************

         call omp_set_num_threads(12) ! Set number of openMP threads

         call readInputFile(Re,Ha,Gr,Fr,Pr,Ec,Rem,finite_Rem,&
         dt_eng,dt_mom,dt_ind,NmaxMHD,N_energy,N_mom,tol_mom,&
         N_PPE,tol_PPE,N_induction,tol_induction,N_cleanB,tol_cleanB)

         call create_directory(dir)

         call printVersion()
         call exportVersion(dir)

         if (solveInduction) then
           select case(solveBMethod)
           case(1:4)
             if (N_induction.lt.1) stop 'Error: N_induction must be larger than 1 for low Rem cases'
           case default
           end select
         endif

         ! **************************************************************
         ! Initialize all grids
         call mesh_generate(mesh_mom,mesh_ind,D_sigma)

         call initProps(mesh_mom);     call patch(mesh_mom)
         call initProps(mesh_ind);     call patch(mesh_ind)

         call init(D_fluid,mesh_mom,mesh_ind) ! Domain,interior,exterior

         ! ******************** EXPORT GRIDS ****************************
         if (exportGrids) call export_mesh(mesh_mom,dir//'Ufield/','mesh_mom',1)
         if (exportGrids) call export_mesh(mesh_ind,dir//'Bfield/','mesh_ind',1)

         ! Initialize energy,momentum,induction
         if (solveEnergy)  call init(nrg,mesh_ind,D_fluid,N_energy,dt_eng,Re,Pr,Ec,Ha,dir)
         call init(mom,mesh_mom,N_mom,tol_mom,N_PPE,tol_PPE,dt_mom,Re,Ha,Gr,Fr,dir)
         if (solveInduction) then
           call init(ind,mesh_ind,D_fluid,D_sigma,finite_Rem,Rem,dt_ind,&
           N_induction,tol_induction,N_cleanB,tol_cleanB,dir)
         endif

         ! ********************* EXPORT RAW ICs *************************
         ! if (exportRawICs) call exportRaw(nrg,nrg%m,dir)
         if (exportRawICs) call export(mom,mom%m,mom%temp_F,dir)
         ! if (exportRawICs) call exportRaw(ind,ind%m,dir)

         ! ********************* EXPORT ICs *****************************
         ! if (exportICs) call export(mom,mom%m,dir)
         ! if (exportICs) call export(nrg,nrg%m,dir)
         ! if (exportICs) call export(ind,ind%m,dir)

         ! ****************** INITIALIZE RUNDATA ************************
         ! rundata is severely outdated and does not exist in the solver.
         ! It's only use now is to export solveBMethod and addJCrossB and that's it.
         call setRunData(rd,dt_mom,dt_ind,Re,Ha,Rem,&
          mom%U%x%RF(1)%f,mom%U%y%RF(1)%f,mom%U%z%RF(1)%f,&
          mesh_mom%g(1),mesh_ind%g(1),addJCrossB,solveBMethod)

         ! *************** CHECK IF CONDITIONS ARE OK *******************
         call printRundata(rd)
         call exportRundata(rd,dir)
         call print(mom%m)
         ! call print(ind%m)

         ! if (exportRawICs) then
         !   if (solveMomentum)  call exportRaw(mom,mom%m,dir)
         !   if (solveEnergy)    call exportRaw(nrg,nrg%m,dir)
         !   if (solveInduction) call embedVelocity(ind,mom%U,mom%m)
         !   if (solveInduction) call exportRaw(ind,ind%m,dir)
         ! endif
         ! if (exportICs) then
         !   if (solveMomentum)  call export(mom,mom%m,dir)
         !   if (solveEnergy)    call export(nrg,nrg%m,dir)
         !   if (solveInduction) call embedVelocity(ind,mom%U,mom%m)
         !   if (solveInduction) call export(ind,ind%m,dir)
         ! endif

         if (stopAfterExportICs) then
           stop 'Exported ICs. Turn off stopAfterExportICs in simParams.f90 to run sim'
         endif

         ! call checkGrid(gd)

         write(*,*) ''
         write(*,*) 'Press enter if these parameters are okay.'
         write(*,*) ''

         ! ********************** PREP LOOP ******************************
         ! This is done in both MOONS and MHDSolver, need to fix this..
         ! if (restartU.and.(.not.solveMomentum)) n_mhd = mom%nstep + ind%nstep
         ! if (restartB.and.(.not.solveInduction)) n_mhd = mom%nstep + ind%nstep

         ! n_mhd = maxval(/mom%nstep,ind%nstep/) ! What if U = fixed, and B is being solved?

         n_mhd = 0 ! Only counts MHD loop, no data is plotted vs MHD step, only n_mom,n_ind,n_nrg

         ! n_mhd = maxval(/mom%nstep,ind%nstep,nrg%nstep/)
         ! if (restartU.or.restartB) then
         !   call readLastStepFromFile(n_mhd,dir//'parameters/','n_mhd')
         !   n_mhd = n_mhd + 1
         ! else; n_mhd = 0
         ! endif
         ! n_mhd = 0

         call writeSwitchToFile(.true.,dir//'parameters/','killSwitch')
         call writeSwitchToFile(.false.,dir//'parameters/','exportNow')

         ! ******************* SET MHD SOLVER SETTINGS *******************
         call init(ss_MHD)

         call setMaxIterations(ss_MHD,n_mhd+NmaxMHD)
         call setIteration(ss_MHD,n_mhd)

         if (restartU.or.restartB) then
         call readLastStepFromFile(n_mhd,dir//'parameters/','n_mhd')
         else; n_mhd = 0
         endif

         ! ********************* SET B SOLVER SETTINGS *******************

         call MHDSolver(nrg,mom,ind,ss_MHD,dir)
         ! call export(mom,mom%m,dir)
         ! call export(ind,ind%m,dir)

         ! ******************* DELETE ALLOCATED DERIVED TYPES ***********

         call delete(ind)
         call delete(mom)

         call delete(mesh_mom)
         call delete(mesh_ind)
       end subroutine

       end module
