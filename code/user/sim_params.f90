     module sim_params_mod
     implicit none

     type sim_params
       logical :: restart_all
       logical :: stopBeforeSolve
       logical :: post_process_only

       logical :: export_meshes
       logical :: export_mat_props
       logical :: exportICs
       logical :: quick_start
       logical :: export_planar

       logical :: export_analytic

       logical :: solveEnergy
       logical :: solveMomentum
       logical :: solveInduction

       integer :: solveTMethod
       integer :: solveUMethod
       integer :: solveBMethod

       logical :: restartT
       logical :: restartU
       logical :: restartB
       logical :: restartB0

       logical :: addJCrossB
       logical :: addBuoyancy
       logical :: addGravity
       logical :: add_Q2D_JCrossB
     end type
     
     interface init;    module procedure init_sim_params;        end interface
     interface init;    module procedure init_sim_params_copy;   end interface

     contains

     subroutine init_sim_params(SP)
       implicit none
       type(sim_params),intent(inout) :: SP
       SP%restart_all               = .false.     ! restart sim (requires no code changes)
       SP%stopBeforeSolve           = .false.     ! Just export ICs, do not run simulation
       SP%post_process_only         = .false.      ! Skip solver loop and just post-process results

       SP%export_meshes             = .true.      ! Export all meshes before starting simulation
       SP%export_mat_props          = .true.      ! Export material properties before starting simulation
       SP%exportICs                 = .false.     ! Export Post-Processed ICs before starting simulation
       SP%quick_start               = .true.     ! Avoids exporting any large datasets before solve
       SP%export_planar             = .false.     ! Export 2D data when N_cell = 1 along given direction

       SP%export_analytic           = .false.     ! Export analytic solutions (MOONS.f90)

       SP%solveEnergy               = .false.     ! Solve energy    equation
       SP%solveMomentum             = .false.     ! Solve momentum  equation
       SP%solveInduction            = .true.      ! Solve induction equation

       SP%restartT                  = .false.     ! restart T  field
       SP%restartU                  = .false.     ! restart U  field
       SP%restartB                  = .false.     ! restart B  field
       SP%restartB0                 = .false.     ! restart B0 field

       SP%solveTMethod              = 5           ! Refer to energy.f90
       SP%solveUMethod              = 1           ! Refer to momentum.f90
       SP%solveBMethod              = 3           ! Refer to induction.f90

       SP%addJCrossB                = .false.     ! add JCrossB      to momentum equation
       SP%add_Q2D_JCrossB           = .false.     ! add Q2D JCrossB  to momentum equation
       SP%addBuoyancy               = .false.     ! add Buoyancy     to momentum equation
       SP%addGravity                = .false.     ! add Gravity      to momentum equation
      end subroutine

     subroutine init_sim_params_copy(SP_out,SP_in)
       implicit none
       type(sim_params),intent(inout) :: SP_out
       type(sim_params),intent(in) :: SP_in
       SP_out%restart_all = SP_in%restart_all
       SP_out%stopBeforeSolve = SP_in%stopBeforeSolve
       SP_out%post_process_only = SP_in%post_process_only
       SP_out%export_meshes = SP_in%export_meshes
       SP_out%export_mat_props = SP_in%export_mat_props
       SP_out%exportICs = SP_in%exportICs
       SP_out%export_planar = SP_in%export_planar
       SP_out%quick_start = SP_in%quick_start
       SP_out%export_analytic = SP_in%export_analytic
       SP_out%solveEnergy = SP_in%solveEnergy
       SP_out%solveMomentum = SP_in%solveMomentum
       SP_out%solveInduction = SP_in%solveInduction
       SP_out%solveTMethod = SP_in%solveTMethod
       SP_out%solveUMethod = SP_in%solveUMethod
       SP_out%solveBMethod = SP_in%solveBMethod
       SP_out%addJCrossB = SP_in%addJCrossB
       SP_out%addBuoyancy = SP_in%addBuoyancy
       SP_out%addGravity = SP_in%addGravity
       SP_out%add_Q2D_JCrossB = SP_in%add_Q2D_JCrossB
       SP_out%restartT = SP_in%restartT
       SP_out%restartU = SP_in%restartU
       SP_out%restartB = SP_in%restartB
       SP_out%restartB0 = SP_in%restartB0
      end subroutine

     end module