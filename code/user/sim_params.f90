     module sim_params_mod
     implicit none

     type sim_params
       logical :: restart_all

       logical :: post_process_only ! depricated
       logical :: stop_before_solve
       logical :: post_process
       logical :: skip_solver_loop
       logical :: stop_after_mesh_export

       logical :: export_analytic
       logical :: export_meshes
       logical :: export_mat_props
       logical :: export_cell_volume
       logical :: export_ICs
       logical :: export_planar
       logical :: export_symmetric

       logical :: solveEnergy
       logical :: solveMomentum
       logical :: solveInduction

       logical :: coupled_time_step

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

       SP%post_process_only         = .false.     ! Skip solver loop and just post-process results
       SP%post_process              = .true.      ! Skip solver loop and just post-process results
       SP%skip_solver_loop          = .false.     ! Skip solver loop
       SP%stop_before_solve         = .false.      ! Just export ICs, do not run simulation
       SP%stop_after_mesh_export    = .false.     ! 

       SP%export_analytic           = .false.     ! Export analytic solutions (MOONS.f90)
       SP%export_meshes             = .true.      ! Export all meshes before starting simulation
       SP%export_mat_props          = .false.      ! Export material properties before starting simulation
       SP%export_ICs                = .false.     ! Export Post-Processed ICs before starting simulation
       SP%export_cell_volume        = .false.     ! Export cell volumes for each mesh
       SP%export_planar             = .false.     ! Export 2D data when N_cell = 1 along given direction
       SP%export_symmetric          = .true.      ! 

       SP%coupled_time_step         = .true.      ! Ensures all time steps are equal to coupled%dt

       SP%solveEnergy               = .false.     ! Solve energy    equation
       SP%solveMomentum             = .true.      ! Solve momentum  equation
       SP%solveInduction            = .true.      ! Solve induction equation

       SP%restartT                  = .false.     ! restart T  field
       SP%restartU                  = .false.     ! restart U  field
       SP%restartB                  = .false.     ! restart B  field
       SP%restartB0                 = .false.     ! restart B0 field

       SP%solveTMethod              = 5           ! Refer to energy.f90
       SP%solveUMethod              = 1           ! Refer to momentum.f90
       SP%solveBMethod              = 3           ! Refer to induction.f90

       SP%addJCrossB                = .true.      ! add JCrossB      to momentum equation
       SP%add_Q2D_JCrossB           = .false.     ! add Q2D JCrossB  to momentum equation
       SP%addBuoyancy               = .false.     ! add Buoyancy     to momentum equation
       SP%addGravity                = .false.     ! add Gravity      to momentum equation
      end subroutine

     subroutine init_sim_params_copy(SP,SP_in)
       implicit none
       type(sim_params),intent(inout) :: SP
       type(sim_params),intent(in) :: SP_in
       SP%restart_all = SP_in%restart_all
       SP%stop_before_solve = SP_in%stop_before_solve
       SP%stop_after_mesh_export = SP_in%stop_after_mesh_export
       SP%post_process = SP_in%post_process
       SP%skip_solver_loop = SP_in%skip_solver_loop
       SP%post_process_only = SP_in%post_process_only
       SP%export_meshes = SP_in%export_meshes
       SP%export_mat_props = SP_in%export_mat_props
       SP%export_ICs = SP_in%export_ICs
       SP%export_planar = SP_in%export_planar
       SP%export_cell_volume = SP_in%export_cell_volume
       SP%export_analytic = SP_in%export_analytic
       SP%export_symmetric = SP_in%export_symmetric
       SP%coupled_time_step = SP_in%coupled_time_step
       SP%solveEnergy = SP_in%solveEnergy
       SP%solveMomentum = SP_in%solveMomentum
       SP%solveInduction = SP_in%solveInduction
       SP%solveTMethod = SP_in%solveTMethod
       SP%solveUMethod = SP_in%solveUMethod
       SP%solveBMethod = SP_in%solveBMethod
       SP%addJCrossB = SP_in%addJCrossB
       SP%addBuoyancy = SP_in%addBuoyancy
       SP%addGravity = SP_in%addGravity
       SP%add_Q2D_JCrossB = SP_in%add_Q2D_JCrossB
       SP%restartT = SP_in%restartT
       SP%restartU = SP_in%restartU
       SP%restartB = SP_in%restartB
       SP%restartB0 = SP_in%restartB0
      end subroutine

     end module