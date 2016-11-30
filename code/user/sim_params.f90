     module sim_params_mod
     use current_precision_mod
     implicit none

     type sim_params
       logical :: restart_all

       logical :: post_process_only        ! depricated
       logical :: post_process
       logical :: skip_solver_loop
       logical :: stop_before_solve
       logical :: stop_after_mesh_export

       logical :: export_analytic
       logical :: export_meshes
       logical :: export_mat_props
       logical :: export_cell_volume
       logical :: export_ICs
       logical :: export_planar
       logical :: export_symmetric
       logical :: export_mesh_block
       logical :: export_soln_only

       logical :: solveEnergy
       logical :: solveMomentum
       logical :: solveInduction
       logical :: matrix_based

       logical :: dynamic_refinement
       integer :: n_max_refinements
       integer :: n_history
       real(cp) :: SS_tol
       real(cp) :: SS_tol_final

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
       SP%restart_all            = .false.            ! restart sim (requires no code changes)

       SP%post_process_only      = .false.            ! Skip solver loop and just post-process results
       SP%post_process           = .true.             ! Skip solver loop and just post-process results
       SP%skip_solver_loop       = .false.            ! Skip solver loop
       SP%stop_before_solve      = .false.             ! Just export ICs, do not run simulation
       SP%stop_after_mesh_export = .false.            !

       SP%export_analytic        = .false.            ! Export analytic solutions (MOONS.f90)
       SP%export_meshes          = .true.             ! Export all meshes before starting simulation
       SP%export_mat_props       = .false.            ! Export material properties before starting simulation
       SP%export_ICs             = .false.            ! Export Post-Processed ICs before starting simulation
       SP%export_cell_volume     = .false.            ! Export cell volumes for each mesh
       SP%export_planar          = .false.            ! Export 2D data when N_cell = 1 along given direction
       SP%export_symmetric       = .false.             !
       SP%export_mesh_block      = .false.            ! Export mesh blocks to FECs
       SP%export_soln_only       = .true.             ! Export mesh blocks to FECs

       SP%coupled_time_step      = .true.             ! Ensures all time steps are equal to coupled%dt

       SP%solveEnergy            = .false.            ! Solve energy    equation
       SP%solveMomentum          = .true.             ! Solve momentum  equation
       SP%solveInduction         = .true.             ! Solve induction equation
       SP%matrix_based           = .false.            ! Solve induction equation

       SP%dynamic_refinement     = .false.             ! Perform dynamic mesh refinement
       SP%n_max_refinements      = 2                  ! Maximum number of mesh refinements after SS reached
       SP%n_history              = 2                  ! number of points to check for SS
       SP%SS_tol                 = 10.0_cp**(-1.0_cp) ! steady state tolerance
       SP%SS_tol_final           = 10.0_cp**(-3.0_cp) ! steady state tolerance at finest mesh

       SP%restartT               = .false.            ! restart T  field
       SP%restartU               = .false.            ! restart U  field
       SP%restartB               = .false.            ! restart B  field
       SP%restartB0              = .false.            ! restart B0 field

       SP%solveTMethod           = 5                  ! Refer to energy.f90
       SP%solveUMethod           = 1                  ! Refer to momentum.f90
       SP%solveBMethod           = 2                  ! Refer to induction.f90

       SP%addJCrossB             = .true.             ! add JCrossB      to momentum equation
       SP%add_Q2D_JCrossB        = .false.            ! add Q2D JCrossB  to momentum equation
       SP%addBuoyancy            = .false.            ! add Buoyancy     to momentum equation
       SP%addGravity             = .false.            ! add Gravity      to momentum equation
      end subroutine

     subroutine init_sim_params_copy(SP,SP_in)
       implicit none
       type(sim_params),intent(inout) :: SP
       type(sim_params),intent(in) :: SP_in
       SP%restart_all            = SP_in%restart_all
       SP%stop_before_solve      = SP_in%stop_before_solve
       SP%stop_after_mesh_export = SP_in%stop_after_mesh_export
       SP%post_process           = SP_in%post_process
       SP%skip_solver_loop       = SP_in%skip_solver_loop
       SP%post_process_only      = SP_in%post_process_only
       SP%export_meshes          = SP_in%export_meshes
       SP%export_mat_props       = SP_in%export_mat_props
       SP%export_ICs             = SP_in%export_ICs
       SP%export_planar          = SP_in%export_planar
       SP%export_cell_volume     = SP_in%export_cell_volume
       SP%export_analytic        = SP_in%export_analytic
       SP%export_symmetric       = SP_in%export_symmetric
       SP%export_mesh_block      = SP_in%export_mesh_block
       SP%export_soln_only       = SP_in%export_soln_only
       SP%coupled_time_step      = SP_in%coupled_time_step
       SP%solveEnergy            = SP_in%solveEnergy
       SP%solveMomentum          = SP_in%solveMomentum
       SP%solveInduction         = SP_in%solveInduction
       SP%matrix_based           = SP_in%matrix_based
       SP%dynamic_refinement     = SP_in%dynamic_refinement
       SP%n_max_refinements      = SP_in%n_max_refinements
       SP%n_history              = SP_in%n_history
       SP%SS_tol                 = SP_in%SS_tol
       SP%SS_tol_final           = SP_in%SS_tol_final
       SP%solveTMethod           = SP_in%solveTMethod
       SP%solveUMethod           = SP_in%solveUMethod
       SP%solveBMethod           = SP_in%solveBMethod
       SP%addJCrossB             = SP_in%addJCrossB
       SP%addBuoyancy            = SP_in%addBuoyancy
       SP%addGravity             = SP_in%addGravity
       SP%add_Q2D_JCrossB        = SP_in%add_Q2D_JCrossB
       SP%restartT               = SP_in%restartT
       SP%restartU               = SP_in%restartU
       SP%restartB               = SP_in%restartB
       SP%restartB0              = SP_in%restartB0
      end subroutine

     end module