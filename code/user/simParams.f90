       module simParams_mod
       implicit none
       logical :: restart_all               = .false.     ! restart sim (requires no code changes)
       logical :: stopBeforeSolve           = .false.     ! Just export ICs, do not run simulation
       logical :: post_process_only         = .true.      ! Skip solver loop and just post-process results

       logical :: exportGrids               = .false.     ! Export all Grids before starting simulation
       logical :: exportICs                 = .false.     ! Export Post-Processed ICs before starting simulation
       logical :: export_planar             = .false.     ! Export 2D data when N_cell = 1 along given direction
       logical :: quick_start               = .true.      ! Avoids exporting any large datasets before solve

       logical :: export_analytic           = .false.     ! Export analytic solutions (MOONS.f90)

       logical :: solveEnergy               = .false.     ! Solve energy equation
       logical :: solveMomentum             = .false.     ! Solve momentum equation
       logical :: solveInduction            = .true.      ! Solve induction equation

       integer :: solveTMethod              = 5           ! Refer to energy.f90
       integer :: solveUMethod              = 1           ! Refer to momentum.f90
       integer :: solveBMethod              = 4           ! Refer to induction.f90

       logical :: addJCrossB                = .false.     ! add JCrossB to momentum equation
       logical :: addBuoyancy               = .false.     ! add Buoyancy to momentum equation
       logical :: addGravity                = .false.     ! add Gravity to momentum equation
       
       end module