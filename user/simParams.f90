       module simParams_mod
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
       private :: cp

       ! ************************* GRID/ICs *************************
       logical :: exportGrids               = .false.   ! Export all Grids before starting simulation
       logical :: exportRawICs              = .false.   ! Export Raw ICs before starting simulation
       logical :: exportICs                 = .false.   ! Export Post-Processed ICs before starting simulation

       logical :: autoMatchBetas            = .true.    ! Auto match stretching at wall

       logical :: nonUniformGridFluid       = .true.    ! (T/F)
       logical :: nonUniformGridWall        = .true.    ! (T/F, F-> overrides wall thickness)
       logical :: overrideGeometryWarnings  = .false.

       ! ******************** PARALLELIZATION *******************
       ! Use the -fopenMP flag to parallelize the following:
       !     - myError.f90 (LnError3D,LnError3DUniform)
       !     - myDel.f90 (myDel) done
       !     - mySOR.f90 / myJacobi.f90 (Poisson loop)
       !     - ops_discrete.f90 (collocatedCross)
       !     - interpOps.f90 (interpO2)
       !     - ops_aux.f90 (collocatedMagnitude)
       !     - myTriSolver.f90 (applyTriSolver)

       ! ************************ T-FIELD ***********************
       logical :: solveEnergy = .false.
       logical :: restartT = .false.
       integer,parameter :: solveTMethod = 1
       !                                   1 : Explicit Euler

       ! ************************ U-FIELD ***********************
       logical :: solveMomentum = .true.
       logical :: restartU = .false.
       logical :: computeKU = .true.    ! Compute kinetic energy at each time step
       integer,parameter :: solveUMethod = 1
       !                                   1 : Explicit Euler
       !                                   2 : Semi-Implicit 3D ADI (Douglas)

       integer :: advectiveUFormulation = 1
       !                                  1 : Donor-Cell (conservative form)
       !                                  2 : Advective form
       !                                  3 : Upwind (not yet implemented)
       !                                  4 : Hybrid (not yet implemented)

       ! real(cp) :: lambdu = 0.5 ! Upwind blending parameter  ( 0 <= lambdu <= 1 ) Not yet implemented
       !                                                       pure         central
       !                                                      upwind       difference
       logical :: solveCoupled = .true. ! Change to addJCrossB
       ! logical :: addBuoyancy = .true. ! Not yet implemented
       ! logical :: addGravity = .true. ! Not yet implemented

       ! ************************ B-FIELD ***********************
       logical :: solveInduction = .true.
       logical :: restartB = .false.
       logical :: cleanB = .false.
       logical :: multiMaterials = .false.
       logical :: computeKB = .true.    ! Compute magnetic energy at each time step
       logical :: computeKB0 = .false.   ! Compute magnetic energy at each time step

       integer,parameter :: solveBMethod = 5
       !                                   1 : Low Rem (Poisson, assumes uniform properties)
       !                                   2 : Low Rem (Pseudo time step for uniform properties)
       !                                   3 : Low Rem (Pseudo time step)
       !                                   4 : Low Rem Constrained Transport (CT) Method
       !                                   5 : Finite Rem Constrained Transport (CT) Method
       !                                   6 : Low Rem Semi-Implicit 3D ADI (Douglas) (good for highly insulating walls)
       !                                   7 : Low Rem Multigrid (Poisson, assumes uniform properties)
       !                                   8 : Low Rem For Jack's Experiment (Current injection)

       ! More difficult parameters to have pre-defined cases for:
       integer,parameter :: preDefinedGeometry = 0
       !                                         0 : User-defined case (no override)
       !                                         1 : Square, hmin=-1/2, hmax = 1/2, tw = 0.0
       !                                         2 : Square, hmin=-1, hmax = 1, tw = 0.0
       !                                         3 : Square, hmin=-1/2, hmax = 1/2, tw = 0.1
       !                                         4 : Square, hmin=-1, hmax = 1, tw = 0.1
       !                                         5 : Duct, L=25, a=1/2, tw = 0.0
       !                                         6 : Duct, L=25, a=1, tw = 0.0
       !                                         7 : Duct, L=25, a=1/2, tw = 0.1
       !                                         8 : Duct, L=25, a=1, tw = 0.1
       ! ************************** MHD *************************

       ! ****************** BENCHMARK CASES  ********************
       ! ********** (OVERRIDES USER DEFINED SETUP) **************
       ! Setting benchmarkCase will define all parameters for the 
       ! simulation EXCEPT
       !      All other parameters in this file
       !      Number of cells in fluid domain (in griddata.f90)
       !      Number of cells in wall domain  (in griddata.f90)
       !      preDefined cases (this must be set along side benchmarkCase)
       ! Setting benchmarkCase will define
       !      Geometry,stretching factors,...,Re,Ha,Rem,...,dt,Nmax
       ! 

       integer,parameter :: benchmarkCase = 1003
       ! 
       ! 0-99-series (verification cases against exact solutions)
       ! 
       !    0 : LDC          , Re=400,Ha=0  (symmetry) Ni=even
       !    1 : LDC          , Re=400,Ha=0  (symmetry) Ni=odd
       ! 
       !    2 : LDC          , Re=400,Ha=0  (refinement in symmetry plane) N=32
       !    3 : LDC          , Re=400,Ha=0  (refinement in symmetry plane) N=64
       !    4 : LDC          , Re=400,Ha=0  (refinement in symmetry plane) N=128
       !    5 : LDC          , Re=400,Ha=0  (refinement in symmetry plane) N=256
       ! 
       !    6 : Duct Flow    , Re=400,Ha=0  (refinement) N=32
       !    7 : Duct Flow    , Re=400,Ha=0  (refinement) N=64
       !    8 : Duct Flow    , Re=400,Ha=0  (refinement) N=128
       !    9 : Duct Flow    , Re=400,Ha=0  (refinement) N=256
       ! 
       !    50 : LDC         , Re=2000,Ha=0 - Onset of turbulent flow
       !    51 : LDC         , Re=3200,Ha=0 - Turbulent flow
       ! 
       ! 100-series (Lid driven cavity flows)
       ! 
       !    100 : LDC , Re=400  , Ha=0    , S=1    (Guj / stella) uniform
       !    101 : LDC , Re=1000 , Ha=0    , S=1    (Guj / stella) non-uniform
       ! 
       !    102 : LDC , Re=100  , Ha=10   , S=1    (sergey)
       !    103 : LDC , Re=1000 , Ha=100  , S=1    (sergey)
       !    104 : LDC , Re=1000 , Ha=1000 , S=1    (sergey) - instabilities
       ! 
       !    105 : LDC , Re=100  , Ha=10   , S=1     (sergey)
       !    106 : LDC , Re=100  , Ha=10   , S=100   (sergey)
       !    107 : LDC , Re=100  , Ha=10   , S=1000  (sergey)
       !    108 : LDC , Re=100  , Ha=10   , S=10^6  (sergey)
       ! 
       !    109 : LDC , Re=100  , Ha=10   , S=1     (Sergey and Peter)
       ! 
       ! 
       ! 200-series (Duct flows)
       ! 
       !    200 : Duct Flow   , Re=100.0  , Ha=0   (test case)
       !    201 : Duct Flow   , Re=1000   , Ha=100 (mhd test case)
       ! 
       !    110 : FD Duct Flow  , Re=100  , Ha=500  , Rem=0    (Sergey/HIMAG/MOONS)
       ! 
       !    250 : Duct Flow   , Re=15574.07   , Ha=2900    (case B2)
       ! 
       ! 300-series (Cylinder driven cavity flows)
       ! 
       !    300 : CDC , Re=400
       ! 
       ! 400-series (Plasma disruption modeling)
       ! 
       !    400 : PD , Re=100   , Ha=10  (Sergey/MOONS/Peter)
       ! 
       ! 1000-series (Purely sequential)
       ! 
       !    1001: Case 1A, LDC, Ha = 10   Re = 100, tw = 0.1, sig_l = sig_w, Rem = 0, conducting walls except lid
       !                B            100
       !                C            1000
       !    1002: Case 2A, FD square duct flow, Ha = 500, Rem = 0, All walls insulating
       !                B                                          Hartmann walls conducting, tw = 0.01, sig_l = sig_w, side walls insulating
       !    1003: Case 3, Plasma disruption, Refer to powerpoint
       ! 
       !    1004: LDC by Salah - To produce Fig. 4, Fig. 5
       !    1005: No slip cavity - Jack's experiment
       ! 
       ! In order to make new benchmarkCases, prepare the following
       !    - MOONS.f90 (Number of steps, time step etc.)
       !    - griddata.f90 (geometry,stretching factors,N cells)
       !    - initializeSigmaMu.f90 (ICs)
       ! ----------------------------------------



       ! ************************ EXPORT ************************
       logical :: minimizePrintedOutput     = .true.    ! (T/F) Not yet implemented
       integer :: nskip_exportRaw            = 100000 ! Raw solution for restart (very expensive)
       integer :: nskip_export               = 100000 ! Processed solution for visualization (very expensive)
       integer :: nskip_exportTransient      = 50     ! Probed data (cheap)
       integer :: nskip_exportErrors         = 100    ! Divergences / Residuals (expensive)
       integer :: nskip_print                = 10     ! Printed data (cheap)
       ! integer :: transientExportXYZ         = 1      ! Component to export (1,2,3) = (x,y,z)

       end module