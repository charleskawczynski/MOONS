REM This is a comment...wierd.
REM Variables are clobbered (destroyed if existing)
REM upond setting them, to check if a variable is already defined:

REM It seems that without variables, compilation works fine,
REM but with variables, the same error manifests itself.


@echo off set var=testing 1 2 3
echo The variable is "%var%"

SET SRCDIR=C:/Users/charl/Documents/GitHub/MOONS/code
SET TARDIR=C:/Users/charl/Documents/MOONS/MOONS5
REM gfortran -fopenmp -g -Wall -Wextra -pedantic -fimplicit-none -fbacktrace -cpp -fcheck=all -Wuninitialized^
gfortran -fopenmp -g -fimplicit-none -cpp ^
 %SRCDIR%/globals/current_precision.f90^
 %SRCDIR%/string/string.f90^
 %SRCDIR%/math/isnan.f90^
 %SRCDIR%/IO/IO_tools.f90^
 %SRCDIR%/IO/exp_Tecplot_Zone.f90^
 %SRCDIR%/IO/exp_Tecplot_Header.f90^
 %SRCDIR%/IO/IO_auxiliary.f90^
 %SRCDIR%/directory/draw_DT.f90^
 %SRCDIR%/directory/dir_tree.f90^
 %SRCDIR%/sparse/array.f90^
 %SRCDIR%/sparse/sparse.f90^
 %SRCDIR%/sparse/triDiag.f90^
 %SRCDIR%/BCs/face_edge_corner_indexing.f90^
 %SRCDIR%/stitches/stitch.f90^
 %SRCDIR%/stitches/stitch_face.f90^
 %SRCDIR%/stitches/stitch_edge.f90^
 %SRCDIR%/stitches/stitch_corner.f90^
 %SRCDIR%/coordinates/coordinates.f90^
 %SRCDIR%/grid/grid.f90^
 %SRCDIR%/grid/grid_distribution_funcs.f90^
 %SRCDIR%/grid/grid_genHelper.f90^
 %SRCDIR%/grid/grid_stretchParamMatch.f90^
 %SRCDIR%/grid/grid_init.f90^
 %SRCDIR%/grid/grid_extend.f90^
 %SRCDIR%/grid/grid_connect.f90^
 %SRCDIR%/grid/grid_generate.f90^
 %SRCDIR%/BCs/bctype.f90^
 %SRCDIR%/BCs/corner.f90^
 %SRCDIR%/BCs/edge.f90^
 %SRCDIR%/BCs/face.f90^
 %SRCDIR%/BCs/BCs.f90^
 %SRCDIR%/fields/RF.f90^
 %SRCDIR%/mesh/mesh.f90^
 %SRCDIR%/user/simParams.f90^
 %SRCDIR%/grid/subdomain.f90^
 %SRCDIR%/grid/domain.f90^
 %SRCDIR%/mesh/mesh_simple_geometries.f90^
 %SRCDIR%/mesh/mesh_complex_geometries.f90^
 %SRCDIR%/mesh/mesh_generate.f90^
 %SRCDIR%/fields/SF.f90^
 %SRCDIR%/fields/VF.f90^
 %SRCDIR%/fields/TF.f90^
 %SRCDIR%/fields/index_mapping.f90^
 %SRCDIR%/IO/export_raw.f90^
 %SRCDIR%/IO/export_g.f90^
 %SRCDIR%/IO/export_SF.f90^
 %SRCDIR%/IO/export_VF.f90^
 %SRCDIR%/IO/import_raw.f90^
 %SRCDIR%/IO/import_g.f90^
 %SRCDIR%/IO/import_SF.f90^
 %SRCDIR%/IO/import_VF.f90^
 %SRCDIR%/IO/IO_SF.f90^
 %SRCDIR%/IO/IO_VF.f90^
 %SRCDIR%/version/version.f90^
 %SRCDIR%/clock/clock.f90^
 %SRCDIR%/clock/stop_clock.f90^
 %SRCDIR%/ops/ops_norms.f90^
 %SRCDIR%/norms/norms.f90^
 %SRCDIR%/stitches/apply_stitches_faces.f90^
 %SRCDIR%/stitches/apply_stitches_edges.f90^
 %SRCDIR%/stitches/apply_stitches_corners.f90^
 %SRCDIR%/stitches/apply_stitches.f90^
 %SRCDIR%/ops/ops_fft.f90^
 %SRCDIR%/ops/ops_dct.f90^
 %SRCDIR%/ops/ops_idct.f90^
 %SRCDIR%/stencils/stencils.f90^
 %SRCDIR%/ops/ops_del.f90^
 %SRCDIR%/ops/ops_embedExtract.f90^
 %SRCDIR%/ops/ops_BEM.f90^
 %SRCDIR%/BCs/check_BCs.f90^
 %SRCDIR%/BCs/apply_BCs_faces.f90^
 %SRCDIR%/BCs/apply_BCs_faces_implicit.f90^
 %SRCDIR%/BCs/apply_BCs_edges_implicit.f90^
 %SRCDIR%/BCs/apply_BCs_edges.f90^
 %SRCDIR%/BCs/apply_BCs_corners.f90^
 %SRCDIR%/BCs/apply_BCs.f90^
 %SRCDIR%/BCs/apply_BCs_implicit.f90^
 %SRCDIR%/ops/ops_interp.f90^
 %SRCDIR%/ops/ops_aux.f90^
 %SRCDIR%/ops/ops_discrete.f90^
 %SRCDIR%/ops/ops_advect.f90^
 %SRCDIR%/IO/export_raw_processed.f90^
 %SRCDIR%/probes/probe_transient.f90^
 %SRCDIR%/probes/probe_base.f90^
 %SRCDIR%/probes/probe_derived.f90^
 %SRCDIR%/solvers/matrix_free_params.f90^
 %SRCDIR%/solvers/matrix_free_operators.f90^
 %SRCDIR%/solvers/preconditioners.f90^
 %SRCDIR%/solvers/AB2.f90^
 %SRCDIR%/solvers/compute_energy.f90^
 %SRCDIR%/sparse/matrix.f90^
 %SRCDIR%/solvers/FFT/FFT_poisson.f90^
 %SRCDIR%/solvers/Jacobi/Jacobi_solver.f90^
 %SRCDIR%/solvers/Jacobi/Jacobi.f90^
 %SRCDIR%/solvers/GS/GS_poisson.f90^
 %SRCDIR%/solvers/SOR/SOR.f90^
 %SRCDIR%/solvers/PSE/PSE_solver.f90^
 %SRCDIR%/solvers/PSE/PSE.f90^
 %SRCDIR%/solvers/PCG/PCG_aux.f90^
 %SRCDIR%/solvers/PCG/PCG_solver.f90^
 %SRCDIR%/solvers/PCG/PCG.f90^
 %SRCDIR%/solvers/energy/init_TBCs.f90^
 %SRCDIR%/solvers/energy/init_Tfield.f90^
 %SRCDIR%/solvers/energy/init_K.f90^
 %SRCDIR%/solvers/energy/energy_aux.f90^
 %SRCDIR%/solvers/energy/energy_solver.f90^
 %SRCDIR%/solvers/energy/energy.f90^
 %SRCDIR%/solvers/momentum/profile_funcs.f90^
 %SRCDIR%/solvers/momentum/init_UBCs.f90^
 %SRCDIR%/solvers/momentum/init_PBCs.f90^
 %SRCDIR%/solvers/momentum/init_Ufield.f90^
 %SRCDIR%/solvers/momentum/init_Pfield.f90^
 %SRCDIR%/solvers/momentum/E_K_budget_terms.f90^
 %SRCDIR%/solvers/momentum/E_K_budget.f90^
 %SRCDIR%/solvers/momentum/momentum_aux.f90^
 %SRCDIR%/solvers/momentum/momentum_solver.f90^
 %SRCDIR%/solvers/momentum/momentum.f90^
 %SRCDIR%/solvers/induction/E_M_budget_terms.f90^
 %SRCDIR%/solvers/induction/E_M_budget.f90^
 %SRCDIR%/solvers/induction/init_phiBCs.f90^
 %SRCDIR%/solvers/induction/init_BBCs.f90^
 %SRCDIR%/solvers/induction/init_Bfield.f90^
 %SRCDIR%/solvers/induction/init_Sigma.f90^
 %SRCDIR%/solvers/induction/induction_aux.f90^
 %SRCDIR%/solvers/induction/induction_solver.f90^
 %SRCDIR%/solvers/induction/induction.f90^
 %SRCDIR%/solvers/MHDSolver.f90^
 %SRCDIR%/user/inputFile.f90^
 %SRCDIR%/user/MOONS.f90^
 parametricStudy.f90^
 -o main.exe
del *.mod