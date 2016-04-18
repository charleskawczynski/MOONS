gfortran -fopenmp -g -Wall -Wextra -pedantic -fimplicit-none -fbacktrace -cpp -fcheck=all -Wuninitialized^
 code/globals/current_precision.f90^
 code/string/string.f90^
 code/math/isnan.f90^
 code/IO/IO_tools.f90^
 code/IO/exp_Tecplot_Zone.f90^
 code/IO/exp_Tecplot_Header.f90^
 code/IO/IO_auxiliary.f90^
 code/directory/draw_DT.f90^
 code/directory/dir_tree.f90^
 code/sparse/array.f90^
 code/sparse/sparse.f90^
 code/sparse/triDiag.f90^
 code/BCs/face_edge_corner_indexing.f90^
 code/stitches/stitch.f90^
 code/stitches/stitch_face.f90^
 code/stitches/stitch_edge.f90^
 code/stitches/stitch_corner.f90^
 code/coordinates/coordinates.f90^
 code/grid/grid.f90^
 code/grid/grid_distribution_funcs.f90^
 code/grid/grid_genHelper.f90^
 code/grid/grid_stretchParamMatch.f90^
 code/grid/grid_init.f90^
 code/grid/grid_extend.f90^
 code/grid/grid_connect.f90^
 code/grid/grid_generate.f90^
 code/BCs/bctype.f90^
 code/BCs/corner.f90^
 code/BCs/edge.f90^
 code/BCs/face.f90^
 code/BCs/BCs.f90^
 code/fields/RF.f90^
 code/mesh/mesh.f90^
 code/user/simParams.f90^
 code/grid/subdomain.f90^
 code/grid/domain.f90^
 code/mesh/mesh_simple_geometries.f90^
 code/mesh/mesh_complex_geometries.f90^
 code/mesh/mesh_generate.f90^
 code/fields/SF.f90^
 code/fields/VF.f90^
 code/fields/TF.f90^
 code/fields/index_mapping.f90^
 code/IO/export_raw.f90^
 code/IO/export_g.f90^
 code/IO/export_SF.f90^
 code/IO/export_VF.f90^
 code/IO/import_raw.f90^
 code/IO/import_g.f90^
 code/IO/import_SF.f90^
 code/IO/import_VF.f90^
 code/IO/IO_SF.f90^
 code/IO/IO_VF.f90^
 code/version/version.f90^
 code/clock/clock.f90^
 code/clock/stop_clock.f90^
 code/ops/ops_norms.f90^
 code/norms/norms.f90^
 code/stitches/apply_stitches_faces.f90^
 code/stitches/apply_stitches_edges.f90^
 code/stitches/apply_stitches_corners.f90^
 code/stitches/apply_stitches.f90^
 code/ops/ops_fft.f90^
 code/ops/ops_dct.f90^
 code/ops/ops_idct.f90^
 code/stencils/stencils.f90^
 code/ops/ops_del.f90^
 code/ops/ops_embedExtract.f90^
 code/ops/ops_BEM.f90^
 code/BCs/check_BCs.f90^
 code/BCs/apply_BCs_faces.f90^
 code/BCs/apply_BCs_faces_implicit.f90^
 code/BCs/apply_BCs_edges_implicit.f90^
 code/BCs/apply_BCs_edges.f90^
 code/BCs/apply_BCs_corners.f90^
 code/BCs/apply_BCs.f90^
 code/BCs/apply_BCs_implicit.f90^
 code/ops/ops_interp.f90^
 code/ops/ops_aux.f90^
 code/ops/ops_discrete.f90^
 code/ops/ops_advect.f90^
 code/IO/export_raw_processed.f90^
 code/probes/probe_transient.f90^
 code/probes/probe_base.f90^
 code/probes/probe_derived.f90^
 code/solvers/matrix_free_params.f90^
 code/solvers/matrix_free_operators.f90^
 code/solvers/preconditioners.f90^
 code/solvers/AB2.f90^
 code/solvers/compute_energy.f90^
 code/sparse/matrix.f90^
 code/solvers/FFT/FFT_poisson.f90^
 code/solvers/Jacobi/Jacobi_solver.f90^
 code/solvers/Jacobi/Jacobi.f90^
 code/solvers/GS/GS_poisson.f90^
 code/solvers/SOR/SOR.f90^
 code/solvers/PSE/PSE_solver.f90^
 code/solvers/PSE/PSE.f90^
 code/solvers/PCG/PCG_aux.f90^
 code/solvers/PCG/PCG_solver.f90^
 code/solvers/PCG/PCG.f90^
 code/solvers/energy/init_TBCs.f90^
 code/solvers/energy/init_Tfield.f90^
 code/solvers/energy/init_K.f90^
 code/solvers/energy/energy_aux.f90^
 code/solvers/energy/energy_solver.f90^
 code/solvers/energy/energy.f90^
 code/solvers/momentum/profile_funcs.f90^
 code/solvers/momentum/init_UBCs.f90^
 code/solvers/momentum/init_PBCs.f90^
 code/solvers/momentum/init_Ufield.f90^
 code/solvers/momentum/init_Pfield.f90^
 code/solvers/momentum/E_K_budget_terms.f90^
 code/solvers/momentum/E_K_budget.f90^
 code/solvers/momentum/momentum_aux.f90^
 code/solvers/momentum/momentum_solver.f90^
 code/solvers/momentum/momentum.f90^
 code/solvers/induction/E_M_budget_terms.f90^
 code/solvers/induction/E_M_budget.f90^
 code/solvers/induction/init_phiBCs.f90^
 code/solvers/induction/init_BBCs.f90^
 code/solvers/induction/init_Bfield.f90^
 code/solvers/induction/init_Sigma.f90^
 code/solvers/induction/induction_aux.f90^
 code/solvers/induction/induction_solver.f90^
 code/solvers/induction/induction.f90^
 code/solvers/MHDSolver.f90^
 code/user/inputFile.f90^
 code/user/MOONS.f90^
 parametricStudy.f90^
 -o main.exe
del *.mod