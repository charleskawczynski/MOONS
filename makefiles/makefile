# ************* CLUSTER CONFIGURATION ******************
CLUSTER = 'PC_CK'
# CLUSTER = 'Workstation_CK'
# On Hoffman / DOE, run "module load gcc/4.9.3" in terminal before compile
# CLUSTER = 'Hoffman2'
# CLUSTER = 'DOE'
# ****************** OS SETTINGS ********************
ifdef SystemRoot
	OS_USED = 'OS_WINDOWS'
else
	OS_USED = 'OS_LINUX'
endif

ifeq ($(OS_USED),'OS_WINDOWS')
	RM = del
	PS = $(strip \\)
else
	PS = $(strip /)
	RM = rm
endif

ifeq ($(CLUSTER),$(filter $(CLUSTER),'Workstation_CK' 'PC_CK'))
	PS_clean = $(strip \)
endif
ifeq ($(CLUSTER),$(filter $(CLUSTER),'Hoffman2'))
	PS_clean = $(strip /)
endif

# ************* MAKE CONFIGURATION ******************

MKE = gmake
# MKE = make
# ************* DIRECTORY SETTINGS ******************
SRC_DIR = ..$(PS)code

ifeq ($(CLUSTER),'PC_CK')
	TARGET_DIR = C:$(PS)Users$(PS)Charlie$(PS)Documents$(PS)MOONS$(PS)SIMS$(PS)MOONS1
endif

ifeq ($(CLUSTER),'Workstation_CK')
	TARGET_DIR = C:$(PS)Users$(PS)charl$(PS)Documents$(PS)MOONS$(PS)MOONS5
endif

ifeq ($(CLUSTER),'Hoffman2')
	TARGET_DIR = $(PS)u$(PS)home$(PS)c$(PS)ckawczyn$(PS)project-morley$(PS)MOONS_out$(PS)MOONS1
#	TARGET_DIR = $(PS)u$(PS)home$(PS)p$(PS)pyhuang$(PS)Public$(PS)MOONS_out$(PS)MOONS1 # For peter, check this
endif

ifeq ($(CLUSTER),'DOE')
	TARGET_DIR = $(PS)global$(PS)cscratch1$(PS)sd$(PS)cfusion$(PS)MOONS_out
endif

MOD_DIR = .$(PS)mod$(PS)
OBJ_DIR = .$(PS)obj$(PS)
LIB_DIR = $(SRC_DIR)$(PS)lib

# ************** COMPILER & STANDARD ****************
ifeq ($(CLUSTER),$(filter $(CLUSTER),'Workstation_CK' 'PC_CK' 'Hoffman2'))
	FC = gfortran
endif
ifeq ($(CLUSTER),'DOE')
	FC = ftn
endif

$(info CLUSTER = $(CLUSTER))
$(info BUILD COMMAND = $(FC))
$(info TARGET = $(TARGET_DIR))
$(info PS = $(PS))
$(info PS_clean = $(PS_clean))
$(info OS_USED = $(OS_USED))

# ****************** DEFAULT FLAGS ******************
FCFLAGS = -J"$(MOD_DIR)" -fimplicit-none -Wuninitialized -cpp
# FCFLAGS += -std=legacy
FCFLAGS += -std=gnu
# FCFLAGS += -std=f95
# FCFLAGS += -std=f2003
# FCFLAGS += -std=2008

# Precision (most files contain this)
# FCFLAGS += -D_SINGLE_PRECISION_
FCFLAGS += -D_DOUBLE_PRECISION_
# FCFLAGS += -D_QUAD_PRECISION_

# FFT_poisson.f90:
# FCFLAGS += -D_FFT_FULL_
# FCFLAGS += -D_FFT_RADIX2_

# is_nan.f90:
# FCFLAGS += -D_ISNAN_USE_HACK_

# Order of accuracy of finite difference stencil
# stencils.f90:
# FCFLAGS += -D_STENCILS_O2_
# FCFLAGS += -D_STENCILS_O4_


ifeq ($(OS_USED),'OS_WINDOWS')
	FCFLAGS += -D_OS_WINDOWS_
else
	FCFLAGS += -D_OS_LINUX_
endif

# FCFLAGS += -D_DEL_PLANAR_X_
# FCFLAGS += -D_DEL_PLANAR_Y_
# FCFLAGS += -D_DEL_PLANAR_Z_

# ******************** LIBRARIES ********************
# FLIBS = $(LIB_DIR)$(PS)tecio.lib

# ****************** DEBUGGING **********************
# fcheck=all includes -fcheck-array-temporaries, 
# which triggers tons of warning in stencils.f90
# So include all but this one:

# -O0 bad in cygwin generated gfortran
FC_DEBUG += -Wall -Wextra -fbacktrace -O0 -Og
# FC_DEBUG += -Wall -Wextra -fbacktrace

# Suppress temporrary array warning:
# FC_DEBUG += -Wno-array-temporaries
FC_DEBUG += -fcheck=bounds -fcheck=do -fcheck=mem -fcheck=pointer -fcheck=recursion

# These flags are for rigorous mesh / stencil checking
# FC_DEBUG += -D_DEBUG_COORDINATES_
# FC_DEBUG += -D_DEBUG_COORDINATE_STRETCH_PARAM_MATCH_
# FC_DEBUG += -D_DEBUG_APPLY_STITCHES
FC_DEBUG += -D_DEBUG_EMBEDEXTRACT_
FC_DEBUG += -D_DEBUG_DATA_LOCATION_
FC_DEBUG += -D_DEBUG_ARRAY_
FC_DEBUG += -D_DEBUG_GF_
FC_DEBUG += -D_DEBUG_OPS_NORMS_
FC_DEBUG += -D_DEBUG_SF_
FC_DEBUG += -D_DEBUG_VF_
FC_DEBUG += -D_DEBUG_TF_
FC_DEBUG += -D_DEBUG_DEL_
FC_DEBUG += -D_DEBUG_DISCRETE_OPS_
FC_DEBUG += -D_DEBUG_INTERP_
FC_DEBUG += -D_DEBUG_BOUNDARY_CONDITIONS_
FC_DEBUG += -D_DEBUG_APPLY_BCS_
FC_DEBUG += -D_DEBUG_IO_TOOLS_
FC_DEBUG += -D_DEBUG_GF_NORMS_
FC_DEBUG += -D_DEBUG_STENCIL_1D_
FC_DEBUG += -D_DEBUG_STENCIL_3D_

# **************** OPTIMIZE/PARALLELIZE *********************
FCFLAGS += -fopenmp
FC_OPTIMIZE += -g -O3

# 3D OPERATIONS
FC_OPTIMIZE += -D_PARALLELIZE_GF_
FC_OPTIMIZE += -D_PARALLELIZE_DEL_
FC_OPTIMIZE += -D_PARALLELIZE_INTERP_
FC_OPTIMIZE += -D_PARALLELIZE_CROSS_PRODUCT_GF_
FC_OPTIMIZE += -D_PARALLELIZE_Jacobi_
FC_OPTIMIZE += -D_PARALLELIZE_SOR_
FC_OPTIMIZE += -D_PARALLELIZE_EMBEDEXTRACT_
FC_OPTIMIZE += -D_PARALLELIZE_GF_NORMS_

# 2D OPERATIONS
# Choose 1 of 2:
# FC_OPTIMIZE += -D_PARALLELIZE_GF_PLANE_
FC_OPTIMIZE += -D_PARALLELIZE_BF_PLANE_
# Choose 1 of 2:
# FC_OPTIMIZE += -D_PARALLELIZE_APPLY_BCS_FACES_
FC_OPTIMIZE += -D_PARALLELIZE_APPLY_BCS_FACES_RAW_

# Independent:
FC_OPTIMIZE += -D_PARALLELIZE_GF_PLANE_SUM_

# 1D OPERATIONS (likely not worth it)
# FC_OPTIMIZE += -D_PARALLELIZE_APPLY_BCS_EDGES_RAW_
# FC_OPTIMIZE += -D_PARALLELIZE_APPLY_BCS_CORNERS_RAW_

# AUXILIARY OPERATIONS
# FC_OPTIMIZE += -D_OPTIMIZE_IO_TOOLS_
FC_OPTIMIZE += -D_PARALLELIZE_RICHARDSONEXTRAPOLATION_

# FC_OPTIMIZE += mpif90
# FC_OPTIMIZE += mpirun -np 4

# ****************** PROFILING **********************
# To profile: run sim profile flag, then run
# gprof MOONS.exe > timeReport.txt
FC_PROFILE += -pg

# ******** ITERATIVE SOLVER CONVERGENCE TESTS *******

# Code verification
# FCFLAGS += -D_EXPORT_JAC_SF_CONVERGENCE_
# FCFLAGS += -D_EXPORT_JAC_VF_CONVERGENCE_
# FCFLAGS += -D_EXPORT_SOR_CONVERGENCE_
# FCFLAGS += -D_EXPORT_CG_CONVERGENCE_
# FCFLAGS += -D_EXPORT_PCG_SF_CONVERGENCE_
# FCFLAGS += -D_EXPORT_PCG_VF_CONVERGENCE_
# FCFLAGS += -D_EXPORT_GS_CONVERGENCE_
# FCFLAGS += -D_EXPORT_MG_CONVERGENCE_

# FCFLAGS += $(FC_DEBUG)
# FCFLAGS += $(FC_PROFILE)
FCFLAGS += $(FC_OPTIMIZE)

TARGET = $(TARGET_DIR)$(PS)MOONS.exe

# **************** INCLUDE PATHS ********************
VPATH =\
	$(TARGET_DIR)\
	$(SRC_DIR)$(PS)block\
	$(SRC_DIR)$(PS)BCs\
	$(SRC_DIR)$(PS)clock\
	$(SRC_DIR)$(PS)convergence_rate\
	$(SRC_DIR)$(PS)coordinates\
	$(SRC_DIR)$(PS)directory\
	$(SRC_DIR)$(PS)domain\
	$(SRC_DIR)$(PS)fields\
	$(SRC_DIR)$(PS)fields$(PS)default\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_2D\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_arithmatic_3D\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_matmult_3D\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_global\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)BF\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)SF\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)VF\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)TF\
	$(SRC_DIR)$(PS)geometric_region\
	$(SRC_DIR)$(PS)globals\
	$(SRC_DIR)$(PS)grid\
	$(SRC_DIR)$(PS)IO\
	$(SRC_DIR)$(PS)lib\
	$(SRC_DIR)$(PS)math\
	$(SRC_DIR)$(PS)mesh\
	$(SRC_DIR)$(PS)norms\
	$(SRC_DIR)$(PS)ops\
	$(SRC_DIR)$(PS)probes\
	$(SRC_DIR)$(PS)solvers\
	$(SRC_DIR)$(PS)procedure_array\
	$(SRC_DIR)$(PS)solvers$(PS)energy\
	$(SRC_DIR)$(PS)solvers$(PS)FFT\
	$(SRC_DIR)$(PS)solvers$(PS)GS\
	$(SRC_DIR)$(PS)solvers$(PS)induction\
	$(SRC_DIR)$(PS)solvers$(PS)Jacobi\
	$(SRC_DIR)$(PS)solvers$(PS)MG\
	$(SRC_DIR)$(PS)solvers$(PS)momentum\
	$(SRC_DIR)$(PS)solvers$(PS)PCG\
	$(SRC_DIR)$(PS)solvers$(PS)PSE\
	$(SRC_DIR)$(PS)solvers$(PS)SOR\
	$(SRC_DIR)$(PS)sparse\
	$(SRC_DIR)$(PS)stencils\
	$(SRC_DIR)$(PS)stitches\
	$(SRC_DIR)$(PS)string\
	$(SRC_DIR)$(PS)table\
	$(SRC_DIR)$(PS)user\
	$(SRC_DIR)$(PS)version

# **************** SOURCE FILES *********************
SRCS_F =\
	$(SRC_DIR)$(PS)globals$(PS)current_precision.f90\
	$(SRC_DIR)$(PS)BCs$(PS)face_edge_corner_indexing.f90\
	$(SRC_DIR)$(PS)math$(PS)simple_int_tensor.f90\
	$(SRC_DIR)$(PS)string$(PS)string.f90\
	$(SRC_DIR)$(PS)string$(PS)string_aux.f90\
	$(SRC_DIR)$(PS)fields$(PS)data_location.f90\
	$(SRC_DIR)$(PS)math$(PS)is_nan.f90\
	$(SRC_DIR)$(PS)math$(PS)even_odd.f90\
	$(SRC_DIR)$(PS)IO$(PS)fmt.f90\
	$(SRC_DIR)$(PS)IO$(PS)datatype_conversion.f90\
	$(SRC_DIR)$(PS)IO$(PS)inquire_funcs.f90\
	$(SRC_DIR)$(PS)IO$(PS)IO_check.f90\
	$(SRC_DIR)$(PS)IO$(PS)IO_tools.f90\
	$(SRC_DIR)$(PS)IO$(PS)exp_Tecplot_Zone.f90\
	$(SRC_DIR)$(PS)IO$(PS)exp_Tecplot_Header.f90\
	$(SRC_DIR)$(PS)table$(PS)table.f90\
	$(SRC_DIR)$(PS)directory$(PS)path.f90\
	$(SRC_DIR)$(PS)directory$(PS)dir_tree.f90\
	$(SRC_DIR)$(PS)sparse$(PS)array.f90\
	$(SRC_DIR)$(PS)sparse$(PS)sparse.f90\
	$(SRC_DIR)$(PS)stitches$(PS)stitch.f90\
	$(SRC_DIR)$(PS)coordinates$(PS)derivative_stencils.f90\
	$(SRC_DIR)$(PS)coordinates$(PS)interpolation_stencils.f90\
	$(SRC_DIR)$(PS)coordinates$(PS)coordinates.f90\
	$(SRC_DIR)$(PS)domain$(PS)overlap.f90\
	$(SRC_DIR)$(PS)grid$(PS)grid.f90\
	$(SRC_DIR)$(PS)coordinates$(PS)coordinate_stretch_parameters.f90\
	$(SRC_DIR)$(PS)coordinates$(PS)coordinate_distribution_funcs.f90\
	$(SRC_DIR)$(PS)coordinates$(PS)coordinate_stretch_param_match.f90\
	$(SRC_DIR)$(PS)grid$(PS)grid_genHelper.f90\
	$(SRC_DIR)$(PS)grid$(PS)grid_init.f90\
	$(SRC_DIR)$(PS)grid$(PS)grid_extend.f90\
	$(SRC_DIR)$(PS)grid$(PS)grid_connect.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_base.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_arithmatic_3D$(PS)GF_assign.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_arithmatic_3D$(PS)GF_add.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_arithmatic_3D$(PS)GF_subtract.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_arithmatic_3D$(PS)GF_multiply.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_arithmatic_3D$(PS)GF_divide.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_arithmatic_3D$(PS)GF_add_product.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_arithmatic_3D$(PS)GF_product_add.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_arithmatic_3D$(PS)GF_square.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_arithmatic_3D$(PS)GF_cross_product.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_2D$(PS)GF_assign_plane.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_2D$(PS)GF_add_plane.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_2D$(PS)GF_assign_ghost.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_2D$(PS)GF_assign_wall.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_2D$(PS)GF_multiply_plane.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_2D$(PS)GF_multiply_wall.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_aux.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_distributions.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_diagonals.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_plane_sum.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_plane_mean.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_mean_along_dir.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_symmetry_error.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_norms_weights.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_norms.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_matmult_3D$(PS)GF_interp.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_matmult_3D$(PS)GF_laplacian.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF_matmult_3D$(PS)GF_curl_curl.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)GF$(PS)GF.f90\
	$(SRC_DIR)$(PS)sparse$(PS)sparse_field.f90\
	$(SRC_DIR)$(PS)sparse$(PS)stencil_1D.f90\
	$(SRC_DIR)$(PS)sparse$(PS)stencil_3D.f90\
	$(SRC_DIR)$(PS)BCs$(PS)bctype.f90\
	$(SRC_DIR)$(PS)BCs$(PS)BC_logicals.f90\
	$(SRC_DIR)$(PS)domain$(PS)sub_domain.f90\
	$(SRC_DIR)$(PS)domain$(PS)physical_sub_domain.f90\
	$(SRC_DIR)$(PS)domain$(PS)face_SD.f90\
	$(SRC_DIR)$(PS)domain$(PS)edge_SD.f90\
	$(SRC_DIR)$(PS)block$(PS)block_curl_curl_stencil.f90\
	$(SRC_DIR)$(PS)block$(PS)block_Laplacian_stencil.f90\
	$(SRC_DIR)$(PS)block$(PS)block.f90\
	$(SRC_DIR)$(PS)mesh$(PS)mesh.f90\
	$(SRC_DIR)$(PS)domain$(PS)physical_domain.f90\
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_faces_raw.f90\
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_edges_raw.f90\
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_faces_raw_implicit.f90\
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_faces_bridge.f90\
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_faces_bridge_implicit.f90\
	$(SRC_DIR)$(PS)procedure_array$(PS)single_procedure.f90\
	$(SRC_DIR)$(PS)procedure_array$(PS)procedure_array.f90\
	$(SRC_DIR)$(PS)procedure_array$(PS)single_procedure_plane_op.f90\
	$(SRC_DIR)$(PS)procedure_array$(PS)procedure_array_plane_op.f90\
	$(SRC_DIR)$(PS)BCs$(PS)boundary.f90\
	$(SRC_DIR)$(PS)BCs$(PS)boundary_conditions.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)BF$(PS)BF.f90\
	$(SRC_DIR)$(PS)block$(PS)mesh_block.f90\
	$(SRC_DIR)$(PS)user$(PS)sim_params.f90\
	$(SRC_DIR)$(PS)user$(PS)print_export.f90\
	$(SRC_DIR)$(PS)user$(PS)kill_switch.f90\
	$(SRC_DIR)$(PS)user$(PS)export_now.f90\
	$(SRC_DIR)$(PS)domain$(PS)mesh_domain.f90\
	$(SRC_DIR)$(PS)mesh$(PS)mesh_simple_geometries.f90\
	$(SRC_DIR)$(PS)mesh$(PS)mesh_BC_geometries.f90\
	$(SRC_DIR)$(PS)mesh$(PS)mesh_BC_geometries_symmetric.f90\
	$(SRC_DIR)$(PS)mesh$(PS)mesh_complex_geometries.f90\
	$(SRC_DIR)$(PS)mesh$(PS)mesh_generate.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)SF$(PS)SF.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)VF$(PS)VF.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)TF$(PS)TF.f90\
	$(SRC_DIR)$(PS)fields$(PS)default$(PS)index_mapping.f90\
	$(SRC_DIR)$(PS)solvers$(PS)time_marching_params.f90\
	$(SRC_DIR)$(PS)mesh$(PS)mesh_stencils.f90\
	$(SRC_DIR)$(PS)IO$(PS)export_raw.f90\
	$(SRC_DIR)$(PS)IO$(PS)export_g.f90\
	$(SRC_DIR)$(PS)IO$(PS)export_SF.f90\
	$(SRC_DIR)$(PS)IO$(PS)export_VF.f90\
	$(SRC_DIR)$(PS)IO$(PS)import_raw.f90\
	$(SRC_DIR)$(PS)IO$(PS)import_g.f90\
	$(SRC_DIR)$(PS)IO$(PS)import_SF.f90\
	$(SRC_DIR)$(PS)IO$(PS)import_VF.f90\
	$(SRC_DIR)$(PS)IO$(PS)IO_SF.f90\
	$(SRC_DIR)$(PS)IO$(PS)IO_VF.f90\
	$(SRC_DIR)$(PS)version$(PS)version.f90\
	$(SRC_DIR)$(PS)clock$(PS)unit_conversion.f90\
	$(SRC_DIR)$(PS)clock$(PS)clock.f90\
	$(SRC_DIR)$(PS)clock$(PS)stop_clock.f90\
	$(SRC_DIR)$(PS)ops$(PS)ops_norms.f90\
	$(SRC_DIR)$(PS)norms$(PS)norms.f90\
	$(SRC_DIR)$(PS)geometric_region$(PS)geometric_region.f90\
	$(SRC_DIR)$(PS)ops$(PS)ops_fft.f90\
	$(SRC_DIR)$(PS)ops$(PS)ops_dct.f90\
	$(SRC_DIR)$(PS)ops$(PS)ops_idct.f90\
	$(SRC_DIR)$(PS)stencils$(PS)stencils.f90\
	$(SRC_DIR)$(PS)ops$(PS)ops_del.f90\
	$(SRC_DIR)$(PS)BCs$(PS)check_BCs.f90\
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_embed.f90\
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_faces.f90\
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_faces_implicit.f90\
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs.f90\
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_implicit.f90\
	$(SRC_DIR)$(PS)ops$(PS)ops_embedExtract.f90\
	$(SRC_DIR)$(PS)ops$(PS)ops_interp.f90\
	$(SRC_DIR)$(PS)IO$(PS)export_raw_processed.f90\
	$(SRC_DIR)$(PS)ops$(PS)ops_aux.f90\
	$(SRC_DIR)$(PS)ops$(PS)ops_discrete.f90\
	$(SRC_DIR)$(PS)ops$(PS)ops_advect.f90\
	$(SRC_DIR)$(PS)probes$(PS)probe.f90\
	$(SRC_DIR)$(PS)solvers$(PS)iter_solver_params.f90\
	$(SRC_DIR)$(PS)solvers$(PS)matrix_free_params.f90\
	$(SRC_DIR)$(PS)solvers$(PS)matrix_free_operators.f90\
	$(SRC_DIR)$(PS)solvers$(PS)BC_funcs.f90\
	$(SRC_DIR)$(PS)solvers$(PS)preconditioners.f90\
	$(SRC_DIR)$(PS)solvers$(PS)AB2.f90\
	$(SRC_DIR)$(PS)solvers$(PS)compute_energy.f90\
	$(SRC_DIR)$(PS)sparse$(PS)matrix.f90\
	$(SRC_DIR)$(PS)solvers$(PS)FFT$(PS)FFT_poisson.f90\
	$(SRC_DIR)$(PS)solvers$(PS)Jacobi$(PS)Jacobi_solver.f90\
	$(SRC_DIR)$(PS)solvers$(PS)Jacobi$(PS)Jacobi.f90\
	$(SRC_DIR)$(PS)solvers$(PS)GS$(PS)GS_solver.f90\
	$(SRC_DIR)$(PS)solvers$(PS)GS$(PS)GS_poisson.f90\
	$(SRC_DIR)$(PS)solvers$(PS)SOR$(PS)SOR.f90\
	$(SRC_DIR)$(PS)solvers$(PS)PSE$(PS)PSE_solver.f90\
	$(SRC_DIR)$(PS)solvers$(PS)PSE$(PS)PSE.f90\
	$(SRC_DIR)$(PS)solvers$(PS)PCG$(PS)PCG_solver.f90\
	$(SRC_DIR)$(PS)solvers$(PS)PCG$(PS)PCG.f90\
	$(SRC_DIR)$(PS)solvers$(PS)energy$(PS)init_TBCs.f90\
	$(SRC_DIR)$(PS)solvers$(PS)energy$(PS)init_Tfield.f90\
	$(SRC_DIR)$(PS)solvers$(PS)energy$(PS)init_K.f90\
	$(SRC_DIR)$(PS)solvers$(PS)energy$(PS)energy_aux.f90\
	$(SRC_DIR)$(PS)solvers$(PS)energy$(PS)energy_solver.f90\
	$(SRC_DIR)$(PS)solvers$(PS)energy$(PS)energy.f90\
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)vorticity_streamfunction.f90\
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)profile_funcs.f90\
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)export_analytic.f90\
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)init_UBCs.f90\
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)init_PBCs.f90\
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)init_Ufield.f90\
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)init_Pfield.f90\
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)E_K_budget_terms.f90\
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)E_K_budget.f90\
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)momentum_aux.f90\
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)momentum_solver.f90\
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)momentum.f90\
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)E_M_budget_terms.f90\
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)E_M_budget.f90\
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)init_phiBCs.f90\
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)init_BBCs.f90\
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)init_Bfield.f90\
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)init_B_interior.f90\
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)init_J_interior.f90\
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)init_Sigma.f90\
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)induction_aux.f90\
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)induction_solver.f90\
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)induction.f90\
	$(SRC_DIR)$(PS)solvers$(PS)MHDSolver.f90\
	$(SRC_DIR)$(PS)user$(PS)inputFile.f90\
	$(SRC_DIR)$(PS)user$(PS)MOONS.f90

SRCS_F += $(TARGET_DIR)$(PS)parametricStudy.f90

# **************** DO NOT EDIT BELOW HERE *********************
# **************** DO NOT EDIT BELOW HERE *********************
# **************** DO NOT EDIT BELOW HERE *********************

OBJS_F = $(patsubst %.f90,$(OBJ_DIR)$(PS)%.o,$(notdir $(SRCS_F)))

$(warning ************************************)
$(warning ********** BUILDING MOONS **********)
$(warning ************************************)

all: $(TARGET)

$(TARGET): $(OBJS_F)
	$(FC) -o $@ $(FCFLAGS) $(OBJS_F) $(FLIBS)

$(OBJ_DIR)$(PS)%.o: %.f90
	$(FC) $(FCFLAGS) -c -o $@ $< $(info )

clean:
	-$(RM) $(subst $(PS),$(PS_clean),$(MOD_DIR)$(PS)*.mod)
	-$(RM) $(subst $(PS),$(PS_clean),$(OBJ_DIR)$(PS)*.o)
	-$(RM) $(subst $(PS),$(PS_clean),$(TARGET).exe)
	-$(RM) $(subst $(PS),$(PS_clean),$(TARGET))

run: myRun
myRun: $(TARGET)
	cd $(TARGET_DIR) && $(MKE) run


info:;  @echo " "
	@echo " "
	@echo "Source files:"
	@echo $(SRCS_F)
	@echo " "
	@echo "Object files:"
	@echo $(OBJS_F)
	@echo " "
	@echo "Compiler          : $(FC)"
	@echo "Library directory : $(LIB_DIR)"
	@echo "Target directory  : $(TARGET_DIR)"
	@echo "Modules directory : $(MOD_DIR)"
	@echo "Object directory  : $(OBJ_DIR)"
	@echo " "