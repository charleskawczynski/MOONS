# ****************** OS SETTINGS ********************
ifdef SystemRoot
    PATHSEP2 = \\
	RM = del
else
    PATHSEP2 = /
	RM = rm
endif
PS = $(strip $(PATHSEP2))
# PS1 is needed for gmake clean
PS1 = $(strip \)
extf90 = $(strip .f90)

# ************* MAKE CONFIGURATION ******************
MKE = gmake
# MKE = make
# ************* DIRECTORY SETTINGS ******************
USER = Charlie
# USER = LM_MHD
# USER = charl

# Code directory
SRC_DIR = C:$(PS)Users$(PS)$(USER)$(PS)Documents$(PS)GitHub$(PS)MOONS$(PS)code
# SRC_DIR = C:$(PS)Users$(PS)LM_MHD$(PS)Documents$(PS)GitHub$(PS)MOONS$(PS)code

# Output directory
# TARGET_DIR = C:$(PS)Users$(PS)$(USER)$(PS)Documents$(PS)MOONS_unitTest
# TARGET_DIR = C:$(PS)Users$(PS)$(USER)$(PS)Documents$(PS)MOONS_unitTest2
# TARGET_DIR = C:$(PS)Users$(PS)$(USER)$(PS)Documents$(PS)MOONS_unitTest3
# TARGET_DIR = C:$(PS)Users$(PS)$(USER)$(PS)Documents$(PS)MOONS_unitTest4
# TARGET_DIR = C:$(PS)Users$(PS)$(USER)$(PS)Documents$(PS)MOONS_unitTest5
# TARGET_DIR = C:$(PS)Users$(PS)$(USER)$(PS)Documents$(PS)MOONS_unitTest6
# TARGET_DIR = C:$(PS)Users$(PS)$(USER)$(PS)Documents$(PS)MOONS$(PS)MOONS
TARGET_DIR = C:$(PS)Users$(PS)$(USER)$(PS)Documents$(PS)MOONS$(PS)MOONS2
# TARGET_DIR = C:$(PS)Users$(PS)$(USER)$(PS)Documents$(PS)MOONS$(PS)MOONS3
# TARGET_DIR = C:$(PS)Users$(PS)$(USER)$(PS)Documents$(PS)MOONS$(PS)MOONS4

MOD_DIR = $(TARGET_DIR)$(PS)mod
OBJ_DIR = $(TARGET_DIR)$(PS)obj
LIB_DIR = $(SRC_DIR)$(PS)lib

# ************** COMPILER & STANDARD ****************
FC      = gfortran

# ****************** DEFAULT FLAGS ******************
FCFLAGS = -J"$(MOD_DIR)" -fimplicit-none -Wuninitialized -cpp
# FCFLAGS += -std=f95
# Precision
# FCFLAGS += -D_SINGLE_PRECISION_
FCFLAGS += -D_DOUBLE_PRECISION_
# FCFLAGS += -D_QUAD_PRECISION_

# FFT Methods
# FCFLAGS += -D_FFT_FULL_
FCFLAGS += -D_FFT_RADIX2_

# Order of accuracy of finite difference stencil
# FCFLAGS += -D_STENCILS_O2_
# FCFLAGS += -D_STENCILS_O4_

FCFLAGS += -fopenmp

# FCFLAGS += -D_DEL_PLANAR_X_
# FCFLAGS += -D_DEL_PLANAR_Y_
# FCFLAGS += -D_DEL_PLANAR_Z_

# ******************** LIBRARIES ********************
# FLIBS = $(LIB_DIR)$(PS)tecio.lib

# ****************** DEBUGGING **********************
# fcheck=all includes -fcheck-array-temporaries, 
# which triggers tons of warning in stencils.f90
# So include all but this one:
FC_DEBUG += -Wall -Wextra -fbacktrace -O0 -Og
FC_DEBUG += -fcheck=bounds -fcheck=do -fcheck=mem -fcheck=pointer -fcheck=recursion

FC_DEBUG += -D_DEBUG_RF_
FC_DEBUG += -D_DEBUG_SF_
FC_DEBUG += -D_DEBUG_VF_
FC_DEBUG += -D_DEBUG_TF_

# _DEBUG_COORDINATES_ is for rigorous mesh / stencil checking
# FC_DEBUG += -D_DEBUG_COORDINATES_

FC_DEBUG += -D_DEBUG_DEL_
FC_DEBUG += -D_DEBUG_DISCRETE_OPS_
FC_DEBUG += -D_DEBUG_INTERP_
FC_DEBUG += -D_DEBUG_APPLY_BCS_
FC_DEBUG += -D_STENCILS_SUPPRESS_WARNING_

# Suppress temporrary array warning:
FC_DEBUG += -Wno-array-temporaries

# **************** OPTIMIZE/PARALLELIZE *********************
FC_OPTIMIZE += -g -O3
FC_OPTIMIZE += -D_PARALLELIZE_RF_
FC_OPTIMIZE += -D_PARALLELIZE_SF_
FC_OPTIMIZE += -D_PARALLELIZE_INTERP_

FC_OPTIMIZE += -D_PARALLELIZE_Jacobi_
FC_OPTIMIZE += -D_PARALLELIZE_SOR_
FC_OPTIMIZE += -D_PARALLELIZE_EMBEDEXTRACT_

# FC_OPTIMIZE += mpif90
# FC_OPTIMIZE += mpirun -np 4

# ****************** PROFILING **********************
# To profile: run sim profile flag, then run
# gprof MOONS.exe > timeReport.txt
FC_PROFILE += -pg

# ******** ITERATIVE SOLVER CONVERGENCE TESTS *******

# Code verification
# FCFLAGS += -D_EXPORT_JAC_CONVERGENCE_
# FCFLAGS += -D_EXPORT_SOR_CONVERGENCE_
# FCFLAGS += -D_EXPORT_CG_CONVERGENCE_
# FCFLAGS += -D_EXPORT_PCG_SF_CONVERGENCE_
# FCFLAGS += -D_EXPORT_PCG_VF_CONVERGENCE_
# FCFLAGS += -D_EXPORT_GS_CONVERGENCE_
# FCFLAGS += -D_EXPORT_MG_CONVERGENCE_

# FCFLAGS += $(FC_DEBUG)
# FCFLAGS += $(FC_PROFILE)
FCFLAGS += $(FC_OPTIMIZE)

# ********** UNIT TEST / FULL SIMULATION *************
# UNIT_TEST = $(SRC_DIR)$(PS)unit_testing$(PS)test_triSolver.f90
# UNIT_TEST = $(SRC_DIR)$(PS)unit_testing$(PS)test_SOR.f90
# UNIT_TEST = $(SRC_DIR)$(PS)unit_testing$(PS)test_complexG.f90
# UNIT_TEST = $(SRC_DIR)$(PS)unit_testing$(PS)test_JAC.f90
# UNIT_TEST = $(SRC_DIR)$(PS)unit_testing$(PS)test_CG_node.f90
# UNIT_TEST = $(SRC_DIR)$(PS)unit_testing$(PS)test_cleanB.f90
# UNIT_TEST = $(SRC_DIR)$(PS)unit_testing$(PS)test_PCG.f90
# UNIT_TEST = $(SRC_DIR)$(PS)unit_testing$(PS)test_CG.f90
# UNIT_TEST = $(SRC_DIR)$(PS)unit_testing$(PS)test_MG.f90

TARGET = $(TARGET_DIR)$(PS)MOONS
# TARGET = $(TARGET_DIR)$(PS)unitTest

# **************** INCLUDE PATHS ********************
VPATH =\
	$(TARGET_DIR) \
	$(SRC_DIR)$(PS)lib \
	$(SRC_DIR)$(PS)user \
	$(SRC_DIR)$(PS)BCs \
	$(SRC_DIR)$(PS)BCs$(PS)stitches \
	$(SRC_DIR)$(PS)unit_testing \
	$(SRC_DIR)$(PS)IO \
	$(SRC_DIR)$(PS)grid \
	$(SRC_DIR)$(PS)probes \
	$(SRC_DIR)$(PS)fields \
	$(SRC_DIR)$(PS)fields$(PS)RF \
	$(SRC_DIR)$(PS)fields$(PS)SF \
	$(SRC_DIR)$(PS)fields$(PS)VF \
	$(SRC_DIR)$(PS)fields$(PS)TF \
	$(SRC_DIR)$(PS)ops \
	$(SRC_DIR)$(PS)static \
	$(SRC_DIR)$(PS)sparse \
	$(SRC_DIR)$(PS)computations \
	$(SRC_DIR)$(PS)solvers \
	$(SRC_DIR)$(PS)solvers$(PS)Jacobi \
	$(SRC_DIR)$(PS)solvers$(PS)GS \
	$(SRC_DIR)$(PS)solvers$(PS)SOR \
	$(SRC_DIR)$(PS)solvers$(PS)MG \
	$(SRC_DIR)$(PS)solvers$(PS)PCG \
	$(SRC_DIR)$(PS)solvers$(PS)FFT \
	$(SRC_DIR)$(PS)solvers$(PS)PSE \
	$(SRC_DIR)$(PS)solvers$(PS)energy \
	$(SRC_DIR)$(PS)solvers$(PS)momentum \
	$(SRC_DIR)$(PS)solvers$(PS)induction


# **************** SOURCE FILES *********************
# $(SRC_DIR)$(PS)static$(PS)dataSet.f90 \
# $(SRC_DIR)$(PS)static$(PS)varStr.f90 \

# $(SRC_DIR)$(PS)fields$(PS)RF.f90 \
# $(SRC_DIR)$(PS)fields$(PS)SF.f90 \
# $(SRC_DIR)$(PS)fields$(PS)VF.f90 \
# $(SRC_DIR)$(PS)fields$(PS)TF.f90 \

# $(SRC_DIR)$(PS)fields$(PS)RF$(PS)RF.f90 \
# $(SRC_DIR)$(PS)fields$(PS)RF$(PS)RF_assign.f90 \
# $(SRC_DIR)$(PS)fields$(PS)RF$(PS)RF_assign_negative.f90 \
# $(SRC_DIR)$(PS)fields$(PS)RF$(PS)RF_add.f90 \
# $(SRC_DIR)$(PS)fields$(PS)RF$(PS)RF_subtract.f90 \
# $(SRC_DIR)$(PS)fields$(PS)RF$(PS)RF_multiply.f90 \
# $(SRC_DIR)$(PS)fields$(PS)RF$(PS)RF_divide.f90 \
# $(SRC_DIR)$(PS)fields$(PS)RF$(PS)RF_square.f90 \
# $(SRC_DIR)$(PS)fields$(PS)RF$(PS)RF_sum.f90 \
# $(SRC_DIR)$(PS)fields$(PS)RF$(PS)RF_aux.f90 \
# $(SRC_DIR)$(PS)fields$(PS)SF$(PS)SF.f90 \
# $(SRC_DIR)$(PS)fields$(PS)VF$(PS)VF.f90 \
# $(SRC_DIR)$(PS)fields$(PS)TF$(PS)TF.f90 \
# $(SRC_DIR)$(PS)fields$(PS)VF$(PS)VF_assign.f90 \
# $(SRC_DIR)$(PS)fields$(PS)VF$(PS)VF_assign_negative.f90 \
# $(SRC_DIR)$(PS)fields$(PS)VF$(PS)VF_add.f90 \
# $(SRC_DIR)$(PS)fields$(PS)VF$(PS)VF_subtract.f90 \
# $(SRC_DIR)$(PS)fields$(PS)VF$(PS)VF_multiply.f90 \
# $(SRC_DIR)$(PS)fields$(PS)VF$(PS)VF_divide.f90 \
# $(SRC_DIR)$(PS)fields$(PS)VF$(PS)VF_aux.f90 \

# $(SRC_DIR)$(PS)solvers$(PS)MG$(PS)MG_tools.f90 \
# $(SRC_DIR)$(PS)solvers$(PS)MG$(PS)MG.f90 \


SRCS_F =\
	$(SRC_DIR)$(PS)IO$(PS)IO_tools.f90 \
	$(SRC_DIR)$(PS)IO$(PS)exp_Tecplot_Zone.f90 \
	$(SRC_DIR)$(PS)IO$(PS)exp_Tecplot_Header.f90 \
	$(SRC_DIR)$(PS)IO$(PS)IO_auxiliary.f90 \
	$(SRC_DIR)$(PS)static$(PS)array.f90 \
	$(SRC_DIR)$(PS)static$(PS)sparse.f90 \
	$(SRC_DIR)$(PS)static$(PS)triDiag.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)stitches$(PS)stitch_face.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)stitches$(PS)stitch_edge.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)stitches$(PS)stitch_corner.f90 \
	$(SRC_DIR)$(PS)grid$(PS)coordinates.f90 \
	$(SRC_DIR)$(PS)grid$(PS)grid.f90 \
	$(SRC_DIR)$(PS)grid$(PS)grid_distribution_funcs.f90 \
	$(SRC_DIR)$(PS)grid$(PS)grid_genHelper.f90 \
	$(SRC_DIR)$(PS)grid$(PS)grid_stretchParamMatch.f90 \
	$(SRC_DIR)$(PS)grid$(PS)grid_init.f90 \
	$(SRC_DIR)$(PS)grid$(PS)grid_extend.f90 \
	$(SRC_DIR)$(PS)grid$(PS)grid_connect.f90 \
	$(SRC_DIR)$(PS)grid$(PS)grid_generate.f90 \
	$(SRC_DIR)$(PS)grid$(PS)mesh.f90 \
	$(SRC_DIR)$(PS)user$(PS)simParams.f90 \
	$(SRC_DIR)$(PS)grid$(PS)subdomain.f90 \
	$(SRC_DIR)$(PS)grid$(PS)domain.f90 \
	$(SRC_DIR)$(PS)grid$(PS)mesh_geometries.f90 \
	$(SRC_DIR)$(PS)grid$(PS)mesh_generate.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)bctype.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)corner.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)edge.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)face.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)BCs.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)vectorBCs.f90 \
	$(SRC_DIR)$(PS)fields$(PS)RF.f90 \
	$(SRC_DIR)$(PS)fields$(PS)SF.f90 \
	$(SRC_DIR)$(PS)fields$(PS)VF.f90 \
	$(SRC_DIR)$(PS)fields$(PS)TF.f90 \
	$(SRC_DIR)$(PS)IO$(PS)export_raw.f90 \
	$(SRC_DIR)$(PS)IO$(PS)export_g.f90 \
	$(SRC_DIR)$(PS)IO$(PS)export_SF.f90 \
	$(SRC_DIR)$(PS)IO$(PS)export_VF.f90 \
	$(SRC_DIR)$(PS)IO$(PS)import_raw.f90 \
	$(SRC_DIR)$(PS)IO$(PS)import_g.f90 \
	$(SRC_DIR)$(PS)IO$(PS)import_SF.f90 \
	$(SRC_DIR)$(PS)IO$(PS)import_VF.f90 \
	$(SRC_DIR)$(PS)IO$(PS)IO_SF.f90 \
	$(SRC_DIR)$(PS)IO$(PS)IO_VF.f90 \
	$(SRC_DIR)$(PS)static$(PS)version.f90 \
	$(SRC_DIR)$(PS)static$(PS)clock.f90 \
	$(SRC_DIR)$(PS)static$(PS)stop_clock.f90 \
	$(SRC_DIR)$(PS)computations$(PS)norms.f90 \
	$(SRC_DIR)$(PS)ops$(PS)apply_stitches_faces.f90 \
	$(SRC_DIR)$(PS)ops$(PS)apply_stitches_edges.f90 \
	$(SRC_DIR)$(PS)ops$(PS)apply_stitches_corners.f90 \
	$(SRC_DIR)$(PS)ops$(PS)apply_stitches.f90 \
	$(SRC_DIR)$(PS)ops$(PS)ops_fft.f90 \
	$(SRC_DIR)$(PS)ops$(PS)ops_dct.f90 \
	$(SRC_DIR)$(PS)ops$(PS)ops_idct.f90 \
	$(SRC_DIR)$(PS)computations$(PS)stencils.f90 \
	$(SRC_DIR)$(PS)ops$(PS)ops_del.f90 \
	$(SRC_DIR)$(PS)ops$(PS)ops_interp.f90 \
	$(SRC_DIR)$(PS)ops$(PS)ops_embedExtract.f90 \
	$(SRC_DIR)$(PS)ops$(PS)ops_aux.f90 \
	$(SRC_DIR)$(PS)ops$(PS)ops_discrete.f90 \
	$(SRC_DIR)$(PS)ops$(PS)ops_advect.f90 \
	$(SRC_DIR)$(PS)IO$(PS)export_raw_processed.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_faces.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_faces_implicit.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_edges.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_corners.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs.f90 \
	$(SRC_DIR)$(PS)BCs$(PS)apply_BCs_implicit.f90 \
	$(SRC_DIR)$(PS)static$(PS)probe_transient.f90 \
	$(SRC_DIR)$(PS)static$(PS)probe_base.f90 \
	$(SRC_DIR)$(PS)static$(PS)probe_derived.f90 \
	$(SRC_DIR)$(PS)static$(PS)monitor.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)matrix_free_params.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)matrix_free_operators.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)preconditioners.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)AB2.f90 \
	$(SRC_DIR)$(PS)sparse$(PS)matrix.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)FFT$(PS)FFT_poisson.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)Jacobi$(PS)Jacobi_solver.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)Jacobi$(PS)Jacobi.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)GS$(PS)GS_poisson.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)SOR$(PS)SOR.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)PSE$(PS)PSE.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)CG$(PS)CG_aux.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)CG$(PS)PCG_solver.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)CG$(PS)PCG.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)energy$(PS)init_TBCs.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)energy$(PS)init_Tfield.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)energy$(PS)init_K.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)energy$(PS)energy_aux.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)energy$(PS)energy_solver.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)energy$(PS)energy.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)profile_funcs.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)init_UBCs.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)init_PBCs.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)init_Ufield.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)init_Pfield.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)momentum_aux.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)momentum_solver.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)momentum$(PS)momentum.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)init_BBCs.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)init_Bfield.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)init_Sigma.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)induction_aux.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)induction_solver.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)induction$(PS)induction.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)MHDSolver.f90 \
	$(SRC_DIR)$(PS)user$(PS)inputFile.f90 \
	$(SRC_DIR)$(PS)user$(PS)MOONS.f90 \
	$(SRC_DIR)$(PS)static$(PS)richardsonExtrapolation.f90 \
	$(SRC_DIR)$(PS)user$(PS)convergenceRate.f90

SRCS_F += $(UNIT_TEST)

SRCS_F += $(TARGET_DIR)$(PS)parametricStudy.f90

# **************** DO NOT EDIT BELOW HERE *********************
# **************** DO NOT EDIT BELOW HERE *********************
# **************** DO NOT EDIT BELOW HERE *********************


OBJS_F = $(patsubst %.f90,$(OBJ_DIR)$(PS)%.o,$(notdir $(SRCS_F)))

all: $(TARGET)

$(TARGET): $(OBJS_F)
	$(FC) -o $@ $(FCFLAGS) $(OBJS_F) $(FLIBS)

$(OBJ_DIR)$(PS)%.o: %.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

clean:
	$(RM) $(subst $(PS),$(PS1),$(MOD_DIR)$(PS)*.mod)
	$(RM) $(subst $(PS),$(PS1),$(OBJ_DIR)$(PS)*.o)
	$(RM) $(subst $(PS),$(PS1),$(TARGET).exe)
	$(RM) $(subst $(PS),$(PS1),$(TARGET))

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
