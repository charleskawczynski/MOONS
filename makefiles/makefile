# **************** RUN CONFIGURATION *******************
RELEASE = 'DEBUG'
# RELEASE = 'OPTIMIZE'
# RELEASE = 'PROFILE'
USE_OPEN_MP = 'TRUE'
PARALLELIZE_3D_OPS = 'TRUE'
# PARALLELIZE_2D_OPS = 'TRUE'
# PARALLELIZE_1D_OPS = 'TRUE'
# FORMAT_OUTPUT = 'FORMATTED'
# ************* CLUSTER CONFIGURATION ******************
# On Hoffman / DOE, run "module load gcc/4.9.3" in terminal before compile
# Submitting on Hoffman run "job.q"
CLUSTER = 'PC_CK'
# CLUSTER = 'Workstation_CK'
# CLUSTER = 'Hoffman2'
# CLUSTER = 'DOE'
# ************* MAKE CONFIGURATION ******************
MKE = gmake
# MKE = make
# ****************** OS SETTINGS ********************
ifdef SystemRoot
	OS_USED = 'OS_WINDOWS'
else
	OS_USED = 'OS_LINUX'
endif

ifeq ($(OS_USED),'OS_WINDOWS')
	RM = del
	PS = $(strip /)
	# BEWARE: The double back-slash has caused an issue with compilation on windows gcc 4.9.2
	# PS = $(strip \\)
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

# ************* DIRECTORY SETTINGS ******************
SRC_DIR = ..$(PS)code$(PS)handwritten
SRC_DIR_GENERATED = ..$(PS)code$(PS)generated_code
SRC_DIR_PRE_GENERATED = ..$(PS)code$(PS)pre_generated

ifeq ($(CLUSTER),'PC_CK')
	TARGET_DIR = C:$(PS)Users$(PS)Charlie$(PS)Documents$(PS)MOONS$(PS)SIMS$(PS)MOONS9
endif

ifeq ($(CLUSTER),'Workstation_CK')
	TARGET_DIR = C:$(PS)Users$(PS)charl$(PS)Documents$(PS)MOONS$(PS)SIMS$(PS)BC_full$(PS)Shatrov_inspired$(PS)M4
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
FCFLAGS = -J"$(MOD_DIR)"
FCFLAGS += -fimplicit-none
FCFLAGS += -Wuninitialized
FCFLAGS += -cpp
# FCFLAGS += -std=legacy
FCFLAGS += -std=gnu
# FCFLAGS += -v
# FCFLAGS += -E -dM
# FCFLAGS += -std=f95
# FCFLAGS += -std=f2003
# FCFLAGS += -std=2008

# Precision (most files contain this)
# FCFLAGS += -D_SINGLE_PRECISION_
FCFLAGS += -D_DOUBLE_PRECISION_
# FCFLAGS += -D_QUAD_PRECISION_

# FCFLAGS += -D_MATRIX_FREE_
# FCFLAGS += -D_OPTIMIZE_IO_TOOLS_

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

ifeq ($(FORMAT_OUTPUT),'FORMATTED')
	FCFLAGS += -D_FORMAT_OUTPUT_
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
ifeq ($(CLUSTER),'Hoffman2')
	FC_DEBUG += -Wall -Wextra -fbacktrace -O0
else
	FC_DEBUG += -Wall -Wextra -fbacktrace -O0 -Og
endif
# FC_DEBUG += -Wall -Wextra -fbacktrace

# Suppress temporrary array warning:
# FC_DEBUG += -Wno-array-temporaries
ifneq ($(CLUSTER),'Hoffman2')
	FC_DEBUG += -fcheck=bounds -fcheck=do -fcheck=mem -fcheck=pointer -fcheck=recursion
endif

# These flags are for rigorous mesh / stencil checking
# FC_DEBUG += -D_DEBUG_COORDINATES_
# FC_DEBUG += -D_DEBUG_COORDINATE_STRETCH_PARAM_MATCH_
# FC_DEBUG += -D_DEBUG_RK_PARAMS_

# FC_DEBUG += -D_DEBUG_IO_EXPORT_
# FC_DEBUG += -D_DEBUG_EMBEDEXTRACT_
# FC_DEBUG += -D_DEBUG_DATA_LOCATION_
# FC_DEBUG += -D_DEBUG_ARRAY_
# FC_DEBUG += -D_DEBUG_GF_
# FC_DEBUG += -D_DEBUG_BF_
# FC_DEBUG += -D_DEBUG_OPS_NORMS_
# FC_DEBUG += -D_DEBUG_SF_
# FC_DEBUG += -D_DEBUG_VF_
# FC_DEBUG += -D_DEBUG_TF_
# FC_DEBUG += -D_DEBUG_DEL_
# FC_DEBUG += -D_DEBUG_DISCRETE_OPS_
# FC_DEBUG += -D_DEBUG_INTERP_
# FC_DEBUG += -D_DEBUG_BOUNDARY_CONDITIONS_
# FC_DEBUG += -D_DEBUG_APPLY_BCS_
# FC_DEBUG += -D_DEBUG_IO_TOOLS_
# FC_DEBUG += -D_DEBUG_GF_NORMS_

# **************** OPTIMIZE/PARALLELIZE *********************
ifeq ($(USE_OPEN_MP),'TRUE')
	FCFLAGS += -fopenmp
endif

FC_OPTIMIZE += -g -O3

ifeq ($(PARALLELIZE_3D_OPS),'TRUE')
	FC_OPTIMIZE += -D_PARALLELIZE_GF_
	FC_OPTIMIZE += -D_PARALLELIZE_DEL_
	FC_OPTIMIZE += -D_PARALLELIZE_INTERP_
	FC_OPTIMIZE += -D_PARALLELIZE_CROSS_PRODUCT_GF_
	FC_OPTIMIZE += -D_PARALLELIZE_Jacobi_
	FC_OPTIMIZE += -D_PARALLELIZE_SOR_
	FC_OPTIMIZE += -D_PARALLELIZE_EMBEDEXTRACT_
	FC_OPTIMIZE += -D_PARALLELIZE_GF_NORMS_
	FC_OPTIMIZE += -D_PARALLELIZE_PROLONGATE_
endif

ifeq ($(PARALLELIZE_2D_OPS),'TRUE')
	# Choose 1 of 2:
	# FC_OPTIMIZE += -D_PARALLELIZE_GF_PLANE_
	FC_OPTIMIZE += -D_PARALLELIZE_BF_PLANE_
	# Choose 1 of 2:
	# FC_OPTIMIZE += -D_PARALLELIZE_APPLY_BCS_FACES_
	FC_OPTIMIZE += -D_PARALLELIZE_APPLY_BCS_FACES_RAW_
	# Independent:
	FC_OPTIMIZE += -D_PARALLELIZE_GF_PLANE_SUM_
endif

ifeq ($(PARALLELIZE_1D_OPS),'TRUE')
	# 1D OPERATIONS (likely not worth it)
	# FC_OPTIMIZE += -D_PARALLELIZE_APPLY_BCS_EDGES_RAW_
	# FC_OPTIMIZE += -D_PARALLELIZE_APPLY_BCS_CORNERS_RAW_
endif

# AUXILIARY OPERATIONS
# FC_OPTIMIZE += -D_PARALLELIZE_RICHARDSONEXTRAPOLATION_

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

ifeq ($(RELEASE),'DEBUG')
	FCFLAGS += $(FC_DEBUG)
endif
ifeq ($(RELEASE),'PROFILE')
	FCFLAGS += $(FC_PROFILE)
endif
ifeq ($(RELEASE),'OPTIMIZE')
	FCFLAGS += $(FC_OPTIMIZE)
endif

TARGET = $(TARGET_DIR)$(PS)MOONS.exe

include vpath_pre_generated.make
include vpath_generated.make
include vpath_handwritten.make

include src_all.make

SRCS_F += $(TARGET_DIR)$(PS)main.f90

# **************** DO NOT EDIT BELOW HERE *********************
# **************** DO NOT EDIT BELOW HERE *********************
# **************** DO NOT EDIT BELOW HERE *********************

OBJS_F = $(patsubst %.f90,$(OBJ_DIR)$(PS)%.o,$(notdir $(SRCS_F)))

$(info ************************************)
$(info ********** BUILDING MOONS **********)
$(info ************************************)
$(info $(OBJ_DIR))
$(info $(SRC_DIR_PRE_GENERATED))
$(info $(SRC_DIR_GENERATED))
$(info $(SRC_DIR))

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
	@echo | gcc -dM -E -
	@echo "Object files:"
	@echo $(OBJS_F)
	@echo " "
	@echo "Compiler          : $(FC)"
	@echo "Library directory : $(LIB_DIR)"
	@echo "Target directory  : $(TARGET_DIR)"
	@echo "Modules directory : $(MOD_DIR)"
	@echo "Object directory  : $(OBJ_DIR)"
	@echo " "