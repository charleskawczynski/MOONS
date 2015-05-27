# ****************** OS SETTINGS ********************
ifdef SystemRoot
    PATHSEP2 = \\
else
    PATHSEP2 = /
endif
PS = $(strip $(PATHSEP2))
# PS1 is needed for gmake clean
PS1 = $(strip \)
extf90 = $(strip .f90)

ifdef SystemRoot
	RM = del
else
	RM = rm
endif

# ************* DIRECTORY SETTINGS ******************
SRC_DIR = C:$(PS)Users$(PS)Charlie$(PS)Documents$(PS)GitHub$(PS)MOONS
TARGET_DIR = C:$(PS)Users$(PS)Charlie$(PS)Desktop$(PS)MOONS_symmetry

MOD_DIR = $(TARGET_DIR)$(PS)mod
OBJ_DIR = $(TARGET_DIR)$(PS)obj
LIB_DIR = $(SRC_DIR)$(PS)lib

# **************** INCLUDE PATHS ********************
VPATH =\
	$(TARGET_DIR) \
	$(SRC_DIR)$(PS)unit_testing \
	$(SRC_DIR)$(PS)lib \
	$(SRC_DIR)$(PS)user \
	$(SRC_DIR)$(PS)static \
	$(SRC_DIR)$(PS)computations \
	$(SRC_DIR)$(PS)solvers$(PS)steadyState \
	$(SRC_DIR)$(PS)solvers$(PS)transient \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)energy \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)momentum \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)induction

# ************** COMPILER & STANDARD ****************
FC      = gfortran
# FCFLAGS = -std=f95

# ****************** DEFAULT FLAGS ******************
FCFLAGS = -J"$(MOD_DIR)" -fimplicit-none -Wuninitialized


# ******************** LIBRARIES ********************
# FLIBS = $(LIB_DIR)$(PS)tecio.lib

# ****************** DEBUGGING **********************
# CPU time ~ Debugging level
# FCFLAGS += -g
# FCFLAGS += -g -Wall
# FCFLAGS += -g -Wall -Wextra
# FCFLAGS += -g -Wall -Wextra -fbacktrace
FCFLAGS += -g -Wall -Wextra -fbacktrace -fcheck=all
# FCFLAGS += -g -Wall -Wextra -pedantic -fbacktrace -Q

# **************** OPTIMIZATION *********************
# Speed-up ~ Optimization level
FCFLAGS += -O0
# FCFLAGS += -O1
# FCFLAGS += -O2
# FCFLAGS += -O3
# FCFLAGS += -O4
# FCFLAGS += -O5
# **************** PARALLELIZATION ******************
FCFLAGS += -fopenmp

# ****************** PROFILING **********************
# To profile, run simulation with the flag:
# FCFLAGS += -pg
# Then run the following command:
# gprof MOONS.exe > timeReport.txt

# *********** PRE-PROCESSOR DIRECTIVES **************
FCFLAGS += -cpp

# *********** PRE-PROCESSOR DEFINITIONS *************
# Precision
# FCFLAGS += -D_SINGLE_PRECISION_
FCFLAGS += -D_DOUBLE_PRECISION_
# FCFLAGS += -D_QUAD_PRECISION_

FCFLAGS += -D_PARALLELIZE_Jacobi_
FCFLAGS += -D_PARALLELIZE_SOR_
FCFLAGS += -D_PARALLELIZE_VECTOR_FIELD_
FCFLAGS += -D_PARALLELIZE_SCALAR_FIELD_

# Code verification
# FCFLAGS += -D_EXPORT_Jacobi_CONVERGENCE_
# FCFLAGS += -D_EXPORT_SOR_CONVERGENCE_
# FCFLAGS += -D_EXPORT_ADI_CONVERGENCE_
# FCFLAGS += -D_EXPORT_MG_CONVERGENCE_

# check grid is performed every time a grid is initialized
# FCFLAGS += -D_CHECK_GRID_

# Internal debugging
# FCFLAGS += -D_DEBUG_DELOPS_
# FCFLAGS += -D_DEBUG_VECTOROPS_
FCFLAGS += -D_DEBUG_DEL_
FCFLAGS += -D_DEBUG_INTERP_
# FCFLAGS += -D_DEBUG_VECTOR_
# FCFLAGS += -D_DEBUG_SCALAR_ not yet implemented
# FCFLAGS += -D_CHECK_SYMMETRY_
# FCFLAGS += -D_CHECK_SYMMETRY_DELOPS_
# FCFLAGS += -D_CHECK_SYMMETRY_POISSON_


TARGET = $(TARGET_DIR)$(PS)testSymmetry

# **************** SOURCE FILES *********************

SRCS_F =\
	$(SRC_DIR)$(PS)static$(PS)scalarField.f90 \
	$(SRC_DIR)$(PS)static$(PS)vectorField.f90 \
	$(SRC_DIR)$(PS)static$(PS)IO_tools.f90 \
	$(SRC_DIR)$(PS)static$(PS)IO_tecplotHeaders.f90 \
	$(SRC_DIR)$(PS)static$(PS)IO_scalarBase.f90 \
	$(SRC_DIR)$(PS)static$(PS)IO_vectorBase.f90 \
	$(SRC_DIR)$(PS)static$(PS)coordinates.f90 \
	$(SRC_DIR)$(PS)static$(PS)gridGenTools.f90 \
	$(SRC_DIR)$(PS)static$(PS)grid.f90 \
	$(SRC_DIR)$(PS)static$(PS)gridGen.f90 \
	$(SRC_DIR)$(PS)static$(PS)IO_scalarFields.f90 \
	$(SRC_DIR)$(PS)static$(PS)IO_vectorFields.f90 \
	$(SRC_DIR)$(PS)static$(PS)IO_transientFields.f90 \
	$(SRC_DIR)$(PS)static$(PS)IO_auxiliary.f90 \
	$(SRC_DIR)$(PS)static$(PS)version.f90 \
	$(SRC_DIR)$(PS)user$(PS)simParams.f90 \
	$(SRC_DIR)$(PS)static$(PS)solverSettings.f90 \
	$(SRC_DIR)$(PS)static$(PS)myTime.f90 \
	$(SRC_DIR)$(PS)computations$(PS)myError.f90 \
	$(SRC_DIR)$(PS)user$(PS)griddata.f90 \
	$(SRC_DIR)$(PS)static$(PS)BCs.f90 \
	$(SRC_DIR)$(PS)static$(PS)vectorBCs.f90 \
	$(SRC_DIR)$(PS)user$(PS)rundata.f90 \
	$(SRC_DIR)$(PS)computations$(PS)stencils.f90 \
	$(SRC_DIR)$(PS)computations$(PS)del.f90 \
	$(SRC_DIR)$(PS)computations$(PS)delVC.f90 \
	$(SRC_DIR)$(PS)computations$(PS)interpOps.f90 \
	$(SRC_DIR)$(PS)computations$(PS)ops_aux.f90 \
	$(SRC_DIR)$(PS)computations$(PS)ops_discrete.f90 \
	$(SRC_DIR)$(PS)computations$(PS)ops_physics.f90 \
	$(SRC_DIR)$(PS)computations$(PS)ops_discrete_complex.f90 \
	$(SRC_DIR)$(PS)computations$(PS)applyBCs.f90 \
	$(SRC_DIR)$(PS)static$(PS)probe_transient.f90 \
	$(SRC_DIR)$(PS)static$(PS)probe_base.f90 \
	$(SRC_DIR)$(PS)static$(PS)probe_derived.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)myTriSolver.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)steadyState$(PS)myJacobi.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)steadyState$(PS)mySOR.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)myADI.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)steadyState$(PS)MG_tools.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)steadyState$(PS)myMG.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)steadyState$(PS)myPoisson.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)energy$(PS)initializeTBCs.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)energy$(PS)initializeTfield.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)energy$(PS)initializeK.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)induction$(PS)initializeBBCs.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)induction$(PS)initializeBfield.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)induction$(PS)initializeSigmaMu.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)momentum$(PS)initializeUBCs.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)momentum$(PS)initializeUfield.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)energy$(PS)energySolver.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)induction$(PS)inductionSolver.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)momentum$(PS)momentumSolver.f90 \
	$(SRC_DIR)$(PS)solvers$(PS)transient$(PS)MHDSolver.f90 \
	$(SRC_DIR)$(PS)user$(PS)MOONS.f90 \
	$(SRC_DIR)$(PS)unit_testing$(PS)symmetry.f90 \
	$(TARGET_DIR)$(PS)parametricStudy.f90

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
	cd $(TARGET_DIR) && gmake run

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
