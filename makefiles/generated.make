VPATH +=\
	$(SRC_DIR_GENERATED)$(PS)dir_tree\
	$(SRC_DIR_GENERATED)$(PS)grid\
	$(SRC_DIR_GENERATED)$(PS)mesh_params\
	$(SRC_DIR_GENERATED)$(PS)probe\
	$(SRC_DIR_GENERATED)$(PS)stitches\

SRCS_F+=\
	$(SRC_DIR_GENERATED)$(PS)stitches$(PS)stitch.f90\
	$(SRC_DIR_GENERATED)$(PS)stitches$(PS)stitch_face.f90\
	$(SRC_DIR_GENERATED)$(PS)dir_tree$(PS)path.f90\
	$(SRC_DIR_GENERATED)$(PS)dir_tree$(PS)dir_group.f90\
	$(SRC_DIR_GENERATED)$(PS)dir_tree$(PS)dir_tree.f90\
	$(SRC_DIR_GENERATED)$(PS)probe$(PS)probe.f90\
	$(SRC_DIR_GENERATED)$(PS)mesh_params$(PS)mesh_quality_params.f90\
	$(SRC_DIR_GENERATED)$(PS)mesh_params$(PS)segment.f90\
	$(SRC_DIR_GENERATED)$(PS)mesh_params$(PS)mesh_params.f90\
	$(SRC_DIR_GENERATED)$(PS)grid$(PS)array.f90\
	$(SRC_DIR_GENERATED)$(PS)grid$(PS)sparse.f90\
	$(SRC_DIR_GENERATED)$(PS)grid$(PS)coordinates.f90\
	$(SRC_DIR_GENERATED)$(PS)grid$(PS)grid.f90\
