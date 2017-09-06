VPATH +=\
	$(SRC_DIR_GENERATED)$(PS)apply_face_BC_op\
	$(SRC_DIR_GENERATED)$(PS)block\
	$(SRC_DIR_GENERATED)$(PS)block_field\
	$(SRC_DIR_GENERATED)$(PS)boundary_conditions\
	$(SRC_DIR_GENERATED)$(PS)dir_tree\
	$(SRC_DIR_GENERATED)$(PS)grid\
	$(SRC_DIR_GENERATED)$(PS)grid_field\
	$(SRC_DIR_GENERATED)$(PS)mesh_params\
	$(SRC_DIR_GENERATED)$(PS)physical_domain\
	$(SRC_DIR_GENERATED)$(PS)probe\
	$(SRC_DIR_GENERATED)$(PS)procedure_array\
	$(SRC_DIR_GENERATED)$(PS)sim_params\
	$(SRC_DIR_GENERATED)$(PS)stitches\
	$(SRC_DIR_GENERATED)$(PS)stop_clock\
	$(SRC_DIR_GENERATED)$(PS)sub_domain\
	$(SRC_DIR_GENERATED)$(PS)var_set\

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
	$(SRC_DIR_GENERATED)$(PS)var_set$(PS)RK_params.f90\
	$(SRC_DIR_GENERATED)$(PS)var_set$(PS)solver_settings.f90\
	$(SRC_DIR_GENERATED)$(PS)var_set$(PS)matrix_free_params.f90\
	$(SRC_DIR_GENERATED)$(PS)var_set$(PS)time_marching_params.f90\
	$(SRC_DIR_GENERATED)$(PS)var_set$(PS)iter_solver_params.f90\
	$(SRC_DIR_GENERATED)$(PS)var_set$(PS)export_line.f90\
	$(SRC_DIR_GENERATED)$(PS)var_set$(PS)export_lines.f90\
	$(SRC_DIR_GENERATED)$(PS)var_set$(PS)export_plane.f90\
	$(SRC_DIR_GENERATED)$(PS)var_set$(PS)export_planes.f90\
	$(SRC_DIR_GENERATED)$(PS)var_set$(PS)export_field.f90\
	$(SRC_DIR_GENERATED)$(PS)var_set$(PS)var.f90\
	$(SRC_DIR_GENERATED)$(PS)var_set$(PS)var_set.f90\
	$(SRC_DIR_GENERATED)$(PS)stop_clock$(PS)unit_conversion.f90\
	$(SRC_DIR_GENERATED)$(PS)stop_clock$(PS)clock.f90\
	$(SRC_DIR_GENERATED)$(PS)stop_clock$(PS)stop_clock.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)stats_period.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)time_statistics_params.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)mirror_props.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)export_logicals.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)geometry_props.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)equation_term.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)energy_terms.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)momentum_terms.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)induction_terms.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)export_frequency_params.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)export_frequency.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)flow_control_logicals.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)sim_config_params.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)dimensionless_params.f90\
	$(SRC_DIR_GENERATED)$(PS)sim_params$(PS)sim_params.f90\
	$(SRC_DIR_GENERATED)$(PS)grid_field$(PS)grid_field.f90\
	$(SRC_DIR_GENERATED)$(PS)sub_domain$(PS)overlap.f90\
	$(SRC_DIR_GENERATED)$(PS)sub_domain$(PS)sub_domain.f90\
	$(SRC_DIR_GENERATED)$(PS)sub_domain$(PS)index_2D.f90\
	$(SRC_DIR_GENERATED)$(PS)sub_domain$(PS)face_SD.f90\
	$(SRC_DIR_GENERATED)$(PS)physical_domain$(PS)physical_sub_domain.f90\
	$(SRC_DIR_GENERATED)$(PS)physical_domain$(PS)physical_domain.f90\
	$(SRC_DIR_GENERATED)$(PS)apply_face_BC_op$(PS)apply_face_BC_op.f90\
	$(SRC_DIR_GENERATED)$(PS)apply_face_BC_op$(PS)plane_op.f90\
	$(SRC_DIR_GENERATED)$(PS)procedure_array$(PS)single_procedure.f90\
	$(SRC_DIR_GENERATED)$(PS)procedure_array$(PS)single_procedure_plane_op.f90\
	$(SRC_DIR_GENERATED)$(PS)procedure_array$(PS)procedure_array.f90\
	$(SRC_DIR_GENERATED)$(PS)procedure_array$(PS)procedure_array_plane_op.f90\
	$(SRC_DIR_GENERATED)$(PS)boundary_conditions$(PS)BC_logicals.f90\
	$(SRC_DIR_GENERATED)$(PS)boundary_conditions$(PS)single_boundary.f90\
	$(SRC_DIR_GENERATED)$(PS)boundary_conditions$(PS)boundary.f90\
	$(SRC_DIR_GENERATED)$(PS)boundary_conditions$(PS)boundary_conditions.f90\
	$(SRC_DIR_GENERATED)$(PS)block$(PS)block.f90\
	$(SRC_DIR_GENERATED)$(PS)block_field$(PS)block_field.f90\
