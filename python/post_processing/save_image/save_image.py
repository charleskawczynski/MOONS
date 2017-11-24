import dir_funcs as DF
import file_IO as IO
import tec_header_funcs as THF
import tecplot_macro_funcs as TMF

def save_new_image(L_in,root,macro_dir,sub_folder,file_name,
	var_name,var_num,image_width_png,image_width_eps,field_num,
	N_approx_contour_levels,plane,x_range,y_range,n_data_sets,PS):
	L = L_in
	fig_file = sub_folder+PS+file_name
	print('fig_file = '+str(fig_file.replace(root,'')))

	L = TMF.extract_plane(L,plane)
	L = TMF.set_plot_type_dimension(L,2)
	a = DF.adj_given_dir(plane)
	if plane==3:
		L = TMF.set_2D_axis_var(L,1,a[0])
		L = TMF.set_2D_axis_var(L,2,a[1])
	else:
		L = TMF.set_2D_axis_var(L,1,a[1])
		L = TMF.set_2D_axis_var(L,2,a[0])

	L = TMF.show_hide_mesh(L,False)
	L = TMF.show_hide_contour_all(L,True)

	for i in range(1,n_data_sets+2):
		L = TMF.show_hide_contour(L,i,False)
		L = TMF.set_contour_lines_type(L,i,'LINES')
		L = TMF.set_contour_lines_color(L,i,'BLACK')
		L = TMF.show_hide_edge(L,i,False)
		L = TMF.set_edge_color(L,i)

	L = TMF.show_hide_contour(L,1+n_data_sets,True)
	if n_data_sets==3:
		L = TMF.show_hide_edge(L,1,True)
		L = TMF.show_hide_edge(L,2,True)
		L = TMF.show_hide_edge(L,3,True)
		L = TMF.activate_field(L,2)
		L = TMF.activate_field(L,3)
	else:
		L = TMF.show_hide_edge(L,1,True)

	L = TMF.switch_var(L,var_num)
	L = TMF.set_text_box_top_right(L,var_name,50)
	L = TMF.deactivate_field(L,field_num)
	L = TMF.reset_N_approx_contour_levels(L,N_approx_contour_levels)
	L = TMF.activate_field(L,field_num)
	L = TMF.delete_zero_level_contour(L)

	L = TMF.set_label_size(L,7)
	L = TMF.set_title_size(L,7)
	L = TMF.set_label_offset(L,[1,1])
	L = TMF.set_axis_range(L,x_range,y_range)
	# L = TMF.set_x_label_align(L,50)
	# L = TMF.set_x_label_align(L,50)
	L = TMF.add_mesh_labels(L)
	L = TMF.save_image(L,fig_file)
	L = TMF.delete_text_box_top_right(L)
	L = TMF.set_plot_type_dimension(L,3)
	L = TMF.delete_zone(L,1+n_data_sets)
	return L

