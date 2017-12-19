import numpy as np
import dir_funcs as DF
import file_IO as IO
import tec_header_funcs as THF
import colormap_macro as CM

def set_preserve_order(seq):
	seen = set()
	seen_add = seen.add
	return [x for x in seq if not (x in seen or seen_add(x))]

def init_macro(): return ['#!MC 1120']
def switch_var(L,var_num):
	return L+["$!GLOBALCONTOUR 1  VAR = "+str(var_num)]

def activate_field(L,field_num):
	return L+['$!ACTIVEFIELDMAPS += ['+str(field_num)+']']

def deactivate_field(L,field_num):
	return L+['$!ACTIVEFIELDMAPS -= ['+str(field_num)+']']

def hide_legend(L):
	return L+['$!GLOBALLINEPLOT LEGEND{SHOW = NO}']

def set_plot_type_dimension(L,dimension):
	if dimension==1:
		temp = ['$!PLOTTYPE = XYLINE']
	else:
		temp = ['$!PLOTTYPE = CARTESIAN'+str(dimension)+'D']
	return L+temp

def delete_zone(L,zone_num):
	return L+['$!DELETEZONES  ['+str(zone_num)+']']

def delete_map(L,map_num):
	return L+['$!DELETELINEMAPS  ['+str(map_num)+']']

def set_2D_axis_var(L,axis_num,var_num):
	s = DF.XYZ_given_dir(axis_num)
	return L+['$!TWODAXIS '+s+'DETAIL{VARNUM = '+str(var_num)+'}']

def show_hide_contour_all(L,TF):
	if TF: L.append('$!FIELDLAYERS SHOWCONTOUR = YES')
	else: L.append('$!FIELDLAYERS SHOWCONTOUR = NO')
	return L

def show_hide_symbols_all(L,TF):
	if TF: L.append('$!LINEPLOTLAYERS SHOWSYMBOLS = YES')
	else: L.append('$!LINEPLOTLAYERS SHOWSYMBOLS = NO')
	return L

def set_label_show(L):
	return L+['$!GLOBALCONTOUR 1  LABELS{SHOW = YES}']

def set_contour_lines_type(L,field_num,line_type):
	return L+['$!FIELDMAP ['+str(field_num)+']  CONTOUR{CONTOURTYPE = '+line_type+'}']

def set_y_axis_var(L,map_num,var_num):
	return L+['$!LINEMAP ['+str(map_num)+']  ASSIGN{YAXISVAR = '+str(var_num)+'}']

# $!FIELDMAP [3]  EDGELAYER{COLOR = BLACK}
# $!FIELDMAP [3]  EDGELAYER{SHOW = NO}
# $!FIELDMAP [2]  EDGELAYER{SHOW = NO}
# $!FIELDMAP [2]  EDGELAYER{SHOW = YES}
# $!FIELDMAP [3]  CONTOUR{CONTOURTYPE = LINES}
# $!FIELDMAP [3]  CONTOUR{COLOR = BLACK}

def set_contour_lines_color(L,field_num,color):
	return L+['$!FIELDMAP ['+str(field_num)+']  CONTOUR{COLOR = '+color+'}']

def show_hide_mesh(L,TF):
	if TF: L.append('$!FIELDLAYERS SHOWMESH = YES')
	else: L.append('$!FIELDLAYERS SHOWMESH = NO')
	return L

def show_hide_contour(L,field_num,TF):
	if TF: L.append('$!FIELDMAP ['+str(field_num)+']  CONTOUR{SHOW = YES}')
	else: L.append('$!FIELDMAP ['+str(field_num)+']  CONTOUR{SHOW = NO}')
	return L

def show_hide_edge(L,field_num,TF):
	if TF: L.append('$!FIELDMAP ['+str(field_num)+']  EDGELAYER{SHOW = YES}')
	else: L.append('$!FIELDMAP ['+str(field_num)+']  EDGELAYER{SHOW = NO}')
	return L

def set_edge_color(L,field_num):
	return L+['$!FIELDMAP ['+str(field_num)+']  EDGELAYER{COLOR = BLACK}']

def hide_border(L):
	return L+['$!FRAMELAYOUT SHOWBORDER = NO']

def read_data_set(L,file_name):
	file_name_buffer = '"'+file_name+'"'
	H = THF.get_header_list(file_name)
	var_list = THF.get_variable_list(H)
	var_list_s = ' '.join(var_list)
	# print(' ------------------------ VERY EXPENSIVE CALL ------------------------ ')
	# print(' ------------------------ VERY EXPENSIVE CALL ------------------------ ')
	print(' ------------------------ VERY EXPENSIVE CALL ------------------------ ')
	L.append("$!READDATASET  '"+file_name_buffer+" '")
	L.append("READDATAOPTION = NEW")
	L.append("RESETSTYLE = YES")
	L.append("INCLUDETEXT = NO")
	L.append("INCLUDEGEOM = NO")
	L.append("INCLUDECUSTOMLABELS = NO")
	L.append("VARLOADMODE = BYNAME")
	L.append("ASSIGNSTRANDIDS = YES")
	L.append("INITIALPLOTTYPE = CARTESIAN3D")
	VARNAMELIST = "VARNAMELIST = '"+var_list_s+"'"
	L.append(VARNAMELIST)
	return (L,var_list)

def set_equation(L,equation_expression,zone = False):
	if zone:
		L.append("$!ALTERDATA  ["+str(zone)+"]")
	else:
		L.append("$!ALTERDATA ")
	L.append("EQUATION = '"+equation_expression+"'")
	return L

def set_style(L,field_num,y_axis_var_num,legend_name):
	L.append('$!ACTIVELINEMAPS += ['+str(field_num)+']')
	L.append('$!LINEMAP ['+str(field_num)+"]  NAME = '"+legend_name+"'")
	L.append('$!LINEMAP ['+str(field_num)+"]  ASSIGN{YAXISVAR = "+str(y_axis_var_num)+"}")
	L.append('$!LINEMAP ['+str(field_num)+"]  ASSIGN{ZONE = "+str(field_num)+"}")
	return L

def set_active_field(L,field_num,TF):
	if TF: L.append('$!ACTIVELINEMAPS += ['+str(field_num)+']')
	else:  L.append('$!ACTIVELINEMAPS -= ['+str(field_num)+']')
	return L

def new_line(L,field_num,x_var_num,y_var_num,zone_num,legend_name):
	# $!LINEMAP [10]  INDICES{IJKLINES = I}
	L.append('$!CREATELINEMAP ')
	L.append('$!ACTIVELINEMAPS += ['+str(field_num)+']')
	L.append('$!LINEMAP ['+str(field_num)+"]  NAME = '"+legend_name+"'")
	L.append('$!LINEMAP ['+str(field_num)+"]  ASSIGN{XAXISVAR = "+str(x_var_num)+"}")
	L.append('$!LINEMAP ['+str(field_num)+"]  ASSIGN{YAXISVAR = "+str(y_var_num)+"}")
	L.append('$!LINEMAP ['+str(field_num)+"]  ASSIGN{ZONE = "+str(zone_num)+"}")
	field_num=field_num+1
	return L

def append_data_set(L,file_name,var_list_existing):
	file_name_buffer = '"'+file_name+'"'
	H = THF.get_header_list(file_name)
	var_list_new = THF.get_variable_list(H)
	var_list = list(set_preserve_order(var_list_existing+var_list_new))
	var_list_s = ' '.join(var_list)
	L.append("$!READDATASET  '"+file_name_buffer+" '")
	L.append("READDATAOPTION = APPEND")
	L.append("RESETSTYLE = NO")
	L.append("INCLUDETEXT = NO")
	L.append("INCLUDEGEOM = NO")
	L.append("INCLUDECUSTOMLABELS = NO")
	L.append("VARLOADMODE = BYNAME")
	L.append("ASSIGNSTRANDIDS = YES")
	L.append("INITIALPLOTTYPE = CARTESIAN3D")
	VARNAMELIST = "VARNAMELIST = '"+var_list_s+"'"
	L.append(VARNAMELIST)
	return (L,var_list)

def save_image(L,file_name_wo_ext):
	file_name = file_name_wo_ext
	image_width_eps = 1275
	image_width_png = 600
	L.append("$!EXPORTSETUP EXPORTFORMAT = "+'PNG')
	L.append("$!EXPORTSETUP IMAGEWIDTH = "+str(image_width_png))
	L.append("$!EXPORTSETUP EXPORTFNAME = '"+file_name+'.png'+"'")
	L.append("$!EXPORT ")
	L.append("  EXPORTREGION = CURRENTFRAME")
	L.append("$!EXPORTSETUP EXPORTFORMAT = "+'EPS')
	L.append("$!EXPORTSETUP IMAGEWIDTH = "+str(image_width_eps))
	L.append("$!EXPORTSETUP EXPORTFNAME = '"+file_name+'.eps'+"'")
	L.append("$!EXPORT ")
	L.append("  EXPORTREGION = CURRENTFRAME")
	save_style(L,file_name_wo_ext)
	return L

def save_style(L,file_name_wo_ext):
	L.append('$!WRITESTYLESHEET  "'+file_name_wo_ext+'.sty"')
	L.append('  INCLUDEPLOTSTYLE = YES')
	L.append('  INCLUDETEXT = YES')
	L.append('  INCLUDEGEOM = YES')
	L.append('  INCLUDEAUXDATA = YES')
	L.append('  INCLUDESTREAMPOSITIONS = YES')
	L.append('  INCLUDECONTOURLEVELS = YES')
	L.append('  INCLUDEFACTORYDEFAULTS = NO')
	L.append('  USERELATIVEPATHS = YES')
	L.append('  COMPRESS = NO')
	return L

def reset_N_approx_contour_levels(L,N_approx_contour_levels):
	L.append("$!CONTOURLEVELS RESETTONICE")
	L.append("  CONTOURGROUP = 1")
	L.append("  APPROXNUMVALUES = "+str(N_approx_contour_levels))
	return L

def set_line_props(L,field_num):
	L.append('$!LINEMAP ['+str(field_num)+']  LINES{COLOR = BLACK}')
	L.append('$!LINEMAP ['+str(field_num)+']  LINES{LINETHICKNESS = 0.2}')
	return L

def set_symbol_wrapper(L,field_num,symbol_num_abs):
	return set_symbol(L,field_num,(symbol_num_abs-1)%(2*7)+1)

def set_symbol(L,field_num,symbol_num):
	print('symbol_num = '+str(symbol_num))
	L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{COLOR = BLACK}')
	L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{FILLCOLOR = BLACK}')
	L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{FILLCOLOR = BLACK}')
	L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{SIZE = 2}')
	L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{LINETHICKNESS = 0.2}')
	# L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{SKIPMODE = BYFRAMEUNITS}')
	# L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{SKIPPING = 10}')

	L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{SYMBOLSHAPE{ISASCII = NO}}')

	s = '$!LINEMAP ['+str(field_num)+']  SYMBOLS{SYMBOLSHAPE{GEOMSHAPE = '
	s1 = '$!LINEMAP ['+str(field_num)+']  SYMBOLS{FILLMODE = NONE}'
	s2 = '$!LINEMAP ['+str(field_num)+']  SYMBOLS{FILLMODE = USELINECOLOR}'
	if symbol_num==1: L.append(s+'CIRCLE}}'); L.append(s1)
	elif symbol_num==2: L.append(s+'SQUARE}}'); L.append(s1)
	elif symbol_num==3: L.append(s+'DEL}}'); L.append(s1)
	elif symbol_num==4: L.append(s+'GRAD}}'); L.append(s1)
	elif symbol_num==5: L.append(s+'RTRI}}'); L.append(s1)
	elif symbol_num==6: L.append(s+'LTRI}}'); L.append(s1)
	elif symbol_num==7: L.append(s+'DIAMOND}}'); L.append(s1)
	elif symbol_num==8: L.append(s+'CIRCLE}}'); L.append(s2)
	elif symbol_num==9: L.append(s+'SQUARE}}'); L.append(s2)
	elif symbol_num==10: L.append(s+'DEL}}'); L.append(s2)
	elif symbol_num==11: L.append(s+'GRAD}}'); L.append(s2)
	elif symbol_num==12: L.append(s+'RTRI}}'); L.append(s2)
	elif symbol_num==13: L.append(s+'LTRI}}'); L.append(s2)
	elif symbol_num==14: L.append(s+'DIAMOND}}'); L.append(s2)
	else: raise NameError('Bad symbol_num in set_symbol in tecplot_macro_funcs.py')
	return L

def set_symbol_based_on_Rem(L,field_num,Re_m):
	print('Re_m = '+str(Re_m))
	L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{COLOR = BLACK}')
	L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{FILLCOLOR = BLACK}')
	L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{FILLCOLOR = BLACK}')
	L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{SIZE = 2}')
	L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{LINETHICKNESS = 0.2}')
	# L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{SKIPMODE = BYFRAMEUNITS}')
	# L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{SKIPPING = 10}')

	L.append('$!LINEMAP ['+str(field_num)+']  SYMBOLS{SYMBOLSHAPE{ISASCII = NO}}')

	s = '$!LINEMAP ['+str(field_num)+']  SYMBOLS{SYMBOLSHAPE{GEOMSHAPE = '
	s1 = '$!LINEMAP ['+str(field_num)+']  SYMBOLS{FILLMODE = NONE}'
	s2 = '$!LINEMAP ['+str(field_num)+']  SYMBOLS{FILLMODE = USELINECOLOR}'
	if Re_m==0: L.append(s+'CIRCLE}}'); L.append(s1)
	elif Re_m==1: L.append(s+'SQUARE}}'); L.append(s1)
	elif Re_m==100: L.append(s+'DEL}}'); L.append(s1)
	elif Re_m==200: L.append(s+'GRAD}}'); L.append(s1)
	elif Re_m==300: L.append(s+'RTRI}}'); L.append(s1)
	elif Re_m==400: L.append(s+'LTRI}}'); L.append(s1)
	elif Re_m==500: L.append(s+'DIAMOND}}'); L.append(s1)
	elif Re_m==600: L.append(s+'CIRCLE}}'); L.append(s2)
	elif Re_m==700: L.append(s+'SQUARE}}'); L.append(s2)
	elif Re_m==800: L.append(s+'DEL}}'); L.append(s2)
	elif Re_m==900: L.append(s+'GRAD}}'); L.append(s2)
	elif Re_m==1000: L.append(s+'RTRI}}'); L.append(s2)
	elif Re_m==1500: L.append(s+'LTRI}}'); L.append(s2)
	elif Re_m==2000: L.append(s+'DIAMOND}}'); L.append(s2)
	else: raise NameError('Bad Re_m in set_symbol in tecplot_macro_funcs.py')
	return L

def set_axis_fit(L):
	L.append("$!VIEW AXISFIT")
	L.append("  AXIS = 'Y'")
	L.append("  AXISNUM = 1")
	L.append("$!VIEW AXISFIT")
	L.append("  AXIS = 'X'")
	L.append("  AXISNUM = 1")
	# L.append("$!VIEW AXISMAKECURRENTVALUESNICE")
	# L.append("  AXIS = 'Y'")
	# L.append("  AXISNUM = 1")
	return L

def set_axis_nice_fit(L):
	L.append("$!VIEW AXISNICEFIT")
	L.append("  AXIS = 'X'")
	L.append("  AXISNUM = 1")
	L.append("$!VIEW AXISNICEFIT")
	L.append("  AXIS = 'Y'")
	L.append("  AXISNUM = 1")
	return L

def show_legend(L,position):
	L.append('$!GLOBALLINEPLOT LEGEND{SHOW = YES}')
	L.append('$!GLOBALLINEPLOT LEGEND{BOX{BOXTYPE = NONE}}')
	L.append('$!GLOBALLINEPLOT LEGEND{BOX{MARGIN = 0}}')
	L.append('$!GLOBALLINEPLOT LEGEND{XYPOS{X = '+str(position[0])+'}}')
	L.append('$!GLOBALLINEPLOT LEGEND{XYPOS{Y = '+str(position[1])+'}}')
	return L

def delete_zero_level_contour(L):
	L.append("$!CONTOURLEVELS DELETERANGE")
	L.append("  CONTOURGROUP = 1")
	# L.append("  RANGEMIN = -0.00000000005")
	# L.append("  RANGEMAX = 0.00000000005")
	L.append("  RANGEMIN = 0.49999999995")
	L.append("  RANGEMAX = 0.50000000005")
	return L

def delete_text_box_top_right(L):
	L.append("$!PICK SETMOUSEMODE")
	L.append("  MOUSEMODE = SELECT")
	L.append("$!PICK ADD")
	L.append("  X = 8.64641744548")
	L.append("  Y = 1.54595015576")
	L.append("  CONSIDERSTYLE = YES")
	L.append("$!PICK CLEAR")
	return L
def set_text_box_top_right(L,var_name,size):
	L.append("$!ATTACHTEXT ")
	L.append("  ANCHORPOS")
	L.append("    {")
	L.append("    X = 82.32952578746971")
	L.append("    Y = 81.30841121495327")
	L.append("    }")
	L.append("  TEXTSHAPE")
	L.append("    {")
	L.append("    HEIGHT = "+str(size)+"")
	L.append("    }")
	L.append("  TEXT = '"+var_name+"'")
	return L

def delete_text_box_top_left(L):
	L.append("$!PICK SETMOUSEMODE")
	L.append("  MOUSEMODE = SELECT")
	L.append("$!PICK ADD")
	L.append("  X = 1.8501026694")
	L.append("  Y = 0.766427104723")
	L.append("  CONSIDERSTYLE = YES")
	L.append("$!PICK CLEAR")
	return L
def set_text_box_top_left(L,var_name,size):
	L.append("$!ATTACHTEXT ")
	L.append("  ANCHORPOS")
	L.append("    {")
	L.append("    X = 5")
	L.append("    Y = 93")
	L.append("    }")
	L.append("  TEXTSHAPE")
	L.append("    {")
	L.append("    HEIGHT = "+str(size)+"")
	L.append("    }")
	L.append("  TEXT = '"+var_name+"'")
	return L

def set_text_box(L,var_name,position,size):
	L.append("$!ATTACHTEXT ")
	L.append("  ANCHORPOS")
	L.append("    {")
	L.append("    X = "+str(position[0]))
	L.append("    Y = "+str(position[1]))
	L.append("    }")
	L.append("  TEXTSHAPE")
	L.append("    {")
	L.append("    HEIGHT = "+str(size)+"")
	L.append("    }")
	L.append("  ANCHOR = HEADLEFT")
	L.append("  CLIPPING = CLIPTOFRAME")
	L.append("  TEXT = '"+var_name+"'")
	return L

def delete_text_box(L):
	# Deletes all text boxes in a relatively large range:
	L.append("$!PICK ADDALLINRECT")
	L.append("  SELECTTEXT = YES")
	L.append("  X1 = 0.667351129363")
	L.append("  X2 = 9.92607802875")
	L.append("  Y1 = 0.0179671457906")
	L.append("  Y2 = 7.15143737166")
	L.append("$!PICK CLEAR")
	return L

def extract_plane(L,plane):
	a = DF.adj_XYZ_given_dir(plane)
	d = DF.XYZ_given_dir(plane)
	L.append('$!GLOBALTHREED SLICE{NORMAL{'+a[0]+' = 0}}')
	L.append('$!GLOBALTHREED SLICE{NORMAL{'+a[1]+' = 0}}')
	L.append('$!GLOBALTHREED SLICE{NORMAL{'+d+' = 1}}')
	L.append("$!CREATESLICEZONEFROMPLANE ")
	L.append("  SLICESOURCE = VOLUMEZONES")
	L.append("  FORCEEXTRACTIONTOSINGLEZONE = YES")
	L.append("  COPYCELLCENTEREDVALUES = NO")
	return L

def set_labels(L,labels):
	L.append("$!XYLINEAXIS YDETAIL 1 {TITLE{TITLEMODE = USETEXT}}")
	L.append("$!XYLINEAXIS YDETAIL 1 {TITLE{TEXT = '"+labels[0]+"'}}")
	L.append("$!XYLINEAXIS XDETAIL 1 {TITLE{TITLEMODE = USETEXT}}")
	L.append("$!XYLINEAXIS XDETAIL 1 {TITLE{TEXT = '"+labels[1]+"'}}")
	return L

def set_y_label(L,y_label):
	L.append("$!XYLINEAXIS YDETAIL 1 {TITLE{TITLEMODE = USETEXT}}")
	L.append("$!XYLINEAXIS YDETAIL 1 {TITLE{TEXT = '"+y_label+"'}}")
	return L

def add_mesh_labels(L):
	label_font_size = 4
	temp = init_macro()
	temp = set_label_font_size(temp,label_font_size)
	temp = set_label_show(temp)
	X_n_rakes = 2; Xrange = [-.5,0]
	Y_n_rakes = 7; Yrange = [-.8,.99]
	Z_n_rakes = 3; Zrange = [-1,1]
	X = np.linspace(Xrange[0],Xrange[1],X_n_rakes)
	Y = np.linspace(Yrange[0],Yrange[1],Y_n_rakes)
	Z = np.linspace(Zrange[0],Zrange[1],Z_n_rakes)
	# X = [0]
	# Y = [0]
	# Z = [-1.049]
	Z = [0]
	for x in X:
		for y in Y:
			for z in Z:
				temp = append_label(temp,x,y,z)
	return L+temp

def add_mesh_labels_general(L,Xrange,Yrange,Zrange,n_rakes,
	X_override,Y_override,Z_override,label_font_size):
	temp = init_macro()
	temp = set_label_font_size(temp,label_font_size)
	temp = set_label_show(temp)
	X = np.linspace(Xrange[0],Xrange[1],n_rakes[0])
	Y = np.linspace(Yrange[0],Yrange[1],n_rakes[1])
	Z = np.linspace(Zrange[0],Zrange[1],n_rakes[2])
	if X_override: X = X_override
	if Y_override: Y = Y_override
	if Z_override: Z = Z_override
	for x in X:
		for y in Y:
			for z in Z:
				temp = append_label(temp,x,y,z)
	return L+temp

def append_label(L,x,y,z):
	L.append('$!CONTOURLABELS ADD')
	L.append('  CONTOURGROUP = 1')
	L.append('    X = '+str(x))
	L.append('    Y = '+str(y))
	L.append('    Z = '+str(z))
	L.append('  ISALIGNED = NO')
	L.append('$!GLOBALCONTOUR 1  LABELS{GENERATEAUTOLABELS = NO}')
	return L

def set_label_size(L,size):
	L.append('$!TWODAXIS XDETAIL{TICKLABEL{TEXTSHAPE{HEIGHT = '+str(size)+'}}}')
	L.append('$!TWODAXIS YDETAIL{TICKLABEL{TEXTSHAPE{HEIGHT = '+str(size)+'}}}')
	L.append('$!TWODAXIS XDETAIL{TITLE{TEXTSHAPE{HEIGHT = '+str(size)+'}}}')
	L.append('$!TWODAXIS YDETAIL{TITLE{TEXTSHAPE{HEIGHT = '+str(size)+'}}}')
	return L

def set_legend_text_size(L,size):
	L.append('$!GLOBALLINEPLOT LEGEND{TEXTSHAPE{HEIGHT = '+str(size-1)+'}}')
	return L

def set_text_size(L,size):
	L.append('$!GLOBALLINEPLOT LEGEND{TEXTSHAPE{HEIGHT = '+str(size-1)+'}}')
	L.append('$!TWODAXIS YDETAIL{TICKLABEL{TEXTSHAPE{HEIGHT = '+str(size)+'}}}')
	L.append('$!TWODAXIS XDETAIL{TICKLABEL{TEXTSHAPE{HEIGHT = '+str(size)+'}}}')
	L.append('$!XYLINEAXIS XDETAIL 1 {TICKLABEL{TEXTSHAPE{HEIGHT = '+str(size)+'}}}')
	L.append('$!XYLINEAXIS YDETAIL 1 {TICKLABEL{TEXTSHAPE{HEIGHT = '+str(size)+'}}}')
	L.append('$!GLOBALCONTOUR 1  LABELS{TEXTSHAPE{HEIGHT = '+str(size)+'}}')
	L.append('$!XYLINEAXIS XDETAIL 1 {TITLE{TEXTSHAPE{HEIGHT = '+str(size+2)+'}}}')
	L.append('$!XYLINEAXIS YDETAIL 1 {TITLE{TEXTSHAPE{HEIGHT = '+str(size+2)+'}}}')
	return L

def set_title_size(L,size):
	L.append('$!TWODAXIS YDETAIL{TICKLABEL{TEXTSHAPE{HEIGHT = '+str(size)+'}}}')
	L.append('$!TWODAXIS XDETAIL{TICKLABEL{TEXTSHAPE{HEIGHT = '+str(size)+'}}}')
	L.append('$!XYLINEAXIS YDETAIL 1 {TICKLABEL{TEXTSHAPE{HEIGHT = '+str(size)+'}}}')
	L.append('$!XYLINEAXIS XDETAIL 1 {TICKLABEL{TEXTSHAPE{HEIGHT = '+str(size)+'}}}')
	return L

def set_label_font_size(L,label_font_size):
	L.append('$!GLOBALCONTOUR 1  LABELS{TEXTSHAPE{HEIGHT = '+str(label_font_size)+'}}')
	return L

def set_label_offset(L,offset):
	L.append('$!TWODAXIS XDETAIL{TICKLABEL{OFFSET = '+str(offset[0])+'}}')
	L.append('$!TWODAXIS YDETAIL{TICKLABEL{OFFSET = '+str(offset[1])+'}}')
	L.append('$!XYLINEAXIS XDETAIL 1 {TITLE{PERCENTALONGLINE = '+str(offset[0])+'}}')
	L.append('$!XYLINEAXIS YDETAIL 1 {TITLE{PERCENTALONGLINE = '+str(offset[1])+'}}')
	return L

def set_x_label_align(L,align_percentage):
	L.append('$!TWODAXIS YDETAIL{TITLE{PERCENTALONGLINE = '+str(align_percentage)+'}}')
	return L

def set_axis_range(L,x_range,y_range):
	L.append('$!TWODAXIS XDETAIL{RANGEMIN = '+str(x_range[0])+'}')
	L.append('$!TWODAXIS XDETAIL{RANGEMAX = '+str(x_range[1])+'}')
	L.append('$!TWODAXIS YDETAIL{RANGEMIN = '+str(y_range[0])+'}')
	L.append('$!TWODAXIS YDETAIL{RANGEMAX = '+str(y_range[1])+'}')
	return L

def set_x_axis_range(L,x_range):
	L.append('$!XYLINEAXIS XDETAIL 1 {RANGEMIN = '+str(x_range[0])+'}')
	L.append('$!XYLINEAXIS XDETAIL 1 {RANGEMAX = '+str(x_range[1])+'}')
	return L

def set_y_axis_range(L,y_range):
	L.append('$!XYLINEAXIS YDETAIL 1 {RANGEMIN = '+str(y_range[0])+'}')
	L.append('$!XYLINEAXIS YDETAIL 1 {RANGEMAX = '+str(y_range[1])+'}')
	return L

def set_colormap(L): return CM.set_colormap(L)
