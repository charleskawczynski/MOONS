def init_macro():
	L = []
	L.append('#!MC 1120')
	return L

def set_label_font_size(L,label_font_size):
	L.append('$!GLOBALCONTOUR 1  LABELS{TEXTSHAPE{HEIGHT = '+str(label_font_size)+'}}')
	return L

def append_label(L,x,y,z):
	L.append('$!CONTOURLABELS ADD')
	L.append('  CONTOURGROUP = 1')
	L.append('    X = '+str(x))
	L.append('    Y = '+str(y))
	L.append('    Z = '+str(z))
	L.append('  ISALIGNED = NO')
	L.append('$!GLOBALCONTOUR 1  LABELS{GENERATEAUTOLABELS = NO}')
	return L
