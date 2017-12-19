def init_macro():
	L = []
	L.append('#!MC 1120')
	return L

def append_rake(L,x1,x2,y1,y2,z1,z2):
	L.append('$!STREAMTRACE ADD')
	L.append('  STREAMTYPE = VOLUMELINE')
	L.append('  DIRECTION = BOTH')
	L.append('  STARTPOS')
	L.append('    {')
	L.append('    X = '+str(x1))
	L.append('    Y = '+str(y1))
	L.append('    Z = '+str(z1))
	L.append('    }')
	L.append('  ALTSTARTPOS')
	L.append('    {')
	L.append('    X = '+str(x2))
	L.append('    Y = '+str(y2))
	L.append('    Z = '+str(z2))
	L.append('    }')
	return L

def append_point_rake(L,x,y,z):
	L.append('$!STREAMTRACE ADD')
	L.append('  STREAMTYPE = VOLUMELINE')
	L.append('  DIRECTION = BOTH')
	L.append('  STARTPOS')
	L.append('    {')
	L.append('    X = '+str(x))
	L.append('    Y = '+str(y))
	L.append('    Z = '+str(z))
	L.append('    }')
	return L