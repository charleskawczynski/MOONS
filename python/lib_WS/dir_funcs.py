def adj_given_dir(dir):
	if (dir==1): a = [2,3]
	elif (dir==2): a = [1,3]
	elif (dir==3): a = [1,2]
	else: raise NameError('bad plane in adj_given_dir')
	return a

def xyz_given_dir(dir):
	if (dir==1): s = 'x'
	elif (dir==2): s = 'y'
	elif (dir==3): s = 'z'
	else: raise NameError('bad plane in xyz_given_dir')
	return s

def adj_xyz_given_dir(dir):
	if (dir==1): a = ['y','z']
	elif (dir==2): a = ['x','z']
	elif (dir==3): a = ['x','y']
	else: raise NameError('bad plane in adj_xyz_given_dir')
	return a

def adj_xyz_given_dir_ind(dir):
	if (dir==1): a = [1,2]
	elif (dir==2): a = [0,2]
	elif (dir==3): a = [0,1]
	else: raise NameError('bad plane in adj_xyz_given_dir_ind')
	return a

def uvw_given_dir(dir):
	if (dir==1): s = 'u'
	elif (dir==2): s = 'v'
	elif (dir==3): s = 'w'
	else: raise NameError('bad plane in uvw_given_dir')
	return s

def Bxyz_given_dir(dir):
	if (dir==1): s = 'Bx'
	elif (dir==2): s = 'By'
	elif (dir==3): s = 'Bz'
	else: raise NameError('bad plane in Bxyz_given_dir')
	return s

def Jxyz_given_dir(dir):
	if (dir==1): s = 'Jx'
	elif (dir==2): s = 'Jy'
	elif (dir==3): s = 'Jz'
	else: raise NameError('bad plane in Jxyz_given_dir')
	return s

def adj_XYZ_given_dir(dir):
	if dir==1: a = ['Y','Z']
	elif dir==2: a = ['X','Z']
	elif dir==3: a = ['X','Y']
	else: raise NameError('bad plane in adj_XYZ_given_dir')
	return a

def XYZ_given_dir(dir):
	if dir==1: d = 'X'
	elif dir==2: d = 'Y'
	elif dir==3: d = 'Z'
	else: raise NameError('bad plane in XYZ_given_dir')
	return d
