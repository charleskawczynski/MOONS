def xyz_along(dir):
	if (dir==1): return 'x'
	if (dir==2): return 'y'
	if (dir==3): return 'z'

def xyz_along_ind(dir):
	if (dir==1): return [1-1]
	if (dir==2): return [2-1]
	if (dir==3): return [3-1]

def xyz_adj(dir):
	if (dir==1): return ['y','z']
	if (dir==2): return ['x','z']
	if (dir==3): return ['x','y']

def xyz_adj_ind(dir):
	if (dir==1): return [2-1,3-1]
	if (dir==2): return [1-1,3-1]
	if (dir==3): return [1-1,2-1]

def uvw_along(dir):
	if (dir==1): return 'u'
	if (dir==2): return 'v'
	if (dir==3): return 'w'

def Bxyz_along(dir):
	if (dir==1): return 'Bx'
	if (dir==2): return 'By'
	if (dir==3): return 'Bz'

def Jxyz_along(dir):
	if (dir==1): return 'Jx'
	if (dir==2): return 'Jy'
	if (dir==3): return 'Jz'
