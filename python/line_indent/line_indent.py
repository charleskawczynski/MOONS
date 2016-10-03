import file_IO as IO

n_leading_spaces = '     '
n_leading_indent = '  '

def elevation_in(s,up,down):
	if down in s: e = -1
	elif up in s: e = 1
	else: e = 0
	return e

def elevation_in_condition(s,up,down,t): # except when t is in s
	if down in s and t not in s: e = -1
	elif up in s and t not in s: e = 1
	else: e = 0
	return e

def elevation_condition(c,up,down,t):
	e = []; temp = 0
	for L in c:
		temp = temp + elevation_in_condition(L,up,down,t)
		e.append(temp)
	e.insert(0,0)
	fix = [t - s for s, t in zip(e, e[1:])]
	# fix = [-abs(x) if x>0 else 0 for x in fix]
	fix = [-abs(x) for x in fix]
	L = []
	for x,y in zip(e,fix): L.append(x+y)
	return L

def elevation(c,up,down):
	e = []; temp = 0
	for L in c:
		temp = temp + elevation_in(L,up,down)
		e.append(temp)
	e.insert(0,0)
	fix = [t - s for s, t in zip(e, e[1:])]
	# fix = [-abs(x) if x>0 else 0 for x in fix]
	fix = [-abs(x) for x in fix]
	L = []
	for x,y in zip(e,fix): L.append(x+y)
	return L

def get_e_program(c): return elevation(c,'program','end program')
def get_e_subroutine(c): return elevation(c,'subroutine','end subroutine')
def get_e_function(c): return elevation(c,'function','end function')
def get_e_module(c):  return elevation_condition(c,'module', 'end module','interface')
def get_e_type(c):
	temp = elevation_condition(c,'type','end type','(')
	e = []; past_contains = False
	for s,e_i in zip(c,temp):
		if not past_contains:
			e.append(e_i); past_contains = 'contains' in s
		else: e.append(0)
	return e

def align_interfaces(c):
	cs = [x.strip() for x in c]
	IL = [x.startswith('interface') and x.endswith('end interface') and 'module procedure' in x for x in cs]
	interface_lines = [x for x,y in zip(c,IL) if y]
	if interface_lines != []:
		int_to_mp = 3
		mp_to_end = 3
		L1 = [x.split(';')[0] for x,y in zip(c,IL) if y]
		L2 = [x.split(';')[1] for x,y in zip(c,IL) if y]
		L3 = [x.split(';')[2] for x,y in zip(c,IL) if y]
		print L1
		print L2
		print L3
		max_name_length = max([len(x.replace(' ','').replace('interface','')) for x in L1])
		max_alias_length = max([len(x.replace(' ','').replace('moduleprocedure','')) for x in L2])
		print max_name_length
		print max_alias_length
		max_seg1 = len(n_leading_spaces+'interface '+max_alias_length)
		temp = []
		for s,L in zip(c,IL):
			if L:
				S1 = [s.split(';')[0]
				S2 = [s.split(';')[1]
				S3 = [s.split(';')[2]
				temp.append(S1+S2+S3)
			else:
				temp.append(s)

	# print interface_lines
	# for i in interface_lines:
	# 	L = i.split(';')
	# 	print L
	# print '\n'.join(interface_lines)
	# print L

	# L_max = max([x for x,y in zip(c,IL) if y])
	# print L

	# for x in c:
	# 	if 'contains' in x: break
	# 	xs = x.strip()
	# 	interface_lines = xs.startswith('interface') and xs.endswith('end interface') and 'module procedure' in x
	# 	if interface_lines:
	# 		L = x.split(';')
	# 		x = x+'test'
	# 		print L
	return c

def first_outer(s,up,down):
	if s.count(up) != s.count(down):raise ValueError('Unequal # of '+up+' and '+ down)
	i_start = s.find(up)
	i_stop = s.rfind(down)
	k = 1
	s_result = s[i_start+1:i_stop]
	for i in range(i_start,i_stop):
		s_seg = s[i_start:i_start+k]
		if elevation(s_seg,up,down) == 0:
			s_result = s_seg; i_stop = i; break
		k=k+1
	return (s_result,i_start,i_stop)

def most_outer(s,up,down):
	if s.count(up) != s.count(down):raise ValueError('Unequal # of '+up+' and '+ down)
	i_start = s.find(up)
	i_stop = s.rfind(down)
	return (s[i_start+1:i_stop],i_start,i_stop)

def set_indent_flat(c):
	return [n_leading_spaces+x.lstrip() if not x.startswith('#') else x.lstrip() for x in c]

def indent_type_def(c):
	print elevation()
	c = ['     '+x.lstrip() for x in c]
	return c

def indent_program_unit(c,e):
	L =[]
	for x,i in zip(c,e):
		if i>0: L.append(n_leading_indent+x)
		else: L.append(x)
	return L

def modify_lines(c):
	c = set_indent_flat(c)
	c = indent_program_unit(c,get_e_subroutine(c))
	c = indent_program_unit(c,get_e_function(c))
	c = indent_program_unit(c,get_e_type(c))
	c = align_interfaces(c)
	# c = indent_program_unit(c,get_e_module(c))
	return c

def get_contents_list(f):
	c = IO.get_file_contents(f)
	return c.split('\n')

def export_new_file(f,c):
	c = '\n'.join(c)
	IO.set_file_contents(f.replace('.f90','')+'_new'+'.f90',c)
	# IO.set_file_contents(f,c) # for production

def process(f):
	print f
	c = get_contents_list(f)
	c = modify_lines(c)
	export_new_file(f,c)

