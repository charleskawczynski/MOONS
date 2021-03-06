import ntpath
import numpy as np
import os
from os import listdir
from os.path import isfile, join
from shutil import copyfile
import sys
from collections import Iterable

# os.path.dirname(os.path.realpath(__file__))
# os.getcwd()
# full_path = path+file_name+ext

def flatten(foo):
	for x in foo:
		if hasattr(x, '__iter__') and not isinstance(x, str):
			for y in flatten(x):
				yield y
		else:
			yield x

def flatten_list_with_str(foo):
	return list(flatten(foo))

def flatten_list(l):
	return [item for sublist in l for item in sublist]

def flatten_list_with_str_old(l):
	return lambda l: [item for sublist in l for item in sublist]

def rename_file(f_src,f_dst):
	os.rename(f_src,f_dst)

def does_file_exist(f):
	return os.path.exists(f)

def get_file_extension(f):
	filename, file_extension = os.path.splitext(f)
	return file_extension

def get_full_filename_without_extension(f):
	return os.path.splitext(f)[0]

def get_file_from_path(p):
	return ntpath.basename(p)

def gen_files(dir, pattern):
	for dirname, subdirs, files in os.walk(dir):
		for f in files:
			if f.endswith(pattern):
				yield os.path.join(dirname, f)

def last_line(filename):
	fh = open(filename, 'rb')
	fh.seek(-1024, 2)
	last = fh.readlines()[-1].decode()
	return [float(i) for i in last.split()]

def delete_pyc_files():
	for f in gen_files('.', '.pyc'):
		os.remove(f)

def remove_file_if_exists(p):
	if os.path.exists(p): os.remove(p)

def remove_path_if_exists(p):
	if os.path.exists(p): shutil.rmtree(p)

def delete_pyc_files_1_dir():
	filelist = [ f for f in os.listdir(".") if f.endswith(".pyc") ]
	for f in filelist:
		os.remove(f)

def make_path(new_path):
	if not os.path.exists(new_path):
		os.makedirs(new_path)

def get_all_files_in_path(file_path):
	return [k for k in listdir(file_path) if isfile(join(file_path, k))]

def get_file_contents(file_name):
	with open(file_name, 'r') as content_file:
		content = content_file.read()
	return content

def get_e_budget(file_name):
	L = []
	with open(file_name, 'r') as content_file:
		content = content_file.read()
		temp = content.split('\n')
		temp = [x.split('=') for x in temp]
		temp = [tuple([k.replace(' ','') for k in x]) for x in temp]
		temp = [x for x in temp if x[0]]
		L=L+temp
	return L[1:]

def set_file_contents(file_path,contents):
	text_file = open(file_path, "w")
	text_file.write(contents)
	text_file.close()

def directory_tree(root,new_path,PS):
	path = root+new_path
	path = path.replace(root,'')
	p = path.split(PS)
	p = filter(None, p) # Remove empty strings
	l = []
	for k in p:
		l.append(k+PS)
		new_path = root+''.join(l)
		make_path(new_path)

def highest_matching_directory(f1,f2,PS):
	p1 = f1.split(PS); p1 = filter(None,p1)
	p2 = f2.split(PS); p2 = filter(None,p2)
	l = []
	for k1,k2 in zip(p1,p2):
		if k1==k2: l.append(k1+PS)
		else: mismatch=k1; break
	return (''.join(l),mismatch)

def get_header(file_name):
	fp = open(file_name)
	H = []
	for i, line in enumerate(fp):
		if i < 3: H.append(line)
		else: break
	fp.close()
	return H

def copy_file(src,dst):
	copyfile(src,dst)

def copy_data_file(file_old,file_new):
	arr = np.loadtxt(file_old,skiprows=3) # Get data
	H = get_header(file_old) # Get header
	head = ''.join(H)
	delim = '	  '
	np.savetxt(file_new, arr, delimiter=delim, header = head, comments='') # Save file

def save_data_to_file(file_name,data,header):
	head = ''.join(header)
	delim = '	  '
	np.savetxt(file_name, data, delimiter=delim, header = head, comments='') # Save file

def get_tec_data(f):
	arr = np.loadtxt(f,skiprows=3) # Get data
	H = get_header(f) # Get header
	return (arr,H)

def get_data(f,n):
	arr = np.loadtxt(f,skiprows=n) # Get data
	H = get_header(f) # Get header
	return (arr,H)

# def get_data(f,N_skip_header):
# 	arr = np.loadtxt(f,skiprows=N_skip_header) # Get data
# 	H = get_header(f) # Get header
# 	return (arr,H)

def get_data_all(f):
	return np.loadtxt(f)

def get_vec_data(d): return ([k[0] for k in d],[k[1] for k in d])

def get_vec_data_np(d): return (np.array([k[0] for k in d]),np.array([k[1] for k in d]))

def make_directory_tree_target(root,source,target,PS):
	for s,t in zip(source,target):
		directory_tree(root,t+'Ufield'+PS,PS)
		directory_tree(root,t+'Bfield'+PS,PS)
		directory_tree(root,t+'Jfield'+PS,PS)
		directory_tree(root,t+'Tfield'+PS,PS)
		directory_tree(root,t+'material'+PS,PS)
		directory_tree(root,t+'parameters'+PS,PS)

def get_shape(header):
	h = ' '.join(header).replace(' = ','=').replace('= ','=').replace(' =','=')
	Isplit = h.split('I=')
	Jsplit = h.split('J=')
	Ksplit = h.split('K=')
	I_exists = len(Isplit)>1
	J_exists = len(Jsplit)>1
	K_exists = len(Ksplit)>1
	if (I_exists): Isplit = Isplit[1].split(' ')[0].replace(',','')
	if (J_exists): Jsplit = Jsplit[1].split(' ')[0].replace(',','')
	if (K_exists): Ksplit = Ksplit[1].split(' ')[0].replace(',','')
	if (I_exists): Isplit = int(Isplit)
	if (J_exists): Jsplit = int(Jsplit)
	if (K_exists): Ksplit = int(Ksplit)
	if (not I_exists): Isplit = False
	if (not J_exists): Jsplit = False
	if (not K_exists): Ksplit = False
	return (Isplit,Jsplit,Ksplit)

def keep_direction_only_in_header(header,direction):
	h = header
	if direction==1: h[1] = h[1].replace('"y",','').replace('"z",','')
	if direction==2: h[1] = h[1].replace('"x",','').replace('"z",','')
	if direction==3: h[1] = h[1].replace('"x",','').replace('"y",','')
	s = h[1].split('"')
	if direction==1: s = [x.replace('np','')[0:3]+'(x) '+x.replace('np','')[4:] if 'np' in x else x for x in s]
	if direction==2: s = [x.replace('np','')[0:3]+'(y) '+x.replace('np','')[4:] if 'np' in x else x for x in s]
	if direction==3: s = [x.replace('np','')[0:3]+'(z) '+x.replace('np','')[4:] if 'np' in x else x for x in s]
	h[1] = '"'.join(s)

	s = ''.join(header[2]).split(',')
	if direction==1: s = [x+',' for x in s if 'J=' not in x.replace(' ','') and 'K=' not in x.replace(' ','')]
	if direction==2: s = [x+',' for x in s if 'I=' not in x.replace(' ','') and 'K=' not in x.replace(' ','')]
	if direction==3: s = [x+',' for x in s if 'I=' not in x.replace(' ','') and 'J=' not in x.replace(' ','')]
	s[-1] = s[-1][:-1]
	if direction==1 or direction==2: s.append(' DATAPACKING = POINT\n')
	s = [x if not x.endswith('\n') else x[:-1] for x in s]
	h[2] = s
	h = flatten_list_with_str(h)
	return h

def keep_component_only_in_header(header,direction):
	h = header
	# print('h')
	# print(h)
	s = h[1].split('"x"')
	if len(s)==2:
		v = h[1].split('"x"')[1]
		c = h[1].split('"x"')[0]+'"x"'
	s = h[1].split('"y"')
	if len(s)==2:
		v = h[1].split('"y"')[1]
		c = h[1].split('"y"')[0]+'"y"'
	s = h[1].split('"z"')
	if len(s)==2:
		v = h[1].split('"z"')[1]
		c = h[1].split('"z"')[0]+'"z"'
	L = v[1:].split(',')
	L = [x for i,x in enumerate(L) if i==direction-1]
	h[1] = c+','+','.join(L)
	h = flatten_list_with_str(h)
	return h

def neglect_direction_in_header(header,direction):
	h = header
	if direction==1: h[1] = h[1].replace('"x",','')
	if direction==2: h[1] = h[1].replace('"y",','')
	if direction==3: h[1] = h[1].replace('"z",','')
	s = h[1].split('"')
	if direction==1: s = [x.replace('np','')[0:3]+'(y,z)'+x.replace('np','')[4:] if 'np' in x else x for x in s]
	if direction==2: s = [x.replace('np','')[0:3]+'(x,y)'+x.replace('np','')[4:] if 'np' in x else x for x in s]
	if direction==3: s = [x.replace('np','')[0:3]+'(y,z)'+x.replace('np','')[4:] if 'np' in x else x for x in s]
	h[1] = '"'.join(s)
	s = ''.join(header[2]).split(',')
	if direction==1: s = [x+',' for x in s if 'I=' not in x.replace(' ','')]
	if direction==2: s = [x+',' for x in s if 'J=' not in x.replace(' ','')]
	if direction==3: s = [x+',' for x in s if 'K=' not in x.replace(' ','')]
	s[-1] = s[-1][:-1]
	if direction==3: s.append(' DATAPACKING = POINT\n')
	s = [x if not x.endswith('\n') else x[:-1] for x in s]
	h[2] = s
	h = flatten_list_with_str(h)
	return h

def keep_direction_only_in_header_new(header,direction):
	h = header
	if direction==1: h[1] = h[1].replace('"y",','').replace('"z",','')
	if direction==2: h[1] = h[1].replace('"x",','').replace('"z",','')
	if direction==3: h[1] = h[1].replace('"x",','').replace('"y",','')
	s = h[1].split('"')
	if direction==1: s = [x.replace('np','')+'(x)' if 'np' in x else x for x in h[1].split('"')]
	if direction==2: s = [x.replace('np','')+'(y)' if 'np' in x else x for x in h[1].split('"')]
	if direction==3: s = [x.replace('np','')+'(z)' if 'np' in x else x for x in h[1].split('"')]
	h[1] = ''.join(s)

	if direction==1: h[1] = h[1].replace('"x",','').replace('"z",','')
	if direction==2: h[1] = h[1].replace('"x",','').replace('"z",','')
	if direction==3: h[1] = h[1].replace('"x",','').replace('"y",','')
	s = ''.join(header[2]).split(',')
	if direction==1: s = [x+',' for x in s if 'J=' not in x.replace(' ','') and 'K=' not in x.replace(' ','')]
	if direction==2: s = [x+',' for x in s if 'I=' not in x.replace(' ','') and 'K=' not in x.replace(' ','')]
	if direction==3: s = [x+',' for x in s if 'I=' not in x.replace(' ','') and 'J=' not in x.replace(' ','')]
	s[-1] = s[-1][:-1]
	if direction==1 or direction==2: s.append(' DATAPACKING = POINT\n')
	s = [x if not x.endswith('\n') else x[:-1] for x in s]
	h[2] = s
	h = flatten_list_with_str(h)
	return h

def combine_variables_in_header(h1,h2,suffix1,suffix2):
	h = h1
	vars1 = h1[1].replace('VARIABLES = ','').replace('"x",','').replace('"y",','').replace('"z",','').replace('\n','')
	vars2 = h2[1].replace('VARIABLES = ','').replace('"x",','').replace('"y",','').replace('"z",','').replace('\n','')
	coordinates = h1[1].replace(vars1,'').replace('\n','')
	vars1 = vars1.split(','); vars_1 = ['"'+k[1:-1]+'_'+suffix1+'"' for k in vars1]
	vars2 = vars2.split(','); vars_2 = ['"'+k[1:-1]+'_'+suffix2+'"' for k in vars2]
	h[1] = coordinates+','.join(vars_1)+','+','.join(vars_2)+'\n'
	return h

def combine_variables_in_header_new(h1,h2):
	h = h1
	s1 = h[1].replace('\n',',')
	vars1 = h1[1].replace('VARIABLES = ','').replace('"x",','').replace('"y",','').replace('"z",','').replace('\n','')
	vars2 = h2[1].replace('VARIABLES = ','').replace('"x",','').replace('"y",','').replace('"z",','').replace('\n','')
	coordinates = h1[1].replace(vars1,'')
	s = s2.split(',')
	suffix2 = '2'
	s = ['"'+k[1:-1]+'_'+suffix2+'"' for k in s]
	h[1] = s1+','.join(s[-3:])+'\n'
	return h

def get_SS_coordinate(f):
	(arr,header) = get_data(f)
	(x,y) = get_vec_data_np(arr)
	return (x[-1],y[-1])
