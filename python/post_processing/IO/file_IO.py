import numpy as np
import os
from os import listdir
from os.path import isfile, join
from shutil import copyfile
import sys

# os.path.dirname(os.path.realpath(__file__))
# os.getcwd()

def gen_files(dir, pattern):
	for dirname, subdirs, files in os.walk(dir):
		for f in files:
			if f.endswith(pattern):
				yield os.path.join(dirname, f)

def delete_pyc_files():
	for f in gen_files('.', '.pyc'):
		os.remove(f)

def delete_pyc_files_1_dir():
	filelist = [ f for f in os.listdir(".") if f.endswith(".pyc") ]
	for f in filelist:
	    os.remove(f)

def make_path(new_path):
	if not os.path.exists(new_path):
		os.makedirs(new_path)

def get_all_files_in_path(file_path):
	return [k for k in listdir(file_path) if isfile(join(file_path, k))]

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

def get_data(f):
	arr = np.loadtxt(f,skiprows=3) # Get data
	H = get_header(f) # Get header
	return (arr,H)

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

def keep_direction_only_in_header(header,direction):
	h = header
	if direction==1: h[1] = h[1].replace('"y",','').replace('"z",','')
	if direction==2: h[1] = h[1].replace('"x",','').replace('"z",','')
	if direction==3: h[1] = h[1].replace('"x",','').replace('"y",','')
	s = ''.join(header[2]).split(',')
	if direction==1: s = [x+',' for x in s if 'J = ' not in x and 'K = ' not in x]
	if direction==2: s = [x+',' for x in s if 'I = ' not in x and 'K = ' not in x]
	if direction==3: s = [x+',' for x in s if 'I = ' not in x and 'J = ' not in x]
	s[-1] = s[-1][:-1]
	if direction==1 or direction==2: s.append(' DATAPACKING = POINT\n')
	s = [x if not x.endswith('\n') else x[:-1] for x in s]
	h[2] = s
	h = [item for sublist in h for item in sublist]
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
	print coordinates
	print s2
	s = s2.split(',')
	suffix2 = '2'
	s = ['"'+k[1:-1]+'_'+suffix2+'"' for k in s]
	h[1] = s1+','.join(s[-3:])+'\n'
	return h
