import numpy as np
import os
from os import listdir
from os.path import isfile, join
from shutil import copyfile
import sys
import shutil

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

def set_file_contents(file_path,contents):
	text_file = open(file_path, "w")
	text_file.write(contents)
	text_file.close()

def write_list_to_file(file_name,L):
	f = open(file_name,'w+',encoding='utf8')
	f.write('\n'.join(L))
	f.close()

def get_file_contents(file_path):
	with open(file_path, 'r') as content_file: contents = content_file.read()
	return contents

def read_file_to_list(file_name):
	with open(file_name,encoding='utf8') as f: # Accounts for encoded characters
		L = f.read().splitlines() # list of lines
	return L

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

def file_len_old(fname):
	with open(fname) as f:
		for i, l in enumerate(f):
			pass
	return i + 1

def file_len(file_name):
	with open(file_name,encoding='utf8') as f: # Accounts for encoded characters
		text = f.read().splitlines() # list of lines
	lines = len(text) # length of the list = number of lines
	return lines

def highest_matching_directory(f1,f2,PS):
	p1 = f1.split(PS); p1 = filter(None,p1)
	p2 = f2.split(PS); p2 = filter(None,p2)
	l = []
	for k1,k2 in zip(p1,p2):
		if k1==k2: l.append(k1+PS)
		else: mismatch=k1; break
	return (''.join(l),mismatch)

def copy_file(src,dst):
	copyfile(src,dst)

def make_directory_tree_target(root,LDC,PS):
	for L in LDC:
		directory_tree(root,L+'e_budget'+PS,PS)
		directory_tree(root,L+'Ufield'+PS,PS)
		make_new_directory_tree_in_field(root,L+'Ufield'+PS,PS)
		directory_tree(root,L+'Bfield'+PS,PS)
		make_new_directory_tree_in_field(root,L+'Bfield'+PS,PS)
		directory_tree(root,L+'Jfield'+PS,PS)
		make_new_directory_tree_in_field(root,L+'Jfield'+PS,PS)
		directory_tree(root,L+'Tfield'+PS,PS)
		make_new_directory_tree_in_field(root,L+'Tfield'+PS,PS)
		print(L.replace(root,''))

def delete_folders_in_dir_tree(root,LDC,PS):
	for L in LDC:
		remove_path_if_exists(L+'Ufield'+PS+'transient'+PS)
		remove_path_if_exists(L+'Bfield'+PS+'transient'+PS)
		remove_path_if_exists(L+'Jfield'+PS+'transient'+PS)
		remove_path_if_exists(L+'Tfield'+PS+'transient'+PS)
		remove_path_if_exists(L+'Ufield'+PS+'post_processing'+PS)
		remove_path_if_exists(L+'Bfield'+PS+'post_processing'+PS)
		remove_path_if_exists(L+'Jfield'+PS+'post_processing'+PS)
		remove_path_if_exists(L+'Tfield'+PS+'post_processing'+PS)

def delete_files_in_dir_tree(root,LDC,PS):
	for L in LDC:
		remove_file_if_exists(L+'Ufield'+PS+'norm_GS_p.dat')
		remove_file_if_exists(L+'Ufield'+PS+'energy_budget.dat')
		remove_file_if_exists(L+'Ufield'+PS+'KU_compact.dat')
		remove_file_if_exists(L+'Ufield'+PS+'norm_PCG_VF_U.dat')
		remove_file_if_exists(L+'Bfield'+PS+'energy_budget.dat')

def move_file(old_path,new_path):
	if os.path.exists(old_path): shutil.move(old_path,new_path)

def organize_Ufield(root,LDC,PS):
	for L in LDC:
		field_dir = L+'Ufield'+PS
		file = 'divUc.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'jCrossBf_x.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'jCrossBf_y.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'jCrossBf_z.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'pc.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'pnp.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Uf_x.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Uf_y.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Uf_z.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Unp.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)

		file = 'KU.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)

		file = 'KE_vs_t.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)

		file = 'transient_divU.dat'; move_file(field_dir+file,field_dir+'res'+PS+file)
		file = 'transient_divU_info.dat'; move_file(field_dir+file,field_dir+'res'+PS+file)
		file = 'norm_PCG_SF_p.dat'; move_file(field_dir+file,field_dir+'res'+PS+file)
		file = 'norm_PCG_SF_p.dat'; move_file(field_dir+file,field_dir+'res'+PS+file)

def organize_Bfield(root,LDC,PS):
	for L in LDC:
		field_dir = L+'Bfield'+PS
		file = 'B0np.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Bf_x.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Bf_y.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Bf_z.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Bnp.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'divBc.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Ue_x.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Ve_y.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'We_z.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)

		file = 'Uexe_x.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Uexe_y.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Uexe_z.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)

		file = 'Veye_x.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Veye_y.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Veye_z.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)

		file = 'Weze_x.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Weze_y.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Weze_z.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)

		file = 'KB.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)
		file = 'KB_c.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)
		file = 'KB_f.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)
		file = 'KB0.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)
		file = 'KB0_c.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)
		file = 'KB0_f.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)
		file = 'KBi.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)
		file = 'KBi_c.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)
		file = 'KBi_f.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)

		file = 'MEi_c_vs_t.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)
		file = 'MEi_f_vs_t.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)
		file = 'MEi_t_vs_t.dat'; move_file(field_dir+file,field_dir+'energy'+PS+file)
		file = 'MEi_vs_t.dat';   move_file(field_dir+file,field_dir+'energy'+PS+file)

		file = 'transient_divB.dat'; move_file(field_dir+file,field_dir+'res'+PS+file)
		file = 'transient_divB_info.dat'; move_file(field_dir+file,field_dir+'res'+PS+file)

		f = get_all_files_in_path(field_dir)
		f = [x for x in f if x.startswith('norm')] # to handle non-ascii character, blah.
		for file in f:
			move_file(field_dir+file,field_dir+'res'+PS+file)

def organize_Jfield(root,LDC,PS):
	for L in LDC:
		field_dir = L+'Jfield'+PS
		file = 'divJn.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Je_x.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Je_y.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Je_z.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)
		file = 'Jnp.dat'; move_file(field_dir+file,field_dir+'field'+PS+file)

		file = 'transient_divJ.dat'; move_file(field_dir+file,field_dir+'res'+PS+file)
		file = 'transient_divJ_info.dat'; move_file(field_dir+file,field_dir+'res'+PS+file)

def make_new_directory_tree_in_field(root,field_dir,PS):
	directory_tree(root,field_dir+'field' +PS,PS)
	directory_tree(root,field_dir+'res'   +PS,PS)
	directory_tree(root,field_dir+'energy'+PS,PS)

