import os
if os.name == 'posix':
    pass # this is the operating system name
elif os.name == 'nt':
    clear = lambda: os.system('cls')
    clear()
from os import listdir
from os.path import isfile, join
import sys
import shutil
import zipfile
import glob

def get_all_files_in_path(file_path):
    return [k for k in listdir(file_path) if isfile(join(file_path, k))]

def read_file_to_list(file_name):
    with open(file_name,encoding='utf8') as f: # Accounts for encoded characters
        L = f.read().splitlines() # list of lines
    return L

def write_list_to_file(file_name,L):
    f = open(file_name,'w+')
    f.write('\n'.join(L))
    f.close()

def get_component(c_str):
	if c_str=='u': c = 1
	elif c_str=='v': c = 2
	elif c_str=='w': c = 3
	elif c_str=='Bx': c = 1
	elif c_str=='By': c = 2
	elif c_str=='Bz': c = 3
	elif c_str=='Jx': c = 1
	elif c_str=='Jy': c = 2
	elif c_str=='Jz': c = 3
	else:
		print('c_str = '+c_str)
		raise NameError('Bad component in get_component')
	return c

f = []
PS = '/'
for dirpath, dirnames, filenames in os.walk("."):
    for filename in [f for f in filenames if f.endswith(".dat")]:
        f = f+ [dirpath+PS+filename]

f_all = f
f_all = [x.replace('\\',PS) for x in f_all]

for f in f_all:
	L = read_file_to_list(f)
	Rem = f.split(PS)[-1].replace('.dat','').split('Rem')[-1].lstrip('0')
	if Rem=='': Rem = '0'
	component_str = f.split(PS)[1].split(',')[0]
	print(component_str)
	c = get_component(component_str)
	cols = [0, c]
	prefix = component_str+', '
	leg = 'Re<sub>m</sub> = '+Rem
	L[1] = L[1].replace('"Upn_x","Upn_y","Upn_z"','"'+leg+'"')
	L[1] = L[1].replace('"Jpn_x","Jpn_y","Jpn_z"','"'+leg+'"')
	L[1] = L[1].replace('"Bpn_x","Bpn_y","Bpn_z"','"'+leg+'"')
	L[0] = L[0].replace('TITLE = "3D Vector Field"','TITLE = "3D Vector Field, '+prefix+'"')
	print()
	temp = [x.split('	') if '	' in x else x for x in L]
	L = ['	'.join([y for i,y in enumerate(x) if any([i==z for z in cols])]) if isinstance(x, list) else x for x in temp]
	f_new = f.split(PS)
	f_new[-1] = 'new_'+f_new[-1]
	f_new = PS.join(f_new)
	f_new = f_new.replace('new_new','new')
	print(f_new)
	write_list_to_file(f_new,L)

