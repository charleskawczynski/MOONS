import ntpath
import numpy as np
import os
from os import listdir
from os.path import isfile, join
from shutil import copyfile
import sys

def get_header_list(file_name):
	fp = open(file_name)
	H = []
	for i, line in enumerate(fp):
		if i < 3: H.append(line)
		else: break
	fp.close()
	return H

def create_header_0D(title,var_name_list,n_points):
	L = []
	L = L + ['TITLE = "'+title+'"']
	L = L + ['VARIABLES = "'+'","'.join(var_name_list)+'"']
	L = L + ['ZONE , T ="1", I = '+str(n_points)+' DATAPACKING = POINT']
	return L

def get_shape(header_list):
	h = ' '.join(header_list).replace(' = ','=').replace('= ','=').replace(' =','=')
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

def get_variable_list(header_list):
	h = ' '.join(header_list).replace(' = ','=').replace('= ','=').replace(' =','=')
	variable_list = header_list[1].replace('VARIABLES = ','')
	variable_list = variable_list.split(',')
	variable_list = [x.replace(' ','') for x in variable_list]
	variable_list = [x.replace(',','') for x in variable_list]
	variable_list = [x.replace('\n','') for x in variable_list]
	return variable_list

def keep_direction_only_in_header(header_list,direction):
	h = header_list
	if direction==1: h[1] = h[1].replace('"y",','').replace('"z",','')
	if direction==2: h[1] = h[1].replace('"x",','').replace('"z",','')
	if direction==3: h[1] = h[1].replace('"x",','').replace('"y",','')
	s = h[1].split('"')
	if direction==1: s = [x.replace('np','')[0:3]+'(x) '+x.replace('np','')[4:] if 'np' in x else x for x in s]
	if direction==2: s = [x.replace('np','')[0:3]+'(y) '+x.replace('np','')[4:] if 'np' in x else x for x in s]
	if direction==3: s = [x.replace('np','')[0:3]+'(z) '+x.replace('np','')[4:] if 'np' in x else x for x in s]
	h[1] = '"'.join(s)

	s = ''.join(header_list[2]).split(',')
	if direction==1: s = [x+',' for x in s if 'J=' not in x.replace(' ','') and 'K=' not in x.replace(' ','')]
	if direction==2: s = [x+',' for x in s if 'I=' not in x.replace(' ','') and 'K=' not in x.replace(' ','')]
	if direction==3: s = [x+',' for x in s if 'I=' not in x.replace(' ','') and 'J=' not in x.replace(' ','')]
	s[-1] = s[-1][:-1]
	if direction==1 or direction==2: s.append(' DATAPACKING = POINT\n')
	s = [x if not x.endswith('\n') else x[:-1] for x in s]
	h[2] = s
	h = [item for sublist in h for item in sublist]
	return h

def neglect_direction_in_header(header_list,direction):
	h = header_list
	if direction==1: h[1] = h[1].replace('"x",','')
	if direction==2: h[1] = h[1].replace('"y",','')
	if direction==3: h[1] = h[1].replace('"z",','')
	s = h[1].split('"')
	if direction==1: s = [x.replace('np','')[0:3]+'(y,z)'+x.replace('np','')[4:] if 'np' in x else x for x in s]
	if direction==2: s = [x.replace('np','')[0:3]+'(x,y)'+x.replace('np','')[4:] if 'np' in x else x for x in s]
	if direction==3: s = [x.replace('np','')[0:3]+'(y,z)'+x.replace('np','')[4:] if 'np' in x else x for x in s]
	h[1] = '"'.join(s)
	s = ''.join(header_list[2]).split(',')
	if direction==1: s = [x+',' for x in s if 'I=' not in x.replace(' ','')]
	if direction==2: s = [x+',' for x in s if 'J=' not in x.replace(' ','')]
	if direction==3: s = [x+',' for x in s if 'K=' not in x.replace(' ','')]
	s[-1] = s[-1][:-1]
	if direction==3: s.append(' DATAPACKING = POINT\n')
	s = [x if not x.endswith('\n') else x[:-1] for x in s]
	h[2] = s
	h = [item for sublist in h for item in sublist]
	return h

def keep_direction_only_in_header_new(header_list,direction):
	h = header_list
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
	s = ''.join(header_list[2]).split(',')
	if direction==1: s = [x+',' for x in s if 'J=' not in x.replace(' ','') and 'K=' not in x.replace(' ','')]
	if direction==2: s = [x+',' for x in s if 'I=' not in x.replace(' ','') and 'K=' not in x.replace(' ','')]
	if direction==3: s = [x+',' for x in s if 'I=' not in x.replace(' ','') and 'J=' not in x.replace(' ','')]
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
	s = s2.split(',')
	suffix2 = '2'
	s = ['"'+k[1:-1]+'_'+suffix2+'"' for k in s]
	h[1] = s1+','.join(s[-3:])+'\n'
	return h
