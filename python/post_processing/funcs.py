import numpy as np
import math as m
from os import listdir
from os.path import isfile, join
import pylab
import matplotlib.pyplot as plt
import math as m
import numpy as np

def get_dt(file_name,n):
	fp = open(file_name)
	for i, line in enumerate(fp):
	    if i == n:
	        TS = line[11:]
	        break
	fp.close()
	T = np.fromstring(TS, sep=' ')
	dt = T[1]
	return dt

def print_error(e):
    for k in range(0,5): print '*************************************'
    print e
    for k in range(0,5): print '*************************************'

def get_and_check_dt(root):
    dt_mom = get_dt(root+'parameters\\info_mom.dat',5)
    dt_ind = get_dt(root+'parameters\\info_ind.dat',4)
    if dt_ind != dt_mom: print_error('Time steps are not equal.')
    else: dt = dt_mom
    return dt

def convert_N_to_t(root,file_old,file_new,var_suffix,ext):
    dt = get_and_check_dt(root)
    convert_N_to_t_given_dt(file_old+ext,file_new+ext,var_suffix,dt)

def convert_N_to_t_given_dt(file_old,file_new,var_suffix,dt):
    arr = np.loadtxt(file_old,skiprows=3)
    t = arr[:,0]*dt # Convert to t
    arr[:,0] = t
    s = arr.shape
    N_desired_points = 300
    n_skip = m.ceil(s[0]/N_desired_points)
    arr = arr[0::n_skip,:] # Sample output
    H = get_header(file_old) # Adjust header
    TF = False
    if TF: # Make header specific
        H[1] = H[1][:-1]+'_'+var_suffix+H[1][-1]
        H[1] = H[1][0:12] + H[1][11:].replace('=','')
        H[1] = H[1].replace('N','t_'+var_suffix)
    else:
        H[1] = H[1].replace('N','t')
    print H
    head = ''.join(H)
    delim = '      '
    np.savetxt(file_new, arr, delimiter=delim, header = head, comments='') # Save file


def get_header(file_name):
    fp = open(file_name)
    H = []
    for i, line in enumerate(fp):
        if i < 3: H.append(line)
        else: break
    fp.close()
    return H

def copy_file(file_old,file_new):
    arr = np.loadtxt(file_old,skiprows=3) # Get data
    H = get_header(file_old) # Get header
    head = ''.join(H)
    delim = '      '
    np.savetxt(file_new, arr, delimiter=delim, header = head, comments='') # Save file

def get_data(f):
    arr = np.loadtxt(f,skiprows=3) # Get data
    H = get_header(f) # Get header
    head = ''.join(H)
    return (arr,head)

def get_vec_data(d): return ([k[0] for k in d],[k[1] for k in d])

def plot_all_files_in_path(file_path,x_label,y_label):
    onlyfiles = [k for k in listdir(file_path) if isfile(join(file_path, k))]
    ymax = 0
    for filename in onlyfiles:
        file = file_path+'\\'+filename
        (arr,header) = get_data(file)
        (x,y) = get_vec_data(arr)
        ymax = np.fabs(y)
        ymax = np.amax(ymax)
        print filename
        plt.plot(x,y,label=filename[3:-4])
    plt.xlabel(x_label)
    plt.ylabel(y_label)
    plt.title(x_label + ' vs. ' + y_label)
    plt.legend(loc=4,prop={'size':10})
    plt.axis([0, 40, 0, ymax*1.2])
    # plt.legend(loc='upper left')
    plt.show()
