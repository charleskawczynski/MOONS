import numpy as np
import math as m

def get_dt(p,n):
	fp = open(p)
	for i, line in enumerate(fp):
	    if i == n:
	        TS = line[11:]
	        break
	fp.close()
	T = np.fromstring(TS, sep=' ')
	dt = T[1]
	return dt

def get_and_check_dt(root):
    dt_mom = get_dt(root+'parameters\\info_mom.dat',5)
    dt_ind = get_dt(root+'parameters\\info_ind.dat',4)
    if dt_ind != dt_mom:
        print '*************************************'
        print '*************************************'
        print '*************************************'
        print 'Time steps are not equal.'
        print '*************************************'
        print '*************************************'
        print '*************************************'
    else: 
        # print 'Time steps are equal.'
        dt = dt_mom
    return dt

def get_header(p):
	fp = open(p)
	H = []
	for i, line in enumerate(fp):
	    if i < 3:
	        H.append(line)
	    else:
	        break
	fp.close()
	return H

def post_process(root,file_old,file_new,var_suffix,ext):
    dt = get_and_check_dt(root)
    export_new(file_old+ext,file_new+ext,var_suffix,dt)

def export_new(file_old,file_new,var_suffix,dt):
    arr = np.loadtxt(file_old,skiprows=3)
    # Convert to t
    t = arr[:,0]*dt
    arr[:,0] = t
    s = arr.shape
    N_desired_points = 300
    n_skip = m.ceil(s[0]/N_desired_points)
    # Sample output
    arr = arr[0::n_skip,:]
    # Adjust header
    H = get_header(file_old)
    TF = False
    # Make header specific
    if TF:
        H[1] = H[1][:-1]+'_'+var_suffix+H[1][-1]
        H[1] = H[1][0:12] + H[1][11:].replace('=','')
        H[1] = H[1].replace('N','t_'+var_suffix)
    else:
        H[1] = H[1].replace('N','t')

    print H
    head = ''.join(H)
    delim = '      '
    # Save file
    np.savetxt(file_new, arr, delimiter=delim, header = head, comments='')
    # print 'data exported'

def copy_file(file_old,file_new):
    # Get data
    arr = np.loadtxt(file_old,skiprows=3)
    # Get header
    H = get_header(file_old)
    head = ''.join(H)
    delim = '      '
    # Save file
    np.savetxt(file_new, arr, delimiter=delim, header = head, comments='')

def get_data(f):
    # Get data
    arr = np.loadtxt(f,skiprows=3)
    # Get header
    H = get_header(f)
    head = ''.join(H)
    return (arr,head)

def get_vec_data(d):
    x = [k[0] for k in d]
    y = [k[1] for k in d]
    return (x,y)
