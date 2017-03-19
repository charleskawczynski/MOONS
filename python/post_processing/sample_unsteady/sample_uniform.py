import numpy as np
import math as m
import file_IO as IO

def process(file_name,N_desired_points):
	arr = np.loadtxt(file_name,skiprows=3)
	arr = get_nearly_uniform_data(arr,N_desired_points)
	H = IO.get_header(file_name) # Adjust header
	head = ''.join(H)
	delim = '	  '
	np.savetxt(file_name, arr, delimiter=delim, header = head, comments='') # Save file

def get_nearly_uniform_data(arr,N_desired_points):
	t = arr[:,0]
	i = get_uniform_index(t,N_desired_points)
	return arr[i,:]

def get_uniform_index(t,N_desired_points):
	t_uniform = np.linspace(np.amin(t),np.amax(t),N_desired_points)
	t_desired = [nearest(t_d, t) for t_d in t_uniform]
	i = np.in1d(t, t_desired)
	return i

def nearest(t_desired,t_all):
    index = (np.abs(t_all - t_desired)).argmin()
    return t_all[index]
