from PIL import Image,ImageOps
import os
import numpy as np
import subprocess

def delete_auxiliary_files_present_dir(extensions):
	for e in extensions:
		for f in os.listdir('.'):
			if f.endswith(e):
				if os.path.isfile(f): os.remove(f)

def build_latex_file(file_name):
	subprocess.check_call(['xelatex', file_name+'.tex'])

def build_eps_from_latex(file_name):
	subprocess.run('latex --jobname='+file_name+' '+file_name+'.tex')
	subprocess.run('dvips '+file_name+'.dvi')

def eps_to_png(file_name):
	subprocess.run('gswin64c -sDEVICE=png256 -r600 -o '+file_name+'.png '+file_name+'.ps')

def get_cropped_line(occupied_elms,tolerance,S):
	if (sum(occupied_elms) == 0):
		cropBox = () # May be an empty row or column to be cropped
	else:
		occupied_min = occupied_elms.argmax()
		occupied_max = S - occupied_elms[::-1].argmax()+1
		cropBox = (occupied_min,occupied_max)
	return cropBox

def get_cropped_array(image_bw,tol):
	max_val = image_bw.max()
	tolerance = max_val*tol
	S = image_bw.shape
	occupied_elms = (image_bw<=tolerance).astype(int)
	S = occupied_elms.shape
	# Traverse rows
	cropBox = [get_cropped_line(occupied_elms[k,:],tolerance,S[1]) for k in range(0,S[0])]
	cropBox = [x for x in cropBox if x]
	# print(cropBox)
	if not cropBox: # all pixels blank
		xmin = [0]
		xmax = [S[0]]
	else:
		xmin = [k[0] for k in cropBox]
		xmax = [k[1] for k in cropBox]
	# Traverse cols
	cropBox = [get_cropped_line(occupied_elms[:,k],tolerance,S[0]) for k in range(0,S[1])]
	cropBox = [x for x in cropBox if x]
	if not cropBox: # all pixels blank
		ymin = [0]
		ymax = [S[1]]
	else:
		ymin = [k[0] for k in cropBox]
		ymax = [k[1] for k in cropBox]
	xmin = min(xmin)
	xmax = max(xmax)
	ymin = min(ymin)
	ymax = max(ymax)
	cropBox = (ymin, ymax, xmin, xmax)
	return cropBox

def is_greyscale(f):
	image=Image.open(f)
	image.load()
	image_data = np.asarray(image)
	S = image_data.shape
	if len(S)==3:
		return False
	elif len(S)==2:
		return True
	else:
		ValueError('Oops greyscale')

def auto_crop(file_name,grey_tolerance):
	# grey_tolerance = 0.9 # (0,1) = crop (more,less)
	if (is_greyscale(file_name)):
		image=Image.open(file_name).convert('L')
	else:
		image=Image.open(file_name)
	image_bw=Image.open(file_name)
	use_alternative_method = image_bw.mode=='RGBA'
	if not use_alternative_method: image_bw=image_bw.convert('L')
	image.load()
	image_bw.load()
	# Handle transparency regions:
	if use_alternative_method:
		m = image_bw.split()[-1] # or [3]
		background = Image.new("RGB", image_bw.size, (255, 255, 255))
		try:
			background.paste(image_bw, mask=m) # 3 is the alpha channel
		except:
			background.paste(image_bw)
		image_bw = background
		image_bw = image_bw.convert('L')
	image_data = np.asarray(image)
	image_data_bw = np.asarray(image_bw)
	S = image_data.shape
	S_bw = image_data_bw.shape
	cropBox = get_cropped_array(image_data_bw,grey_tolerance)
	if len(S)==3:
		image_data_new = image_data[cropBox[0]:cropBox[1]+1, cropBox[2]:cropBox[3]+1 , :]
	elif len(S)==2:
		image_data_new = image_data[cropBox[0]:cropBox[1]+1, cropBox[2]:cropBox[3]+1]
	else:
		ValueError('Oops')
	new_image = Image.fromarray(image_data_new)
	f_new = file_name
	new_image.save(f_new)


file_name = 'MOONS'
build_latex_file(file_name)
build_eps_from_latex(file_name)
eps_to_png(file_name)
delete_auxiliary_files_present_dir(['.aux','.txt','.dvi','.out','.log','.blg','.gz','.synctex.gz'])
auto_crop(file_name+'.png',0.9)
