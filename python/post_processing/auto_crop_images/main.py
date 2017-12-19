import Image
from os import listdir
from os.path import isfile, join
import numpy as np

def auto_crop_images(file_name,ext):
	if ext == '.png' or ext == '.jpg':
		file_name_new = file_name.replace(ext,'')+'_cropped'+ext
		image=Image.open(file_name)
		image.load()
		imageSize = image.size
		imageBox = image.getbbox()
		print imageBox
		cropped=image.crop(imageBox)
		cropped.save(file_name_new)

def auto_crop_images_new_new(file_name,ext):
	if ext == '.png' or ext == '.jpg':
		file_name_new = file_name.replace(ext,'')+'_cropped'+ext
		im = Image.open(file_name)
		pix = np.asarray(im)
		pix = pix[:,:,0:3] # Drop the alpha channel
		idx = np.where(pix-255)[0:2] # Drop the color when finding edges
		box = map(min,idx)[::-1] + map(max,idx)[::-1]
		pad = 2
		box[0] = box[0]-pad # xmin measured from left
		box[1] = box[1]-pad # ymin measured from top
		box[2] = box[2]+pad # xmax
		box[3] = box[3]+pad # ymax
		region = im.crop(box)
		region_pix = np.asarray(region)
		region.save(file_name_new)

def auto_crop_images_new(file_name,ext):
	if ext == '.png' or ext == '.jpg':
		file_name_new = file_name.replace(ext,'')+'_cropped'+ext
		image=Image.open(file_name)
		image.load()
		image_data = np.asarray(image)
		image_data_bw = image_data.max(axis=2)
		non_empty_columns = np.where(image_data_bw.max(axis=0)>0)[0]
		non_empty_rows = np.where(image_data_bw.max(axis=1)>0)[0]
		cropBox = (min(non_empty_rows), max(non_empty_rows), min(non_empty_columns), max(non_empty_columns))
		image_data_new = image_data[cropBox[0]:cropBox[1]+1, cropBox[2]:cropBox[3]+1 , :]
		new_image = Image.fromarray(image_data_new)
		new_image.save(file_name_new)


PS = '\\'
R1 = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS
R2 = 'MOONS'+PS+'matlab'+PS+'formulation'+PS+'matrix_visualization'+PS+'figs'+PS
root = R1+R2
file_path = root
files = [k for k in listdir(file_path) if isfile(join(file_path, k))]
files = []
files.append('PCG_SF_Lap_CC.png')
files.append('PCG_VF_Lap_CC.png')
files.append('PCG_VF_Lap_Face.png')
files.append('PCG_VF_curlcurl_CC.png')
files.append('PCG_VF_curlcurl_Face.png')

file_list = [root+f for f in files]
print 'FILES: '
print '\n'.join(files)
ext_list = [FL[-4:] for FL in file_list]
print '\n\n'

for FL,ext in zip(file_list,ext_list):
	print 'FL = '+FL.replace(root,'')
	auto_crop_images_new_new(FL,ext)

