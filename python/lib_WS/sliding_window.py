import argparse
import time
import cv2
import imutils

import numpy as np
import dir_funcs as DF
import file_IO as IO
import tec_header_funcs as THF
import colormap_macro as CM
from itertools import islice

def pyramid(image, scale=1.5, minSize=(30, 30)):
	yield image
	while True:
		w = int(image.shape[1] / scale)
		image = imutils.resize(image, width=w)
		if image.shape[0] < minSize[1] or image.shape[1] < minSize[0]:
			break
		yield image

def sliding_window(image, stepSize, windowSize):
	for y in reversed(range(0, image.shape[0], stepSize)):
		for x in range(0, image.shape[1], stepSize):
			yield (x, y, image[y:y + windowSize[1], x:x + windowSize[0]])

def get_empty_window(file_name,show_windows):
	position = [100,0]
	default = True
	image = cv2.imread(file_name)
	(winW, winH) = (140, 128)
	nth_level = 0
	levels = list(pyramid(image, scale=1.25,minSize=(winW, winH)))
	n_levels = len(levels)
	print('n_levels = '+str(n_levels))
	for resized in levels:
		nth_level = nth_level+1
		for (x, y, window) in sliding_window(resized, stepSize=15, windowSize=(winW, winH)):
			Lx = x+winW<=1.1*resized.shape[0] # added a small buffer
			Ly = y+winH<.9*resized.shape[1] # added a small buffer
			if Lx and Ly:
				shape_total = window.shape[0]*window.shape[1]*window.shape[2]
				temp = window.reshape(shape_total)
				all_white_space = sum(temp)==255*shape_total
				if all_white_space:
					W = resized.shape[0]
					H = resized.shape[1]

					# top right position (TR)
					x_pos = x+winW
					y_pos = y
					position = [x_pos/W*100,y_pos/H*100] # want top right of box
					position = [min([x,100]) for x in position]
					position = [max([x,0]) for x in position]
					p_TR = position

					# between center and top right position (C)
					x_pos = .75*(x+winW)+.25*x
					y_pos = .25*(y+winH)+.75*y
					# x_pos = .87*(x+winW)+.13*(x)
					position = [x_pos/W*100,y_pos/H*100] # want top right of box
					position = [min([x,100]) for x in position]
					position = [max([x,0]) for x in position]
					p_C = position

					if nth_level==1:
						position = p_TR
					elif nth_level==1:
						position = p_C
					default = False
					if show_windows:
						print(' ---------------------------------------- ')
						# print('window.shape = '+str(window.shape))
						# print('(winW, winH) = '+str((winW, winH)))
						# print('x,y = '+str(x)+','+str(y))
						print('resized.shape = '+str(resized.shape))
						print('position = '+str(position))
						print('nth_level = '+str(nth_level))
						clone = resized.copy()
						cv2.rectangle(clone, (x, y), (x + winW, y + winH), (0, 255, 0), 2)
						cv2.imshow("Window", clone)
						cv2.waitKey(1)
						# time.sleep(0.25)
						time.sleep(0.025)
	position[1] = 100 - position[1] # y-direction is flipped
	return position
