import sys
import os
import numpy as np
clear = lambda: os.system('cls'); clear()

def scale_rgb(r_original,g_original,b_original,n_control_points,scale):
	r = np.ceil(r_original*scale)
	g = np.ceil(g_original*scale)
	b = np.ceil(b_original*scale)
	LS = np.floor(np.linspace(0,len(r)-1,n_control_points)).astype(int)
	r_temp = [r[i] for i in LS]; r = r_temp
	g_temp = [g[i] for i in LS]; g = g_temp
	b_temp = [b[i] for i in LS]; b = b_temp
	return (r,g,b)


def export_colormap(color_map):
	r_original = np.loadtxt(color_map+'/r.dat',skiprows=0)
	g_original = np.loadtxt(color_map+'/g.dat',skiprows=0)
	b_original = np.loadtxt(color_map+'/b.dat',skiprows=0)

	continuous = False
	scale = 256
	n_control_points = 7

	(r,g,b) = scale_rgb(r_original,g_original,b_original,n_control_points,scale)
	if np.amax(r)>=scale or np.amax(g)>=scale or np.amax(b)>=scale:
		(r,g,b) = scale_rgb(r_original,g_original,b_original,n_control_points,scale-1)

	print 'r'; print r
	print 'g'; print g
	print 'b'; print b
	n = [i for i in range(len(b))]
	nc = [i+1 for i in range(len(b))]

	if len(r)!=7: raise ValueError('Error: Adjust n_control_points so that len(r)=7')

	file_name = color_map+'_colormap.mcr'
	c = []
	c.append('#!MC 1120')
	c.append('# Created by Tecplot 360 build 11.2-0-563')
	c.append('$!GLOBALCOLORMAP 1  CONTOURCOLORMAP = USERDEF')
	c.append('$!GLOBALCOLORMAP 1  USERDEFINED{NUMCONTROLPOINTS = 7}')
	c.append('$!COLORMAPCONTROL 1 REDISTRIBUTECONTROLPOINTS')
	s = '$!GLOBALCOLORMAP 1  USERDEFINED{CONTROLPOINT '
	for i in n[:-1]:
		c.append(s+str(nc[i])+' {LEADRGB{R = '+str(r[i])+'}}}')
		c.append(s+str(nc[i])+' {LEADRGB{G = '+str(g[i])+'}}}')
		c.append(s+str(nc[i])+' {LEADRGB{B = '+str(b[i])+'}}}')
		c.append(s+str(nc[i])+' {TRAILRGB{R = '+str(r[i])+'}}}')
		c.append(s+str(nc[i])+' {TRAILRGB{G = '+str(g[i])+'}}}')
		c.append(s+str(nc[i])+' {TRAILRGB{B = '+str(b[i])+'}}}')
	c.append(s+str(nc[-1])+' {TRAILRGB{R = '+str(r[-1])+'}}}')
	c.append(s+str(nc[-1])+' {TRAILRGB{G = '+str(g[-1])+'}}}')
	c.append(s+str(nc[-1])+' {TRAILRGB{B = '+str(b[-1])+'}}}')
	if continuous: c.append('$!GLOBALCONTOUR 1  COLORMAPFILTER{COLORMAPDISTRIBUTION = CONTINUOUS}')
	c.append('')

	f = open(file_name, "w")
	f.write('\n'.join(c))
	f.close()
	print 'Done'

color_maps = ['option_a', 'option_b', 'option_c', 'viridis', 'fake_purula']
for cm in color_maps:
	export_colormap(cm)

