clc; clear all; close all;

griddata.xmin = 0;
griddata.xmax = 1;
griddata.ymin = 0;
griddata.ymax = 60;
dir.working = 'C:\Users\Charlie\Documents\MOONS\DATA_OTHER_AUTHORS\mhd\Bandaru\fig_14\cleaned\';
name.file = 'beta=1e-1_cleaned';
name.ext = '.png';
ind.i_start = 1;
ind.i_end = 1;
ind.j_start = 1;
ind.j_end = 1;

props.samplingFrequency = 5;
props.darknessTol = 200;
props.smoothness = 1;
props.direction = 1;

[f,x,y] = graph_digitizer(dir,name,griddata,ind,props);