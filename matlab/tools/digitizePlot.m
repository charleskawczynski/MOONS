% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;

dir.working = 'C:\Users\Charlie\Desktop\comparison\bandaru\processed\';
name.file = 'uvsz_at_half_pi_Hartmann_Q0.3';
% name.file = 'uvsz_at_pi_Hartmann_Q0.3';

name.ext = '.png';
griddata.xmin = 0;
griddata.xmax = 3;
griddata.ymin = -1;
griddata.ymax = 1;
smoothness = 15;
direction = 2;

[x y] = graphDigitizer(dir,name,griddata,smoothness,direction);

plot(x,y)


