% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;

dir.working = 'C:\Users\Charlie\Desktop\comparison\Guj and Stella\data\';
name.file = 'Re1000_uvsy';
name.ext = '.png';
griddata.xmin = -0.4;
griddata.xmax = 1;
griddata.ymin = 0;
griddata.ymax = 1;
smoothness = 15;
direction = 2;

[x y] = graphDigitizer(dir,name,griddata,smoothness,direction);

plot(x,y)


