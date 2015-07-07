% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;

dir = struct; name = struct;

dir.working = 'C:\Users\Charlie\Desktop\comparison\Shatrov\processed\';
name.file = '3D stability region (figure 9a)';
name.ext = '.png';
griddata.xmin = 0;
griddata.xmax = 6000;
griddata.ymin = 0;
griddata.ymax = 100;
smoothness = 8;
direction = 2;

[x y] = graphDigitizer(dir,name,griddata,smoothness,direction);

plot(x,y)

