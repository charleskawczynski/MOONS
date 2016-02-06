% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;

dir = struct; name = struct;

% dir.working = 'C:\Users\Charlie\Desktop\comparison\Pattison\processed\';
% gd.xmin = 0; gd.xmax = 1; gd.ymin = -0.4;gd.ymax = 1;
% name.file = 'Ha0_NSE';
% name.file = 'Ha0_this_work';
% name.file = 'Ha45_NSE';
% name.file = 'Ha45_this_work';
% name.ext = '.png';
% 
% dir.working = 'C:\Users\Charlie\Desktop\comparison\Guj and Stella\data\';
% gd.xmin = -0.4; gd.xmax = 1; gd.ymin = 0;gd.ymax = 1;
% name.file = 'Re400_uvsy';
% % name.file = 'Re1000_uvsy';
% name.ext = '.png';


% dir.working = 'C:\Users\Charlie\Documents\comparison\Ghia_2D_LDC\processed\'
% name.file = 'Patil_compared_with_Ghia';
% name.ext = '.png';
% gd.xmin = -0.495; gd.xmax = 1; gd.ymin = -0.015;gd.ymax = 1;


dir.working = 'C:\Users\Charlie\Documents\comparison\Weiss\'
name.file = 'Weiss_energy_vs_time_Rem1000';
name.ext = '.png';
gd.xmin = 0; gd.xmax = 4; gd.ymin = 0; gd.ymax = 24;

smoothness = 1;
direction = 1;
darknessTol = 0.9;
[x y] = graphDigitizer(dir,name,gd,smoothness,direction,darknessTol);

plot(x,y,'ro')

