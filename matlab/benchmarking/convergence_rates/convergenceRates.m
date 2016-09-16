% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;

%% Active directory
% myDir.source = 'C:\Users\Charlie\Desktop\MOONS\out\LDC\';
myDir.source = 'C:\Users\Charlie\Desktop\MOONS - BMCs\BMC_100 - 3rd grid refinement test, includes error\';

f = [myDir.source 'e(U(1_to_2)).dat'];
d = importdata(f,' ',1);
e.L1 = d.data(:,1); e.L2 = d.data(:,2); e.Linf = d.data(:,3);
e.R1 = d.data(:,4); e.R2 = d.data(:,5); e.Rinf = d.data(:,6);
N = 2.^(5:6);
plotConvergenceRate(myDir,N,e)



