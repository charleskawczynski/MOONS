%% ***************************************
% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;
% ***************************************

addpath('C:\Users\Charlie\Desktop\MOONS_poisson\out\')

name = struct;
name.file = 'norm_ADI';
name.ext = '.dat';
saveToFile = false;

[n norms] = plotNorms(name,saveToFile);
semilogy(n,norms); hold on

name.file = 'norm_SOR';
[n norms] = plotNorms(name,saveToFile);
semilogy(n,norms)
