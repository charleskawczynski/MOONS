%% ***************************************
% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;
% ***************************************


for m = 3:100
    A = diag(-2*ones(2*m+1,1)) + diag(ones(2*m,1),1) + diag(ones(2*m,1),-1);

    e = eig(A);
    plot(e); hold on
end