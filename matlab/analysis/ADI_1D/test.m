%% ***************************************
% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;

Gr = [1 5 10 50 100 500]*10^6;
Re = 5000;

P1 = -5.98*10^(-8)*Re^2 + 2.284*10^(-3)*Re + 2.308
P2 = 1.8277*10^-6*Re^2 - 7.3037*10^-2*Re - 22.787
P3 = -1.37*10^-5*Re^2 + 0.57516*Re - 95.8


Ha_cr = P1*(log(Gr)).^2 + P2*log(Gr) + P3


semilogx(Gr,Ha_cr,'r-')
xlabel('Gr')
ylabel('Ha_{cr}')

