%% ***************************************
% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;
% ***************************************

k = 3:10;
R = 2.^k;
L1 = [5.5e-2 2.94e-2 1.5151e-2 7.6923e-3 3.87e-3 1.9455e-3 9.7465e-4 4.878e-4];
L2 = [2.347e-2 8.63e-3 3.117e-3 1.1146e-3 3.96e-4 1.405e-4 4.976e-5 1.7606e-5];
Linf = [1.388e-2 3.676e-3 9.4696e-4 2.403e-4 6.05e-5 1.5199e-5 3.807e-6 9.527e-7];

% R = log(R);
p = -1; y1 = exp(p*log(R) + log(mean(R)));
p = -2; y2 = exp(p*log(R) + log(mean(R)));
figure
fs = 14;
loglog(R,Linf,'b-+'); hold on
loglog(R,y2,'r-',R,y1,'g-');
title('Error vs Grid size (loglog)','fontsize',fs)
xlabel('Grid size','fontsize',fs)
ylabel('Error','fontsize',fs)
AX=legend('Result','slope=-2','slope=-1');
LEG = findobj(AX,'type','text');
set(LEG,'FontSize',fs)

