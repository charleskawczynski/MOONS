function convergenceRates(L1,L2,Linf,R)
% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;

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
end

