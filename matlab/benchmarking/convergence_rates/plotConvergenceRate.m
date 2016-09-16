function plotConvergenceRate(myDir,N,e)
% function plotConvergenceRate(myDir,L1,L2,Linf,R1,R2,Rinf)
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;
L1 = e.L1;
L2 = e.L2;
Linf = e.Linf;
R1 = e.R1;
R2 = e.R2;
Rinf = e.Rinf;

R = log(N);
p = -1; y1 = exp(p*log(R) + log(mean(R)));
p = -2; y2 = exp(p*log(R) + log(mean(R)));
% p = -1; y1 = exp(p*log(R) + log(mean(R)));
% p = -2; y2 = exp(p*log(R) + log(mean(R)));
figure
fs = 14;
loglog(N,L1,'b-+'); hold on
loglog(N,L2,'b-+'); hold on
loglog(N,Linf,'b-+'); hold on
loglog(N,y2,'r-',N,y1,'g-');
title('Error vs N Cells (loglog)','fontsize',fs)
xlabel('N Cells','fontsize',fs)
ylabel('Error','fontsize',fs)
AX=legend('Result','slope=-2','slope=-1');
LEG = findobj(AX,'type','text');
set(LEG,'FontSize',fs)
xlim([10 ceil(max(N)/100)*100])
end

