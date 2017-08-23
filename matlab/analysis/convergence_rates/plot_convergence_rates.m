function plot_convergence_rates(x,y,x_label,y_label,fig_title)

p = -1; y1_linear = p*log(x);
p = -2; y2_linear = p*log(x);
p = -20; yn_linear = p*log(x);

y1_linear = y1_linear - mean(y1_linear) + mean(log(y)); % Vertically shift to (x,y)
y2_linear = y2_linear - mean(y2_linear) + mean(log(y)); % Vertically shift to (x,y)
yn_linear = yn_linear - mean(yn_linear) + mean(log(y)); % Vertically shift to (x,y)

y = y;
y1 = exp(y1_linear);
y2 = exp(y2_linear);
yn = exp(yn_linear);

fs = 14;

% ---------------------------------------------------------------
Lx = log(x);
Ly1 = log(y1);
Ly2 = log(y2);
Lyn = log(yn);
Ly = log(y);

slopes = diff(Ly)./diff(Lx);
mean_slope = mean(slopes);
disp(['mean slope = ' num2str(mean_slope)])

% figure
% plot(Lx,Ly,'b-+'); hold on
% plot(Lx,Ly2,'r-',Lx,Ly1,'g-');
% title(fig_title,'fontsize',fs)
% xlabel(x_label,'fontsize',fs)
% ylabel(y_label,'fontsize',fs)
% AX=legend('Result','slope=-2','slope=-1');
% LEG = findobj(AX,'type','text');
% set(LEG,'FontSize',fs)
% ---------------------------------------------------------------
figure
plot(Lx,Ly,'b-+')
title([fig_title ' for best curve fit'],'fontsize',fs)
xlabel(x_label,'fontsize',fs)
ylabel(y_label,'fontsize',fs)
AX=legend('Result');
LEG = findobj(AX,'type','text');
set(LEG,'FontSize',fs)
% ---------------------------------------------------------------
x = exp(Lx);
y1 = exp(Ly1);
y2 = exp(Ly2);
yn = exp(Lyn);
y = exp(Ly);
figure
loglog(x,y,'b-+'); hold on
% loglog(x,y2,'r-',x,y1,'g-'); hold on
loglog(x,y1,'g-'); hold on
loglog(x,y2,'r-'); hold on
loglog(x,yn,'k-')
title(fig_title,'fontsize',fs)
xlabel(x_label,'fontsize',fs)
ylabel(y_label,'fontsize',fs)
% AX=legend('Result','slope=-2','slope=-1');
% AX=legend('Result','slope=-1','slope=-2','slope=-1');
AX=legend('Result','slope=-1','slope=-2','slope=-n');
LEG = findobj(AX,'type','text');
set(LEG,'FontSize',fs)
% ---------------------------------------------------------------

end