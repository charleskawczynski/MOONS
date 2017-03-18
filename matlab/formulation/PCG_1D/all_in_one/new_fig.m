function new_fig(sp,x,y,label_y,fig_title)
if (sp(3)==1)
	figure
end
subplot(sp(1),sp(2),sp(3))
plot(x.c,y.c.vals,'b-o',x.n,y.n.vals,'b-*')
title(fig_title)
xlabel('x')
xlabel(label_y)
legend('CC data','Node data')
end