function plot_field(m,sp,f,label_z,fig_title)
if (sp(3)==1)
	figure
end
subplot(sp(1),sp(2),sp(3))
surf(m.x.hn(2:end-1),m.y.hn(2:end-1),f.vals(2:end-1,2:end-1))
title(fig_title)
xlabel('x')
ylabel('y')
zlabel(label_z)
end