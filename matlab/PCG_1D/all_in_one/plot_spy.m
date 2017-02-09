function plot_spy(sp,print_full,A_in,c,fig_title)
if (sp(3)==1)
	figure
end
if print_full
	disp('-----------------------------------------------------------------------')
	A = round(A_in*c.dh^2);
	disp([fig_title ' = '])
	disp(num2str(A))
else
	A = round(A_in*c.dh^2);
end
subplot(sp(1),sp(2),sp(3))
spy(A)
title(fig_title)
end