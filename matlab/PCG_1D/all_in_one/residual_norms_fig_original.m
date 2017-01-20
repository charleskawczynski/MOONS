function residual_norms_fig_original(sp,iter,norms,fig_title)

c.L2 = norms.c.L2(:);
n.L2 = norms.n.L2(:);
iter_set = 1:size(n.L2);
if (sp(3)==1)
	figure
end
subplot(sp(1),sp(2),sp(3))
semilogy(iter_set,c.L2,'k-o',...
	     iter_set,n.L2,'k-*')
title(fig_title)
xlabel('iteration')
xlabel('residual per iteration')
legend('CC data','Node data')
legend('(c) L2','(n) L2')

end