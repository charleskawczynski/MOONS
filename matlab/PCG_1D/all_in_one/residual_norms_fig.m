function residual_norms_fig(sp,N_inner,N_outer,norms,fig_title)

c.L2 = norms.c.L2(:);
n.L2 = norms.n.L2(:);
iter_set = 1:size(n.L2);
if (sp(3)==1)
	figure
end
subplot(sp(1),sp(2),sp(3))

iter = 1:N_inner; iter = iter';
for i=1:N_outer
	c.L2_subset = c.L2(iter);
	n.L2_subset = n.L2(iter);
	semilogy(iter(1),c.L2_subset(1),'r-o',...
		     iter(2:end),n.L2_subset(2:end),'b-o',...
		     iter(1),n.L2_subset(1),'r-*',...
		     iter(2:end),n.L2_subset(2:end),'b-*'); hold on
	iter = iter+N_inner;
end
title(fig_title)
xlabel('iteration')
xlabel('residual per iteration')
legend('(c) L2  1st iter','(c) L2','(n) L2  1st iter','(n) L2')

end