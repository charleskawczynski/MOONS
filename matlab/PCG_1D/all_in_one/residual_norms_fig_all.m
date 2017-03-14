function residual_norms_fig_all(sp,iter,norms,fig_title)

c.L1 = norms.c.L1(:);
c.L2 = norms.c.L2(:);
c.Linf = norms.c.Linf(:);

n.L1 = norms.n.L1(:);
n.L2 = norms.n.L2(:);
n.Linf = norms.n.Linf(:);
iter_set = 1:size(n.Linf);
if (sp(3)==1)
	figure
end
subplot(sp(1),sp(2),sp(3))
semilogy(iter_set,c.L1,'b-o',...
	     iter_set,c.L2,'k-o',...
	     iter_set,c.Linf,'g-o',...
	     iter_set,n.L1,'b-*',...
	     iter_set,n.L2,'k-*',...
	     iter_set,n.Linf,'g-*')
title(fig_title)
xlabel('iteration')
xlabel('residual per iteration')
legend('CC data','Node data')
legend('(c) L1',...
	   '(c) L2',...
	   '(c) Linf',...
	   '(n) L1',...
	   '(n) L2',...
	   '(n) Linf')

end