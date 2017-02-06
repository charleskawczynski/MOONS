function new_fig_compare(sp,x,y,label_y,y2,label_y2,fig_title)
if (sp(3)==1)
	figure
end
subplot(sp(1),sp(2),sp(3))
plot(x.c,y.c.vals,'b-o',x.n,y.n.vals,'b-*',x.c,y2.c.vals,'r-o',x.n,y2.n.vals,'r-*')
title(fig_title)
xlabel('x')
xlabel([label_y ' and ' label_y2])
legend(['CC ' label_y],['Node ' label_y],['CC ' label_y2],['Node ' label_y2])

end