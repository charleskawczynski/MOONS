function u_dual = interp(u,c)
if (length(u) == c.sc)
    u_dual = zeros(1,c.sn);
    i=1:c.sc-1;
    u_dual(i+1) = 0.5*(u(i)+u(i+1));
else
    u_dual = zeros(1,c.sc);
    i=1:c.sn-1;
    u_dual(i) = 0.5*(u(i)+u(i+1));
end
u_dual = u_dual';
end