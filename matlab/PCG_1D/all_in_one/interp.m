function x_dual = interp(x,c)
if (length(x) == c.sc)
    x_dual = zeros(1,c.sn);
    i=1:c.sc-1;
    x_dual(i+1) = 0.5*(x(i)+x(i+1));
else
    x_dual = zeros(1,c.sc);
    i=1:c.sn-1;
    x_dual(i) = 0.5*(x(i)+x(i+1));
end
x_dual = x_dual';
end