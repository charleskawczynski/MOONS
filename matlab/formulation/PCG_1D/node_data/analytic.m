function u = analytic(u_in,x,forceType,BCs,p)

ub = BCs.bc1.val;
if forceType == 'x'
    u = x.^3/6 - 1/6*x;
elseif forceType == 'zero'
    u = x'*0;
elseif forceType == 'cos'
    u = cos(p*pi*x');
elseif forceType == 'sin'
    u = sin(p*pi*x');
elseif forceType == 'exp'
    u = exp(x'.^2);
end

if BCs.bc1.type.Dirichlet
    if u_in.is_CC
        u = u - (u(1)+u(2))/2;
    elseif u_in.is_N
        u = u - u(2);
    else
        error('Bad input to analytic')
    end
    u = u + BCs.bc1.val;
end
if BCs.bc2.type.Dirichlet
    if u_in.is_CC
        u = u - (u(end)+u(end-1))/2;
    elseif u_in.is_N
        u = u - u(end-1);
    else
        error('Bad input to analytic')
    end
    u = u + BCs.bc2.val;
end

if BCs.bc1.type.Neumann && BCs.bc2.type.Neumann
    u = subtract_mean(u);
end
u = zeroghostpoints(u);
end