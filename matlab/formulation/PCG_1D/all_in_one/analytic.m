function u_ana = analytic(u,x,forceType,BCs,p)

u_ana=u;
f = u.vals;
if strcmp(forceType,'zero'); f = x'*0;
elseif strcmp(forceType,'constant'); f = .5*x.^2-x+1;
elseif strcmp(forceType,'x'); f = x.^3/6 - 1/6*x;
elseif strcmp(forceType,'cos'); f = cos(p*pi*x');
elseif strcmp(forceType,'sin'); f = sin(p*pi*x');
elseif strcmp(forceType,'exp'); f = exp(x'.^2);
elseif strcmp(forceType,'gaussian'); f = x'*0;
end

if BCs.bc1.type.Neumann && BCs.bc2.type.Neumann
    f = subtract_mean(f);
end
f = zeroghostpoints(f);
u_ana.vals = f;
end