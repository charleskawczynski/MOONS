function f = forcing(x,forceType,c,p,u)
if strcmp(forceType,'x')
    f = x'; % x
elseif strcmp(forceType,'zero')
    f = x'*0;
elseif strcmp(forceType,'sin')
    f = -(p*pi)^2*sin(p*pi*x');
elseif strcmp(forceType,'cos')
    f = -(p*pi)^2*cos(p*pi*x');
elseif strcmp(forceType,'exp')
    f = -exp(-x'.^2);
else
    error('Bad force type')
end
f = zeroghostpoints(f);
end