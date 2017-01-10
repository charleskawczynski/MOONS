function b = forcing(x,p,forceType)
    if strcmp(forceType,'zero'); b = x'*0;
elseif strcmp(forceType,'constant'); b = x'*0+1;
elseif strcmp(forceType,'x'); b = x';
elseif strcmp(forceType,'gaussian'); b = exp(-x'.*x'/p);
elseif strcmp(forceType,'sin'); b = -(p*pi)^2*sin(p*pi*x');
elseif strcmp(forceType,'cos'); b = -(p*pi)^2*cos(p*pi*x');
else; error('Bad force type')
end
b = zeroghostpoints(b);
end