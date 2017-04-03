function [x res r Ln] = CG_MIT(b,A,n,c)
% Ln = struct('L1','L2','Linf',{n+1 n+1 n+1});
Ln.L1(n) = 0;
Ln.L2(n) = 0;
Ln.Linf(n) = 0;
A = A*c.dh;
b = b*c.dh;
d = b;                             % initial search direction
r = b;                        % initial residual
% r = b*c.dh;                        % initial residual
x = b*0;                           % initial solution
r2 = r'*r;
res = zeros(n+1,1);
res(1) = r2;
% Ln(1) = norms(r);
% [Ln.L1(1), Ln.L2(1), Ln.Linf(1)] = norms(r);
for i = 1:n
   Ad = A*d;                       % apply the matrix A
%    Ad = Ad*c.dh;
   alpha = r2/(d'*Ad);             % a first scalar product
   x = x+alpha*d;                  % update solution
   r = r-alpha*Ad;                 % update residual
   r2old = r2;
   r2 = r'*r;                      % a second scalar product
   beta = r2/r2old;
   d = r+beta*d;                   % new search direction
   res(i) = r2;
   [Ln.L1(i), Ln.L2(i), Ln.Linf(i)] = norms(r);
end
end