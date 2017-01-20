function [x r L1 L2 Linf] = CG_MIT_raw(x,A,b,n)
r = b-A*x;                  % initial residual
p = r;                      % initial search direction
r2 = r'*r;

L1 = zeros(n,1);
L2 = zeros(n,1);
Linf = zeros(n,1);
for i = 1:n
   Ad = A*p;                % apply the matrix A
   alpha = r2/(p'*Ad);      % a first scalar product
   x = x+alpha*p;           % update solution
   r = r-alpha*Ad;          % update residual
   r2old = r2;
   r2 = r'*r;               % a second scalar product
   beta = r2/r2old;
   p = r+beta*p;            % new search direction

   norms = Ln_norms(r);
   L1(i) = norms.L1;
   L2(i) = norms.L2;
   Linf(i) = norms.Linf;
end
end