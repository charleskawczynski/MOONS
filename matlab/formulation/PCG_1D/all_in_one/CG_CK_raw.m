function [x r L1 L2 Linf] = CG_CK_raw(operator,operator_explicit,x,b,p,N_inner,vol,c)
L1 = zeros(N_inner,1);
L2 = zeros(N_inner,1);
Linf = zeros(N_inner,1);

r=b;
r = multiply_wall_Neumann(r,0.5,x);
tempx = compute_Ax_BC_MF(operator_explicit,x,c);
Ax = operator(x,c);
Ax_BC = tempx.vals + Ax.vals;
% multiply_wall_Neumann is inside operator
r.vals = r.vals - Ax_BC;
r.vals = vol.*r.vals;
r = assign_wall_Dirichlet(r,0,x);

% Works
% Ax_BC = compute_Ax_BC_MF(operator_explicit,x,c);
% b_temp=b; b_temp = multiply_wall_Neumann(b,0.5,x);
% Ax = operator(x,c);
% % Ax = multiply_wall_Neumann(Ax,0.5,x); % Needs to be present when not in operator
% r.vals = vol.*(b_temp.vals - Ax_BC.vals - Ax.vals);
% r = assign_wall_Dirichlet(r,0,x);

disp(['mean(b) = ' num2str(mean(b.vals))])

z.vals = r.vals;
p.vals = z.vals;
rhok = r.vals'*z.vals;
for i = 1:N_inner
   Ax = operator(p,c);
   % Ax = multiply_wall_Neumann(Ax,0.5,x); % Needs to be present when not in operator
   Ax = assign_wall_Dirichlet(Ax,0,x);
   Ax = assign_wall_Periodic_single(Ax,0,x.BCs)
   Ax.vals = Ax.vals.*vol;
   % ghost points are not tracked by Ax since it's multiplied by vol and,
   % therefore, ghost points of r may not be updated, nor z nor p.
   alpha = rhok/(p.vals'*Ax.vals);
   x.vals = x.vals + alpha*p.vals;
   r.vals = r.vals - alpha*Ax.vals;
   x = apply_BCs(x,c);
   z.vals = r.vals;
   rhokp1 = z.vals'*r.vals;
   beta = rhokp1/rhok;
   p.vals = p.vals*beta + z.vals;
   rhok = rhokp1;

   norms = Ln_norms(r.vals);
   L1(i) = norms.L1;
   L2(i) = norms.L2;
   Linf(i) = norms.Linf;
   if L2(i)<1e-10 break
end

% Ax = operator(x,c);
% Ax = multiply_wall_Neumann(Ax,0.5,x); % Needs to be present when not in operator
% r.vals = vol.*(b_temp.vals - Ax_BC.vals - Ax.vals);
% r = assign_wall_Dirichlet(r,0,x);
% norms = Ln_norms(r.vals);
% L1(N_inner+1) = norms.L1;
% L2(N_inner+1) = norms.L2;
% Linf(N_inner+1) = norms.Linf;

end