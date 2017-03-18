clc; close all; clear all;
%% Computes the solution to the Poisson equation
% u_hh = h
% In  0 <= h <= L
% using conjugate gradient method with Dirichlet,
% mixed and pure Neumann boundary conditions.

%% NOTES ON OPERATOR AND ITS BOUNDARY ENTRIES
% 1) If ghost points of x are zero,
%    then A = A* if A* = 0 on the first
%    and last columns, where A is not
%    necessarily zero
% 2) If ghost points of Ax are set to zero,
%    then A = A* if A* = 0 on the first
%    and last rows, where A is not
%    necessarily zero

% This file should be able to pass the following 8 tests:
% 1) Dirichlet BCs, u = 0 on boundary, f ~ sin ----- check
% 2) Dirichlet BCs, u = 0 on boundary, f ~ cos ----- check
% 3) Neumann   BCs, g = 0 on boundary, f ~ cos ----- check
% 4) Dirichlet BCs, u = 1 on boundary, f ~ sin ----- check
% 5) Dirichlet BCs, u = 1 on boundary, f ~ cos ----- check

% 6) Neumann   BCs, g = 1 on boundary, f ~ gaussian?     BONUS (maybe for energy equation)

% These tests have been successfully completed on ______
% Put a checkmark () for each case when completed

%% PARAMS
forceType = 'zero'; % (x,cos,sin)
p = 2; % wavenumber, f = cos(p*pi*x), f = sin(p*pi*x)
N_cells = 13;
BCs.bc1.type = bctype(1); % (1,2) = (Dirichlet,Neumann)
BCs.bc2.type = bctype(1); % (1,2) = (Dirichlet,Neumann)
BCs.bc1.val = 1;
BCs.bc2.val = 0;

%% INIT GRID AND FIELDS
c = coordinates(N_cells);
u.vals = zeros(c.sn,1);
A.n = getOperator(@operator,u.n,c,false,false);
A.n_centered = getOperator(@operator,u.n,c,false,true);

An = full(A.n*c.dhc(1)^2)
An_centered = full(A.n_centered*c.dhc(1)^2)
symmetry = max(max(abs(An - An'))); disp(['symmetry = ' num2str(symmetry)])
figure
spy(A.n); title('A_n')

%% FORCING TERM
f.n.vals = forcing(c.hn,forceType,c,p,u.n);

%% ADJUST RHS WITH BCS
f.n.mvals = f.n.vals;
ubc = u.n; ubc.vals(:) = 0; ubc = apply_BCs(ubc,c,true,false); ubc = saveub(ubc); Aubc.n = A.n_centered*ubc.vals;
Aubc.n = saveOnlyAub(Aubc.n,u.n);
f.n.mvals = modify_forcing(f.n.mvals,u.n,Aubc.n);

%% REMOVE NULLSPACE FOR PURE NEUMANN
if u.c.BCs.bc1.type.Neumann && u.c.BCs.bc2.type.Neumann
    f.n.mvals = subtract_mean(f.n.mvals);
end

disp(['mean(f.n.mvals) = ' num2str(mean(f.n.mvals))])

modifier_fmn = Aubc.n

%% ANALYTIC SOLUTION
u.n.ana = analytic(u.n,c.hn,forceType,u.c.BCs,p);

%% PREPARE CG FOR NUMERICAL SOLUTION
tol = 1e-15; maxit = c.sn; M = eye(size(A));
N_iter = floor(N_cells*0.9);
u.n.exact = 0*u.n.vals;
r.n = init_N(c);
N = 30;
N_iter_n = N;

p = getPad(u.n.BCs);
[u.n.vals(1+p(1):end-p(2)),r.n.L2, r.n.vals(1+p(1):end-p(2)), norms.n] = ...
CG_MIT(f.n.mvals(1+p(1):end-p(2)),A.n(1+p(1):end-p(2),1+p(1):end-p(2)),N_iter_n,c);

%% EXACT SOLUTION (BACKSLASH)
u.n.exact(1+p(1):end-p(2)) = A.n(1+p(1):end-p(2),1+p(1):end-p(2))\f.n.mvals(1+p(1):end-p(2));

%% REMOVE SOLUTION MEAN FOR NEUMANN BCS COMPARISON
if u.n.BCs.bc1.type.Neumann && u.n.BCs.bc2.type.Neumann
    u.n.exact = subtract_mean(u.n.exact);
    u.n.ana = subtract_mean(u.n.ana);
    u.n.vals = subtract_mean(u.n.vals);
elseif u.n.BCs.bc1.type.Dirichlet && u.n.BCs.bc2.type.Dirichlet
    u.n.vals(2) = u.n.BCs.bc1.val;
    u.n.vals(end-1) = u.n.BCs.bc2.val;
    u.n.exact(2) = u.n.BCs.bc1.val;
    u.n.exact(end-1) = u.n.BCs.bc2.val;
end
disp(['mean(u.n.exact) = ' num2str(mean(u.n.exact))])

% figure
% semilogy(1:N_iter_c+1,res.r2c/norm(f.c),1:N_iter_n+1,res.r2n/norm(f.n.vals));
% xlabel('Iteration');
% ylabel('Relative residual for CG');
% legend('Cell center','Node')

% figure
% semilogy(1:N_iter_c+1,res.r2c,1:N_iter_n+1,res.r2n);
% xlabel('Iteration');
% ylabel('Residual for CG');
% legend('Cell center','Node')

figure
semilogy(1:N_iter_c,norms.c.L1,1:N_iter_c,norms.c.L2,1:N_iter_c,norms.c.Linf);
xlabel('Iteration');
ylabel('Residual for CG (Cell center)');
legend('L1','L2','Linf')

figure
plot(c.hc,f.c.vals,'b-o',c.hn,f.n.vals,'r-*')
title('Forcing')
xlabel('x')
ylabel('f')
legend('Cell center','Node')

figure
plot(c.hc,f.c.mvals,'b-o',c.hn,f.n.mvals,'r-*')
title('Modified forcing')
xlabel('x')
ylabel('f')
legend('Cell center','Node')

ymin = min(u.c.ana)*1.4;
ymax = max(u.c.ana)*1.4;
figure
plot(c.hc,u.c.vals,'b-o',c.hn,u.n.vals,'b-*')
title('CG solution')
xlabel('x')
ylabel('u')
% axis([0 1 ymin ymax])
legend('Cell center','Node')

figure
plot(c.hc,u.c.exact,'r-o',c.hn,u.n.exact,'r-*')
title('Backslash solution')
xlabel('x')
ylabel('u')
% axis([0 1 ymin ymax])
legend('Cell center','Node')

figure
plot(c.hc,u.c.ana,'k-o',c.hn,u.n.ana,'k-*')
title('Analytic solution')
xlabel('x')
ylabel('u')
legend('Cell center','Node')
% axis([0 1 ymin ymax])

figure
plot(c.hc,u.c.ana,'k-o',c.hn,u.n.ana,'k-*',c.hc,u.c.exact,'b-o',c.hn,u.n.exact,'b-*',c.hc,u.c.vals,'r-o',c.hn,u.n.vals,'r-*')
title('Solution comparison')
xlabel('x')
ylabel('u')
% axis([0 1 ymin ymax])
legend('analytic (CC)','analytic (N)','Backslash (CC)','Backslash (N)','CG (CC)','CG (N)')

figure
plot(c.hc(2:end-1),r.c.vals(2:end-1),'r-o',c.hn(2:end-1),r.n.vals(2:end-1),'r-*')
title('Spatial residual')
xlabel('x')
ylabel('Residual')
legend('Cell center','Node')
% axis([0 1 ymin ymax])

% figure
% plot(c.hc(2:end-1),r.c.vals(2:end-1),'r-o')
% title('Spatial residual (cell center)')
% xlabel('x')
% ylabel('Residual')
% legend('Cell center','Node')
% axis([0 1 ymin ymax])

norms.c