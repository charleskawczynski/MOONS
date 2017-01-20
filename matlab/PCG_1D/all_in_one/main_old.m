% clc; close all; clear all;
%% Solves Poisson equation
%         Laplacian(u) = b,      0 <= h <= L
% using CG method for several BCs and CC/N data.

%% PARAMS
forceType = 'zero'; % (zero,constant,gaussian,cos,sin)
p = 0.1; % wavenumber, b = cos(p*pi*x), b = sin(p*pi*x)
N_cells = 12;
BCs.bc1.type = bctype(1); BCs.bc1.val = 1; % (1,2) = (Dirichlet,Neumann)
BCs.bc2.type = bctype(1); BCs.bc2.val = 0; % (1,2) = (Dirichlet,Neumann)
% N_inner = floor(N_cells*0.9); % iterations inside CG
N_inner = 15; % iterations inside CG
N_outer = 1;                  % restarts CG
N_total = N_outer*N_inner;
print_full = true;

%% INIT GRID AND FIELDS
c = coordinates(N_cells);
u = init_field(c);
u = init_BCs(u,BCs);
b = init_field(c);
b_initial = init_field(c);

A.c.vals = getOperator(@Laplacian,u.c,c);
A.n.vals = getOperator(@Laplacian,u.n,c);
A.c.explicit.vals = getOperator(@Laplacian_explicit,u.c,c);
A.n.explicit.vals = getOperator(@Laplacian_explicit,u.n,c);

print_symmetry(A,c,print_full)

%% FORCING TERM
b_initial.c.vals = forcing(c.hc,p,forceType);
b_initial.n.vals = forcing(c.hn,p,forceType);

b.c.vals = b_initial.c.vals;
b.n.vals = b_initial.n.vals;

%% COMPUTE AX_BC
Ax_BC.c = compute_Ax_BC(A.c,u.c,c);
Ax_BC.n = compute_Ax_BC(A.n,u.n,c);

%% BC MODIFICATION
b.c = BC_modification(b.c,u.c,Ax_BC.c);
b.n = BC_modification(b.n,u.n,Ax_BC.n);

%% REMOVE NULLSPACE FOR PURE NEUMANN
b.c = nullspace_modification(b.c,u.c,'b.c');
b.n = nullspace_modification(b.n,u.n,'b.n');

%% COMPUTE ANALYTIC AND BACKSLASH SOLUTION
u_ana.c.vals = analytic(u.c,c.hc,forceType,u.c.BCs,p);
u_ana.n.vals = analytic(u.n,c.hn,forceType,u.n.BCs,p);
u_BS.c.vals = 0*u.c.vals;
u_BS.n.vals = 0*u.n.vals;

%% PREPARE CG FOR NUMERICAL SOLUTION
CG_MIT = init_CG(u,A);
CG_CK = init_CG(u,A);
BS = init_CG(u,A);

% ***********************************************************************************
% ***********************************************************************************
% ***********************************************************************************

BS = soln_backslash(BS,b,A,c); % EXACT SOLUTION (BACKSLASH)
for iter=1:N_outer; CG_MIT = soln_CG_MIT(CG_MIT,b,N_inner,BCs); end
CG_MIT.x.c = apply_BCs(CG_MIT.x.c,c);
CG_MIT.x.n = apply_BCs(CG_MIT.x.n,c);
for iter=1:N_outer; CG_CK = soln_CG_CK(CG_CK,@Laplacian,@Laplacian_explicit,b_initial,N_inner,c); end

% ***********************************************************************************
% ***********************************************************************************
% ***********************************************************************************

%% REMOVE SOLUTION MEAN FOR PURE NEUMANN COMPARISON
u_BS.c  = nullspace_modification(u_BS.c, u.c,'u_BS.c');
u_BS.n  = nullspace_modification(u_BS.n, u.n,'u_BS.n');
u_ana.c = nullspace_modification(u_ana.c,u.c,'u_ana.c');
u_ana.n = nullspace_modification(u_ana.n,u.n,'u_ana.n');
u.c     = nullspace_modification(u.c,    u.c,'u.c');
u.n     = nullspace_modification(u.n,    u.n,'u.n');

new_fig([2 2 1],c.h,b_initial,'b','Forcing initial')
new_fig([2 2 2],c.h,b,'b','Modified forcing')
new_fig([2 2 3],c.h,BS.x,'u','Backslash solution')
new_fig([2 2 4],c.h,BS.r,'r','Backslash residual') % (should be zero)

new_fig([2 2 1],c.h,CG_MIT.x,'u','CG MIT solution')
new_fig([2 2 2],c.h,CG_MIT.r,'r','CG MIT residual')
new_fig([2 2 3],c.h,CG_CK.x,'u','CG CK solution')
new_fig([2 2 4],c.h,CG_CK.r,'r','CG CK residual')

residual_norms_fig([2 1 1],N_total,CG_MIT.norms,'CG MIT')
residual_norms_fig([2 1 2],N_total,CG_CK.norms,'CG CK')

