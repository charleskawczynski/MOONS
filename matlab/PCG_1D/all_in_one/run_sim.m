function run_sim(N_cells,forceType,p,BCs,N_inner,N_outer,print_full)
disp('*******************************************************************')
disp('********************** STARTED RUN ********************************')
disp('*******************************************************************')
%% PARAMS
N_total = N_outer*N_inner;

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

print_symmetry(A)

%% FORCING TERM
b_initial.c.vals = forcing(c.hc,p,forceType);
b_initial.n.vals = forcing(c.hn,p,forceType);

b.c.vals = b_initial.c.vals;
b.n.vals = b_initial.n.vals;

%% BC MODIFICATION
b.c = BC_modification(b.c,u.c,A.c,c);
b.n = BC_modification(b.n,u.n,A.n,c);

%% REMOVE NULLSPACE FOR PURE NEUMANN
b = nullspace_modification(b,BCs,'b');

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
u = nullspace_modification(u,BCs,'u');
CG_MIT.x = nullspace_modification(CG_MIT.x,BCs,'u CG MIT');
CG_CK.x  = nullspace_modification(CG_CK.x, BCs, 'u CG CK');
BS.x     = nullspace_modification(BS.x,    BCs,    'u BS');

u_ana.c    = analytic(u.c,c.h.c,forceType,BCs,p);
u_ana.n    = analytic(u.n,c.h.n,forceType,BCs,p);

%% PLOT RESULTS
plot_spy(          [4 4 1],print_full,A.c.explicit.vals,c,'A.c.explicit')
plot_spy(          [4 4 2],print_full,A.n.explicit.vals,c,'A.n.explicit')
plot_spy(          [4 4 3],print_full,A.c.vals,c,'A.c')
plot_spy(          [4 4 4],print_full,A.n.vals,c,'A.n')
new_fig(           [4 4 5],c.h,b_initial,'b','Forcing initial')
new_fig(           [4 4 6],c.h,b,'b','Modified forcing')
new_fig(           [4 4 7],c.h,BS.x,'u','Backslash solution')
new_fig(           [4 4 8],c.h,BS.r,'r','Backslash residual') % (should be zero)
new_fig(           [4 4 9],c.h,CG_MIT.x,'u','CG MIT solution')
new_fig(           [4 4 10],c.h,CG_MIT.r,'r','CG MIT residual')
new_fig(           [4 4 11],c.h,CG_CK.x,'u','CG CK solution')
new_fig(           [4 4 12],c.h,CG_CK.r,'r','CG CK residual')
residual_norms_fig([4 4 13],N_inner,N_outer,CG_MIT.norms,'CG MIT')
residual_norms_fig([4 4 14],N_inner,N_outer,CG_CK.norms,'CG CK')
new_fig(           [4 4 15],c.h,u_ana,'u','Analytic')

disp('*******************************************************************')
disp('********************* FINISHED RUN ********************************')
disp('*******************************************************************')
end