function run_sim(N_cells,forceType,p,BCs,N_inner,N_outer,print_full)
disp('********************** STARTED RUN ********************************')
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
b_initial.c.vals = forcing(c.hc,p,forceType,BCs,b_initial.c.DL);
b_initial.n.vals = forcing(c.hn,p,forceType,BCs,b_initial.n.DL);

b_initial = disrupt_mean_pure_Neumann(b_initial,BCs);
b = b_initial;

%% BC MODIFICATION
b.c = BC_modification(b.c,u.c,A.c,c,'b.c');
b.n = BC_modification(b.n,u.n,A.n,c,'b.n');

%% REMOVE NULLSPACE FOR PURE NEUMANN
b_initial = nullspace_modification(b_initial,BCs,'b_initial');
b = nullspace_modification(b,BCs,'b');
b.c = assign_wall_Periodic_single(b.c,0,BCs);
b.n = assign_wall_Periodic_single(b.n,0,BCs);
b_initial.c = assign_wall_Periodic_single(b_initial.c,0,BCs);
b_initial.n = assign_wall_Periodic_single(b_initial.n,0,BCs);

%% PREPARE CG FOR NUMERICAL SOLUTION
CG_MIT = init_CG(u,A);
CG_CK = init_CG(u,A);
BS = init_CG(u,A);

% ***********************************************************************************
% ***********************************************************************************
% ***********************************************************************************

BS = soln_backslash(BS,b,A,c); % EXACT SOLUTION (BACKSLASH)
for iter=1:N_outer; CG_MIT = soln_CG_MIT(CG_MIT,b,N_inner,BCs); end
for iter=1:N_outer; CG_CK = soln_CG_CK(CG_CK,@Laplacian,@Laplacian_explicit,b_initial,N_inner,c); end

% ***********************************************************************************
% ***********************************************************************************
% ***********************************************************************************

%% REMOVE SOLUTION MEAN FOR PURE NEUMANN COMPARISON
u = nullspace_modification(u,BCs,'u');
CG_MIT.x = nullspace_modification(CG_MIT.x,BCs,'u CG MIT');
CG_CK.x  = nullspace_modification(CG_CK.x, BCs, 'u CG CK');
BS.x     = nullspace_modification(BS.x,    BCs,    'u BS');

CG_MIT.x.c = apply_BCs(CG_MIT.x.c,c);
CG_MIT.x.n = apply_BCs(CG_MIT.x.n,c);
CG_CK.x.c = apply_BCs(CG_CK.x.c,c);
CG_CK.x.n = apply_BCs(CG_CK.x.n,c);
BS.x.c = apply_BCs(BS.x.c,c);
BS.x.n = apply_BCs(BS.x.n,c);

u_ana.c    = analytic(u.c,c.h.c,forceType,BCs,p);
u_ana.n    = analytic(u.n,c.h.n,forceType,BCs,p);

%% PLOT RESULTS
plot_spy(          [2 2 1],print_full,A.c.explicit.vals,c,'A.c.explicit')
plot_spy(          [2 2 2],print_full,A.n.explicit.vals,c,'A.n.explicit')
plot_spy(          [2 2 3],print_full,A.c.vals,c,'A.c')
plot_spy(          [2 2 4],print_full,A.n.vals,c,'A.n')
new_fig(           [2 2 1],c.h,b_initial,'b','Forcing initial')
new_fig(           [2 2 2],c.h,b,'b','Modified forcing')
new_fig(           [2 2 3],c.h,BS.x,'u','Backslash solution')
new_fig(           [2 2 4],c.h,BS.r,'r','Backslash residual') % (should be zero)

new_fig_compare(   [2 1 1],c.h,BS.x,'BS',u_ana,'analytic','Backslash vs analytic')
new_fig_compare(   [2 1 2],c.h,CG_CK.x,'CK',u_ana,'analytic','CK vs analytic')
% new_fig(           [1 1 1],c.h,CG_CK.p,'p','p in CG_CK') % (should be zero)

% new_fig(           [2 2 1],c.h,BS.x_initial,'u','BS')
% new_fig(           [2 2 2],c.h,CG_MIT.x_initial,'u','CG_MIT')
% new_fig(           [2 2 3],c.h,CG_CK.x_initial,'u','CG_CK')

new_fig(           [2 2 1],c.h,CG_MIT.x,'u','CG MIT solution')
new_fig(           [2 2 2],c.h,CG_MIT.r,'r','CG MIT residual')
new_fig(           [2 2 3],c.h,CG_CK.x,'u','CG CK solution')
new_fig(           [2 2 4],c.h,CG_CK.r,'r','CG CK residual')
residual_norms_fig([2 2 1],N_inner,N_outer,CG_MIT.norms,'CG MIT')
residual_norms_fig([2 2 2],N_inner,N_outer,CG_CK.norms,'CG CK')
new_fig(           [2 2 3],c.h,u_ana,'u','Analytic')

disp('********************* FINISHED RUN ********************************')
end