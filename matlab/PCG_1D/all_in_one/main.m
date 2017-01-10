% clc; close all; clear all;
clc;
close all;
% clear all;
%% PARAMS

% Global params
% forceType = string, (zero,constant,x,gaussian,sin,cos)
N_cells = 100;                 % number of cells
print_full = false;            % print matrices
p = 2;                        % wavenumber, b = cos(p*pi*x), b = sin(p*pi*x)
N_total = 200;
N_inner = 2*N_cells;          % iterations inside CG
N_outer = 1;                  % restarts CG

% Laplace equation, 1 at left side, linear to 0 at right side
forceType = 'zero';           % (zero,constant,x,gaussian,sin,cos)
BCs.bc1.type = bctype(1);     % (1,2) = (Dirichlet,Neumann)
BCs.bc2.type = bctype(1);     % (1,2) = (Dirichlet,Neumann)
BCs.bc1.val = 1;              % BC value
BCs.bc2.val = 0;              % BC value
N_outer = 1; N_inner = floor(N_total/N_outer);
run_sim(N_cells,forceType,p,BCs,N_inner,N_outer,print_full)
N_outer = 3; N_inner = floor(N_total/N_outer);
run_sim(N_cells,forceType,p,BCs,N_inner,N_outer,print_full)

% f = cos(p*x), pure Neumann BCs (needs special treatment)
% forceType = 'cos';            % (zero,constant,x,gaussian,sin,cos)
% BCs.bc1.type = bctype(2);     % (1,2) = (Dirichlet,Neumann)
% BCs.bc2.type = bctype(2);     % (1,2) = (Dirichlet,Neumann)
% BCs.bc1.val = 0;              % BC value
% BCs.bc2.val = 0;              % BC value
% N_outer = 1; N_inner = floor(N_total/N_outer);
% run_sim(N_cells,forceType,p,BCs,N_inner,N_outer,print_full)
% N_outer = 10; N_inner = floor(N_total/N_outer);
% run_sim(N_cells,forceType,p,BCs,N_inner,N_outer,print_full)

% f = x, increases from 1 (left) to zero slope (right)
forceType = 'constant';       % (zero,constant,x,gaussian,sin,cos)
BCs.bc1.type = bctype(1);     % (1,2) = (Dirichlet,Neumann)
BCs.bc2.type = bctype(2);     % (1,2) = (Dirichlet,Neumann)
BCs.bc1.val = 1;              % BC value
BCs.bc2.val = 0;              % BC value
N_outer = 1; N_inner = floor(N_total/N_outer);
run_sim(N_cells,forceType,p,BCs,N_inner,N_outer,print_full)
N_outer = 3; N_inner = floor(N_total/N_outer);
run_sim(N_cells,forceType,p,BCs,N_inner,N_outer,print_full)



