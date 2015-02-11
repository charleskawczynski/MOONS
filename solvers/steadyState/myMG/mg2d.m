function [ u, errarray, res ] = mg2d( ui, f, varargin )
%MG2D Perform multigrid solution of 2-d Poisson equation
%   u = MG2D(ui,f,params) solves the 2d Poisson problem
%    
%            (d^2/dx^2 + d^2/dy^2) u = f
%   
%   for the scalar field u, given the initial guess ui, the
%   force field f on a 2-d collocated grid (or, the nodes of a staggered
%   grid). The Dirichlet data are assumed to be absorbed into f.
%
%   The grid information is provided in params.griddata. It
%   is assumed that grid spacing is uniform and the same in both
%   directions.
%
%   Multigrid parameters are in the structure params.rundata.mg
%
%   MAE 259A
%   J. D. Eldredge
%   2/24/2014

%% Unpack the arguments
params = varargin{1};
griddata = params.griddata;
rundata = params.rundata;

%% Get the grid parameters of finest grid
dx = griddata.dx;


%% Get the multigrid parameters
% Number of initial smoothing sweeps at finest level
niteri = rundata.mg.niteri;
% Number of final smoothing sweeps at finest level
niterf = rundata.mg.niterf;
% Number of coarseness levels to descend. Finest grid is level 1, and
% coarsest grid is level maxlev
maxlev = rundata.mg.maxlev;
% Max number of cycles to perform
maxcycle = rundata.mg.maxcycle;
% MG solution tolerance
tol = rundata.mg.tol;
% GS tolerance
gstol = rundata.mg.gstol;
% GS max iterations
gsmaxiter = rundata.mg.gsmaxiter;

% Set initial guess for u
u = ui;
clear errarray
for iter = 1:maxcycle
          
    %% Start the multigrid V-cycle
    if (maxlev > 1),
        % Perform smoothing sweeps on finest level.
        u = mgsmooth(u,f,dx,niteri);
        % Get the residual on finest level (level 1)
        res = f - lap2d(u,griddata);
        % Zero the boundary values
        res(1:griddata.M+1,[1 griddata.N+1]) = 0;
        res([1 griddata.M+1],1:griddata.N+1) = 0;
        
        % Perform the cycle, starting at level 2
        ucc = mgcycle(2,res,params);
        % Prolong correction to grid level 1
        u = u + mgprolong(ucc);
        % Final smoothing sweeps
        u = mgsmooth(u,f,dx,niterf);
        
    else
        % Only one level. Just use relaxation to convergence.
        for riter = 1:gsmaxiter
            unp1 = mgsmooth(u,f,griddata.dx);
            temp = norm(u-unp1,inf);
            u = unp1;
            
            if (temp < gstol),
                break
            end
        end
        
    end
    
    % Final residual
    res = f - lap2d(u,griddata);
    % Zero the boundary values
    res(1:griddata.M+1,[1 griddata.N+1]) = 0;
    res([1 griddata.M+1],1:griddata.N+1) = 0;
    % Norm of residual
    rnorm = norm(res,inf);
   
    % Store the residal error for diagnostics
    errarray(iter) = rnorm;
    
    if (rnorm < tol),
        break
    end
end
    
    



end

