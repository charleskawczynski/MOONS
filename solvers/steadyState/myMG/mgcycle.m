function uc = mgcycle(lev,resf,varargin)
%MGCYCLE Perform one descent of a multigrid cycle 
%   uc = MGCYCLE(res,params) takes the residual resf from a finer grid at level
%        lev-1 and returns the smoothed correction uc at level lev as well
%        as the norm of the residual at that level. It may perform further
%        descent into coarser grids, depending on the number of levels
%        specified in params.rundata.mg.maxlev
%
%   MAE 259A
%   J. D. Eldredge
%   2/24/2014

%% Unpack the arguments
params = varargin{1};
griddata = params.griddata;
rundata = params.rundata;

%% Unpack the multigrid parameters
% Number of smoothing sweeps after each restriction
niter1 = rundata.mg.niter1;
% Number of smoothing sweeps after each prolongation
niter2 = rundata.mg.niter2;
% Number of coarseness levels to descend. Finest grid is level 1, and
% coarsest grid is level maxlev
maxlev = rundata.mg.maxlev;
% GS tolerance
gstol = rundata.mg.gstol;
% GS max iterations
gsmaxiter = rundata.mg.gsmaxiter;

%% Get the grid parameters at level lev and re-set the griddata structure
dx = 2*griddata.dx;
dxsq = dx*dx;
griddata.dx = dx;
griddata.dy = dx;
griddata.M = griddata.M/2;
griddata.N = griddata.N/2;
params.griddata = griddata;

%% Restrict residual onto grid at level lev and use this as RHS data
f = mgrestrict(resf);


%% Perform further descent, if we are not at the bottom yet
if (lev < maxlev),
    % Perform smoothing at this level. Start with zero correction, since we
    % are only computing what will be added to the next-finer grid's
    % correction after prolongation
    uc = zeros(size(f));
    uc = mgsmooth(uc,f,dx,niter1);
    
    % Compute the residual
    res = f - lap2d(uc,griddata);
    
    % Zero the boundary values
    res(1:griddata.M+1,[1 griddata.N+1]) = 0;
    res([1 griddata.M+1],1:griddata.N+1) = 0;    
    
    % Cycle through next coarser level
    ucc = mgcycle(lev+1,res,params);
    % ucc contains correction at coarser level lev+1
    % Prolong this correction to this level (lev)
    uc = uc + mgprolong(ucc);
    
    % Further smoothing
    uc = mgsmooth(uc,f,dx,niter2);
      
else
    % At coarsest level. Solve exactly.
    %uc = poisson2d(f,griddata);     

    uc = zeros(size(f));
    for iter = 1:gsmaxiter
        ucnp1 = mgsmooth(uc,f,griddata.dx);
        temp = norm(uc-ucnp1,inf);
        uc = ucnp1;
  
        if (temp < gstol),
            break
        end
    end
    
end



end

