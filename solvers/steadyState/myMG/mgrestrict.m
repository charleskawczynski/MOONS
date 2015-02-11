function [ ru ] = mgrestrict( u )
%MGRESTRICT Restriction operator for 2-d multigrid
%   ru = MGRESTRICT(u) restrict the data u on a fine grid of uniform spacing
%      onto a coarser grid of uniform spacing with twice the value.
%
%   It is assumed that the Dirichlet boundary data lie in u.
%
%   The routine uses the 9-point restriction stencil that is the adjoint of
%   the 9-point prolongation defined in MGPROLONG
%
%   MAE 259A
%   J. D. Eldredge
%   2/24/2014

%% Set up the size of the restricted data
M = size(u,1)-1;
N = size(u,2)-1;
ru = zeros(M/2+1,N/2+1);
% Note that ru is fixed at zero on the boundary nodes

%% Interior coarse nodes
for jj = 2:N/2
    ru(2:M/2,jj) = 0.0625*u(2:2:M-2,2*jj-2)+ ...
                   0.125*u(3:2:M-1,2*jj-2)+ ...
                   0.0625*u(4:2:M,2*jj-2) + ...
                   0.125*u(2:2:M-2,2*jj-1)+ ...
                   0.25*u(3:2:M-1,2*jj-1)+ ...
                   0.125*u(4:2:M,2*jj-1) + ...
                   0.0625*u(2:2:M-2,2*jj)+ ...
                   0.125*u(3:2:M-1,2*jj)+ ...
                   0.0625*u(4:2:M,2*jj);
end




end

