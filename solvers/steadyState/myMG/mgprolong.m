function [ pu ] = mgprolong( u )
%MGPROLONG Prolongation operator for 2-d multigrid
%   pu = MGPROLONG(u) prolongs the data u on a coarse grid of uniform spacing
%      onto a finer grid of uniform spacing with half the value.
%
%   It is assumed that the Dirichlet boundary data lie in u.
%
%   The routine uses the standard 9-point prolongation operator
%
%   MAE 259A
%   J. D. Eldredge
%   2/24/2014

%% Set up the size of the prolonged data
M = size(u,1)-1;
N = size(u,2)-1;
pu = zeros(2*M+1,2*N+1);

%% Take care of coarse nodes that coincide with fine nodes. This also takes
% care of boundary data
for jj = 1:N+1
    pu(1:2:2*M+1,2*jj-1) = u(1:M+1,jj);
end

%% Now the midpoints in x direction
% and x-average contributions to the interstitial points
for jj = 1:N
    pu(2:2:2*M,2*jj-1) = 0.5* (u(1:M,jj) + u(2:M+1,jj));
    pu(2:2:2*M,2*jj  ) = 0.25*(u(1:M,jj) + u(2:M+1,jj) + u(1:M,jj+1) + u(2:M+1,jj+1));
end
pu(2:2:2*M,2*N+1) = 0.5* (u(1:M,N+1) + u(2:M+1,N+1));



%% Now midpoints in y direction
u = u';
pu = pu';
for ii = 1:M
    pu(2:2:2*N,2*ii-1) = 0.5*(u(1:N,ii) + u(2:N+1,ii));
end
pu(2:2:2*N,2*M+1) = 0.5*(u(1:N,M+1) + u(2:N+1,M+1));
u = u';
pu = pu';



end

