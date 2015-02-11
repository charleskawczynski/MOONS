function [ v ] = mgsmooth( u, f, dx, varargin )
%MGSMOOTH Perform smoothing of data in a multigrid algorithm
%   v = MGSMOOTH(u,f,dx) performs one smoothing iteration of data u on a
%     grid with uniform spacing dx and right-hand side f, using five-point
%     red-black Gauss-Seidel relaxation.
%          u(i,j) = ( u(i-1,j)+u(i+1,j)+u(i,j-1)+u(i,j+1) )/4
%                       - dx^2*f(i,j)/4
%
%     It is assumed that the array u holds prescribed boundary values in
%     the boundary elements. Also, f must be the same size as u.
%
%   v = MGSMOOTH(u,f,dx,niter) performs 'niter' iterations of the smoothing
%     operation.
%
%   MAE 259A
%   J. D. Eldredge
%   2/24/2014

%% Set the number of iterations
niter = 1;
if (nargin > 3),
    niter = varargin{1};
end

%% Put RHS data in temporary array. Get grid sizes
% Store boundary data
v = u;
M = size(u,1)-1;
N = size(u,2)-1;
fact = -0.25*dx*dx;
%f = -0.25*dx*dx*f;

%% This form is a bit more readable
%% Start the smoothing sweeps
% for isweep = 1:niter
%     % Loop through even and odd checkerboard squares
%     for color = 0:1
%         stag = color;
%         for jj = 2:N % Loop through interior rows
%             v(2+stag:2:end-1-stag,jj) = fact*f(2+stag:2:end-1-stag,jj) + 0.25*( ...
%                 v(1+stag:2:end-2-stag,jj) + v(3+stag:2:end-stag,jj));
%             stag = 1-stag; % Switch between even-odd nodes from row to row   
%         end
%         % Now transpose in order to add in the (i,j-1) and (i,j+1) elements
%         v = v';
%         stag = color;
%         for ii = 2:M % Loop through interior columns
%             v(2+stag:2:end-1-stag,ii) = v(2+stag:2:end-1-stag,ii) + 0.25*( ...
%                 v(1+stag:2:end-2-stag,ii) + v(3+stag:2:end-stag,ii));
%             stag = 1-stag; % Switch between even-odd nodes from column to column   
%         end
%         % Transpose back
%         v = v';
%         
%     end
% end

%% This form is more efficient
[I,J] = meshgrid(1:M+1,1:N+1);
red = mod(I+J,2);
black = ones(size(red))-red;
fred = f.*red;
for isweep = 1:niter
    % Get only the red squares on the checkerboard
    vred = v.*red;
    
    % The Laplacian of vred only gathers information from the arms of the
    % stencil at each black point, since the vred = 0 at the stencil
    % center.
    vblack = fact*f + 0.25*lap2d(vred).*black;
    
    % Zero the boundary entries in vblack, so they don't contribute to vred
    vblack(1:M+1,[1 N+1]) = 0;
    vblack([1 M+1],1:N+1) = 0;
    
    % Need to zero out the entries at the red squares in vblack.
    vblack = vblack.*black;
    
    % Now perform the smoothing from the new black square data at the red
    % squares. Also, add the black squares in.
    v = fact*fred + vblack + 0.25*lap2d(vblack).*red;
    
    % Finally, zero out the boundary elements
    v(1:M+1,[1 N+1]) = 0;
    v([1 M+1],1:N+1) = 0;
end





end

