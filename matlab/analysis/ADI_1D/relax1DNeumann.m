function u = relax1DNeumann(un,f,alpha,dt,n,h)
%% Solves the equation
% (I - .5 dt alpha B) u = (I + .5 dt alpha B) un - dt*f
% for u. Where B = tridiag(1,-2,1)

% Construct RHS
[loDiag, Diag, upDiag] = setUpSystemNeumann(.5*dt*alpha,h,n);
Diag(2:end-1) = 1 + Diag(2:end-1);
A = diag(Diag) + diag(upDiag,1) + diag(loDiag,-1);

% u_ghost = zeros(1,2); g = 0; % Ghost point / slope for Neumann

% u_ghost(1) = un(2) - h*g;
% u_ghost(2) = un(end-1) - h*g;

% f(1) = f(1) - alpha/(h^2)*u_ghost(1);
% f(end) = f(end) - alpha/(h^2)*u_ghost(2);

rhs = A*un - dt*f;
rhs(1) = 0; rhs(end) = 0;
% rhs = rhs - mean(rhs);

[loDiag, Diag, upDiag] = setUpSystemNeumann(-.5*dt*alpha,h,n);
Diag(2:end-1) = 1 + Diag(2:end-1);
% u = trisolve(loDiag,Diag,upDiag,rhs,'reg');
u = zeros(size(un));
u(2:end-1) = trisolve(loDiag(2:end-1),Diag(2:end-1),upDiag(2:end-1),rhs(2:end-1),'reg');
% u(1) = u(2);
% u(end) = u(end-1);

g = 0;
u(1) = 1/3*(4*u(2) - 2*h*g - u(3));
u(end) = -1/3*(4*u(end-1) - 2*h*g - u(end-1));

end