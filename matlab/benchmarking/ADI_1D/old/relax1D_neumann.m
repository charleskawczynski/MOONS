function u = relax1D_neumann(un,f,alpha,dt,n,h)
%% Solves the equation
% (I - .5 dt alpha B) u = (I + .5 dt alpha B) un - dt*f
% for u. Where B = tridiag(1,-2,1)

% Construct RHS
[loDiag, Diag, upDiag] = setUpSystemNeumann(.5*dt*alpha,h,n);
Diag(2:end-1) = 1 + Diag(2:end-1);
A = diag(Diag) + diag(upDiag,1) + diag(loDiag,-1);
f(1) = f(1) - un(3)/h^2;
f(end) = f(end) - un(end-2)/h^2;
rhs = A*un - dt*f;

[loDiag, Diag, upDiag] = setUpSystemNeumann(-.5*dt*alpha,h,n);
Diag(2:end-1) = 1 + Diag(2:end-1);
u = trisolve(loDiag,Diag,upDiag,rhs,'reg');

end