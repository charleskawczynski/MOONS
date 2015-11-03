clc; close all; clear all;
% pcg test

L = 1;
N = 20;
dx = L/N;
k = 3*pi;
bctype = 2;

x = linspace(0,L,N+1)';
% x = [min(x)-dx; x; max(x)+dx];
switch bctype
    case 1; u_exact = -sin(k*x);
    case 2; u_exact = -cos(k*x);
end
u_exact = u_exact - mean(u_exact);

m = length(x);
A = (diag(-2*ones(m,1)) + diag(ones(m-1,1),1) + diag(ones(m-1,1),-1));
% A(1,:) = 0; A(end,:) = 0;

% f = [0; diff(diff(u_exact))/dx^2; 0];
% i = 2;   f(i) = (-u_exact(i)+u_exact(i+1))/dx^2;
% i = m-1; f(i) = (-u_exact(i)+u_exact(i-1))/dx^2;

f = [0; diff(diff(u_exact))/dx^2; 0];
% i = 1; f(i) = (-u_exact(i)+u_exact(i+1))/dx^2;
% i = m; f(i) = (-u_exact(i)+u_exact(i-1))/dx^2;
A

A = A/dx^2;
Au(1) = 0; Au(end) = 0;
% Au - f

u_num = (f'*inv(A))';
% u_num = u_exact;

loDiag = diag(A,-1);
Diag = diag(A,0);
upDiag = diag(A,1);

% u_num(2:end-1) = trisolve(loDiag(2:end-1),Diag(2:end-1),upDiag(2:end-1),f(2:end-1),'reg')

% u_num(1) = u_exact(1); u_num(end) = u_exact(end);

Au = A*u_num;
% Au = A*u_exact;

e = u_exact - u_num;
R = abs(f - A*u_num);

figure
subplot(3,1,1)
plot(x,f)
ylabel('f')
title('\nabla^2 u = f')
subplot(3,1,2)
plot(x,Au)
ylabel('Au')
subplot(3,1,3)
plot(x,R)
xlabel('x')
ylabel('residual')

figure
subplot(3,1,1)
plot(x,u_exact)
ylabel('exact')
title('\nabla^2 u = f')
% axis([min(x) max(x) min(u_exact) max(u_exact)])
subplot(3,1,2)
plot(x,u_num)
xlabel('x')
ylabel('numerical')
% axis([min(x) max(x) min(u_exact) max(u_exact)])
subplot(3,1,3)
plot(x,e)
xlabel('x')
ylabel('error')

