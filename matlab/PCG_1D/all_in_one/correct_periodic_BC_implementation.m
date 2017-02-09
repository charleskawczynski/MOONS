% WORKS CORRECTLY
clc; clear all; close all

N_cells = 20;
a = 0;
b = 1;
L = b - a;
p = 2;
dx = L/N_cells;
x = a:dx:b-dx;
coeff = 1/dx^2;
S = length(x);

f = -(p*pi)^2*cos(p*pi*x');
f = f - mean(f);
I = eye(S,S);
d_lower = diag(ones(S-1,1),1);
d_upper = diag(ones(S-1,1),-1);
A = -2*I + d_lower+d_upper;
A(1,end) = 1; A(end,1) = 1;
disp('A = ')
disp(num2str(A))
disp(['N_cells = ' num2str(N_cells)])
disp('size(A) = ')
disp(num2str(size(A)))
A = coeff*A;

u = A\f;
u = [u; u(1)];
x = [x x(end)+dx];
u = u - mean(u);

u_exact = cos(p*pi*x');
u_exact = u_exact - mean(u_exact);
figure
plot(x,u,'b-o',x,u_exact,'r-*')
title('solution')
legend('BS','exact')
