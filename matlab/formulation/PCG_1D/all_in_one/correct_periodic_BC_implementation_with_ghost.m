% WORKS CORRECTLY
clc; clear all; close all

N_cells = 20;
a = 0;
b = 1;
L = b - a;
p = 2;
dx = L/N_cells;
x = a:dx:b;
coeff = 1/dx^2;
pad = 1;
S = 1+pad;
x = [x(1)-dx x x(end)+dx];
LX = length(x);
F = LX-pad;
FP = LX-pad-1;

f = -(p*pi)^2*cos(p*pi*x');
f(S:FP) = f(S:FP) - mean(f(S:FP));
f(1) = 0; f(end) = 0;
f(end-1) = 0;

I = eye(LX,LX);
d_lower = diag(ones(LX-1,1),1);
d_upper = diag(ones(LX-1,1),-1);
A = -2*I + d_lower+d_upper;
A(1,:) = 0; A(end,:) = 0; A(:,1) = 0; A(:,end) = 0;
A(1+pad,end-pad-1) = 1;
A(end-pad,1+pad) = 1;
disp('A = ')
disp(num2str(A))
disp(['N_cells = ' num2str(N_cells)])
disp('size(A) = ')
disp(num2str(size(A)))
A = coeff*A;

u = zeros(size(x));
u(S:FP) = A(S:FP,S:FP)\f(S:FP);
u = u - mean(u);
u(1) = 0; u(end) = 0;

u_exact = cos(p*pi*x');
u_exact = u_exact - mean(u_exact);
u_exact(1) = 0; u_exact(end) = 0;

figure
plot(x,u,'b-o',x,u_exact,'r-*')
title('solution')
legend('BS','exact')
