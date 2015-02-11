% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); format short
% ***************************************
%% Analytic solution of flow in a square duct (modified)
% This solution was found in 
%
% 1. Kashaninejad, N., Chan, W. K. & Nguyen, N. Analytical and Numerical
% Investigations of the Effects of Microchannel Aspect Ratio on Velocity 
% Profile and Friction Factor. 1–9 (2012).

% I've modified this so that the bounds can be specified rather from 0 to
% 2h,2w.

ax = -1; bx = 1;
ay = -1; by = 1;
h = (by-ay)/2; 
w = (bx - ax)/2;
M = 50; N = 50;
I = 100; J = 100;
x = linspace(ax,bx,I);
y = linspace(ay,by,J);
u = zeros(I,J);
alpha = w/h;
F = 100;

% Fortran format
% for i=1:I
%     for j=1:J
%         for m=1:M
%             for n=1:N
%                 A1 = 16*F*alpha^2*h^2/((m*pi)^2+(alpha*n*pi)^2);
%                 A2 = 1/(m*pi)*1/(n*pi);
%                 A3 = (1-cos(m*pi))*(1-cos(n*pi));
%                 A = A1*A2*A3;
%                 u(i,j) = u(i,j) + A*sin(m*pi*x(i)/(2*w))*sin(n*pi*y(j)/(2*h));
%             end
%         end
%     end
% end

% Matlab vectorized format
for m=1:M
    for n=1:N
        A1 = 16*F*alpha^2*h^2/((m*pi)^2+(alpha*n*pi)^2);
        A2 = 1/(m*pi)*1/(n*pi);
        A3 = (1-cos(m*pi))*(1-cos(n*pi));
        A = A1.*A2.*A3;
        u = u + A*sin(m.*pi*(x-ax)/(2*w))'*sin(n.*pi*(y-ay)/(2*h));
    end
end


[X Y] = meshgrid(x,y);

surf(X,Y,u')
title(['Fully Developed Square Duct Flow (exact), F = ' F])
xlabel('x')
ylabel('y')
zlabel('u(x,y)')

