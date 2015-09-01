% triSolver prototype
clc; close all; clear all;

m = 40;
l = 0.5;
u = 1;
d = -2;
A = diag(l*ones(m-1,1),-1) + diag(d*ones(m,1)) + diag(u*ones(m-1,1),1)
[L, U] = lu(A);

[L U]



