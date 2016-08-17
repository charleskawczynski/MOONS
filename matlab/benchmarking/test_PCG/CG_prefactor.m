% Finding pre-factor for solving Ax = b with non-uniform grid CG method.
clc; close all; clear all;


% There may exist some diagonal matrix D, such that
% D*A*x = D*b
% Where D*A is symmetric

A = load('A_lap.dat');

D = symmetry_prefactor(A,false);

S = D*A;
error = S - S';
e = max(max(abs(error)))


