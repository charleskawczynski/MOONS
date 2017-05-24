clear; clc; close all;
syms A_u
syms A_B
syms Theta_u
syms Theta_B
syms G
syms D

M = sym([A_u A_u*Theta_u*G 0 0;
     D 0 0 0;
     0 0 A_B A_B*Theta_B*G;
     0 0 D 0])

% M = [A_u A_u*Theta_u*G 0 0;
%      D 0 0 0;
%      0 0 A_B A_B*Theta_B*G;
%      0 0 D 0]

[L,U] = LU(M)


