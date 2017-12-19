clear; clc; close all;
syms A1 B1 C1 D1 A2 B2 C2 D2
syms A_B
syms A_U
syms Theta_U
syms Theta_B
syms G
syms D

% Final desired form:
M = [A_U A_U*Theta_U*G 0       0             ;
     D      0          0       0             ;
     0      0          A_B     A_B*Theta_B*G ;
     0      0          D       0             ]

% LU, computed from 'wxMaxima' software:
% http://andrejv.github.io/wxmaxima/
L = [1     0     0     0;
     D/A_U 1     0     0;
     0     0     1     0;
     0     0     D/A_B 1]
U = [A_U   A_U*G*Theta_U  0             0        ;
      0     -D*G*Theta_U  0             0        ;
      0      0            A_B      A_B*G*Theta_B ;
      0      0            0          -D*G*Theta_B]

% Trick for decomposing into LDU picked up from:
% http://employees.oneonta.edu/GoutziCJ/fall_1999/math323/matlab/lesson_05.pdf
D = diag(diag(U));
U = inv(D)*U

% Some sanity checks:
zero_original = L*D*U - M

% Final result:
L_new = L*D
U_new = U

