clc; clear all; close all;
mu_0 = 1.2566546543547774e-06;

% Expected
B0 = 5;
fac = 1/mu_0;
sigma_expected = fac*.5*B0*B0/10^6;
disp(['sigma_expected [MPa] = ' num2str(sigma_expected)])

% Actual

B0 = 5;
stress = 66.7943; % Al*{B^0}^2
stress_c = .5*stress; % .5* Al*{B^0}^2 = 33.39

Al = 2.6717726092511431;
Re_m = 2.4900000000000002;
Re = 26310000;
Ha = 13230.000000000000;
rho = 9300;
L = .1;
B_T = B0;
U = 28.29721007304601;
Bc = 1;
Al_correct = fac*Bc^2/(rho*U^2)

stress_test = stress_c/Al*fac/10^6;
disp(['stress_test [MPa] = ' num2str(stress_test)])

stress_test_new = rho*U^2*stress_c/Al*(Al_correct)/10^6;
disp(['stress_test_new [MPa] = ' num2str(stress_test_new)])

A = Bc^2*fac;
disp(['A = ' num2str(A)])
B = rho*U^2*Al;
disp(['B = ' num2str(B)])

Al_used = Ha^2/(Re*Re_m)
Al_computed = 5^2/(rho*U^2)*fac


