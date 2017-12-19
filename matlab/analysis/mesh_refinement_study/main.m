clc; close all; clear all

r_0 = [1 1 1];
% x.N_FWV = [30 6 12];
% x.N_FWV = [40 8 14];
% x.N_FWV = [80 14 8];

% -------------- Shatrov case (took 8 days):
% The grid was modified in order to ensure that the ratio of dx_i to dx_i+1 was less than 2
% x.N_FWV = [128 20 20]; z = x; y = x;
% x.N_FWV = [80 14 16]; z = x; y = x;
% x.N_FWV = [128 20 20]; z = x; y = x;
% --------------

x.N_FWV = [140 14 14]; z = x; y = x;
r = 1.25*r_0;
disp(['Ratio increase factor = ' num2str(r)])
print_N_cells(x,y,z,r,-4)
print_N_cells(x,y,z,r,-3)
print_N_cells(x,y,z,r,-2)
print_N_cells(x,y,z,r,-1)
print_N_cells(x,y,z,r,0)

x.N_FWV = [140 14 14]; z = x; y = x;
r = 1.5*r_0;
disp(['Ratio increase factor = ' num2str(r)])
print_N_cells(x,y,z,r,-4)
print_N_cells(x,y,z,r,-3)
print_N_cells(x,y,z,r,-2)
print_N_cells(x,y,z,r,-1)
print_N_cells(x,y,z,r,0)

x.N_FWV = [140 14 14]; z = x; y = x;
r = [2 1.5 1.5];
disp(['Ratio increase factor = ' num2str(r)])
print_N_cells(x,y,z,r,-2)
print_N_cells(x,y,z,r,-1)
print_N_cells(x,y,z,r,0)

% x.N_FWV = [80 10 10]; z = x; y = x;
x.N_FWV = [20 4 4]; z = x; y = x;
r = [2 1.5 1.5];
disp(['Ratio increase factor = ' num2str(r)])
print_N_cells(x,y,z,r,0)
print_N_cells(x,y,z,r,1)
print_N_cells(x,y,z,r,2)
print_N_cells(x,y,z,r,3)


% disp(' ------- Extremely Coarse ------- ')
% disp(' --------- Very Coarse ---------- ')
% disp(' ----------- Coarse ------------- ')
% disp(' ---------- Moderate ------------ ')
% disp(' ------------ Fine -------------- ')