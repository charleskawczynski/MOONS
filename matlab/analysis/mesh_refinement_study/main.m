clc; close all; clear all

% x.N_FWV = [30 6 12];
% x.N_FWV = [40 8 14];
% x.N_FWV = [80 14 8];

% -------------- Shatrov case (took 8 days):
% The grid was modified in order to ensure that the ratio of dx_i to dx_i+1 was less than 2
x.N_FWV = [80 14 16]; z = x; y = x;
y.N_FWV(1) = y.N_FWV(1) - 13; % Approximate
% --------------

% y.N_FWV(2) = y.N_FWV(2) + 2;
% y.N_FWV(3) = y.N_FWV(3) + 2;

r = 1.25;

disp(['Ratio increase factor = ' num2str(r)])
disp(' --------- Very Coarse Grid ---------- '); print_N_cells(x,y,z,r,-2)
disp(' ----------- Coarse Grid ------------- '); print_N_cells(x,y,z,r,-1)
disp(' ---------- Moderate Grid ------------ '); print_N_cells(x,y,z,r,0)
disp(' ------------ Fine Grid -------------- '); print_N_cells(x,y,z,r,1)

