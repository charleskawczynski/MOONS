% Check if symmetric:
% clc; clear all; close all;
myDir.MOONSdir = 'C:\Users\Charlie\Documents\GitHub\MOONS\';
addpath([myDir.MOONSdir 'matlab\tools'])
addpath([myDir.MOONSdir 'matlab\matrixAnalysis'])
myPlot = myPlotSettings();

% name = 'PCG_VF_U'; A = load([name '.dat']);
% A = A'; % A^T is exported, not A
% A_Lap = A;
% analyzeMatrix(A,name,true,false,true,false,false)
% A_simple = A*2/max(max(abs(A)));
% A_Lap_simple = A_simple;
% 
% name = 'diag(mom_diffusion)U'; A = load([name '.dat']);
% A = A'; % A^T is exported, not A
% A_diagLap = A;
% analyzeMatrix(A,name,true,false,true,false,false)
% A_simple = A*2/max(max(abs(A)));
% A_diagLap_simple = A_simple;
% 
% name = 'prec_lap_VFU'; A = load([name '.dat']);
% A = A'; % A^T is exported, not A
% A_prec = A;
% analyzeMatrix(A,name,true,false,true,false,false)
% A_simple = A*2/max(max(abs(A)));
% A_prec_simple = A_simple;
%
% figure
% plot(diag(A_curl),'b-o'); hold on
% plot(diag(A_prec),'r-*')
% legend('diagonal(A)','preconditioner')


name = 'PCG_VF_op_mat_B'; A = load([name '.dat']);
A = A'; % A^T is exported, not A
A_curl = A;
analyzeMatrix(A,name,true,false,true,false,false)
A_simple = A*2/max(max(abs(A)));
A_curl_simple = A_simple;

name = 'PCG_VF_op_mat_diag_B'; A = load([name '.dat']);
A = A'; % A^T is exported, not A
A_diagcurl = A;
analyzeMatrix(A,name,true,false,true,false,false)
A_diagcurl_simple = A*2/max(max(abs(A)));

name = 'PCG_VF_prec_mat_B'; A = load([name '.dat']);
A = A'; % A^T is exported, not A
A_prec = A;
analyzeMatrix(A,name,true,false,true,false,false)
A_simple = A*2/max(max(abs(A)));
A_prec_simple = A_simple;

figure
plot(diag(A_curl),'b-o'); hold on
plot(diag(A_prec),'r-*')
legend('diagonal(A)','preconditioner')
