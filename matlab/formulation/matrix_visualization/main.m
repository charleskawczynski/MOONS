% Check if symmetric:
clc; clear all; close all;

props.test_symmetry = true;
props.compute_condition_number = false;
props.compute_preconditioned_condition_number = false;
props.debug = false;
props.compute_eig = false;
props.save_fig = true;
props.title_size = 20;
props.axis_size = 20;
props.label_size = 20;
props.filedir = 'figs\';
root = 'C:\Users\Charlie\Documents\MOONS\SIMS\MOONS1\out\LDC\matrix_visualization\';

props.filename = 'PCG_SF_Lap_CC'; props.title = '';
A = load([root props.filename '.dat' ]); analyzeMatrix(A',props) % A^T is exported, not A

props.filename = 'PCG_VF_Lap_CC'; props.title = '';
A = load([root props.filename '.dat' ]); analyzeMatrix(A',props) % A^T is exported, not A

props.filename = 'PCG_VF_Lap_Face'; props.title = '';
A = load([root props.filename '.dat' ]); analyzeMatrix(A',props) % A^T is exported, not A

props.filename = 'PCG_VF_curlcurl_CC'; props.title = '';
A = load([root props.filename '.dat' ]); analyzeMatrix(A',props) % A^T is exported, not A

props.filename = 'PCG_VF_curlcurl_Face'; props.title = '';
A = load([root props.filename '.dat' ]); analyzeMatrix(A',props) % A^T is exported, not A



% A = load([dir name]); A = A'; % A^T is exported, not A
% A_Lap = A;
% analyzeMatrix(A,name,props)
% A_simple = A*2/max(max(abs(A)));
% A_Lap_simple = A_simple;
