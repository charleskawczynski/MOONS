%% myDensity tester
close all; clc; clear all;

Nx = 201;
h = linspace(0,25,Nx);
sigma = 0.2;
h0 = 25/1.5;
gaussSample(20,Nx,sigma,h0,h,true);

