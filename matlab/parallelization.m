% Nthreads
clc; clear all; close all;

Nthreads = [6 10 12 24 48];
iterpersec = [13 13 20 14 7];

plot(Nthreads,iterpersec)
title('# of threads vs iterations per second')
xlabel('Number of threads')
ylabel('iterations per second')

