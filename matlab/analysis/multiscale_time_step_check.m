clc; clear all; close all;

p = linspace(0,1);
dt = 0.01;
num = 1 - dt/2*(3*p.^2) + dt^2/2^2*(2*p.^2+2*p.^2+2*p.^2) + dt^3/2^3*p.^6;
denom = 1 + dt/2*(3*p.^2) + dt^2/2^2*(2*p.^2+2*p.^2+2*p.^2) + dt^3/2^3*p.^6;
G = num./denom

disp(['max = ' num2str(max(abs(G)))])
