clc;close all;

rho = 1;
nu = 1;
mu_m = 1;
sigma = 1;
dt = 0.0045;
B0_z = 5;
dB0_x = -1;
dB0_x_dt = dB0_x/dt;
dB0_x_dt_max = 
dB0_x_dt = dB0_x_dt_max;
a = 1;

A = 1/(rho*nu*mu_m)*B0_z;
C = sigma*mu_m*dB0_x_dt;
D = sigma*mu_m*B0_z;
F = sqrt(A/D);
M = F*D;

m.y = coordinates(a,1000);

y = m.y.hn;

u = a*C/D*(y/a - sinh(M*y)/sinh(M*a));
B_x = a*C/M*(cosh(M*y)/sinh(M*a) - 1/tanh(M*a));

figure
subplot(2,1,1)
plot(y(2:end-1),u(2:end-1))
title('u vs y')
xlabel('y')
ylabel('u')

subplot(2,1,2)
plot(y(2:end-1),B_x(2:end-1))
title('B_x vs y')
xlabel('y')
ylabel('B_x')
