clc;close all;

%% Discretization
N_nodes = 1000;

%% Material properties (SI units)
save_profile_to_file = true;
a = .1;
rho = 9300.0;
mu_m = 1.25665465435e-06;
sigma = 700000.0;
mu = 0.001;
% mu = 10000; % Easier to visualize the profile with
nu = mu/rho;

%% Magnetic field configuration from Mike Ulrickson's data
micro_seconds_to_seconds = 10^6;
B0_z = 5;
dB0_x = 1;
dt = 4500/micro_seconds_to_seconds; % Plasma event time-period
dB0_x_dt_average = dB0_x/dt; % Based on time average
dB0_x_dt_max = -0.000252170601462*micro_seconds_to_seconds; % Based on dB0_x_dt_max

%% Time-varying magnetic field scale
dB0_x_dt = dB0_x_dt_average;
% dB0_x_dt = dB0_x_dt_max;

%% Solution construction
A = 1/(rho*nu*mu_m)*B0_z;
C = sigma*mu_m*dB0_x_dt;
D = sigma*mu_m*B0_z;
F = sqrt(A/D);
M = F*D;
u_coeff = a*C/D;
B_coeff = a*C/(F*D);
y = linspace(-a,a,N_nodes);
u_temp = sinh_a_over_sinh_b_safe(M*y,M*a);
B_temp = cosh_a_over_sinh_b_safe(M*y,M*a);
% u_temp = sinh(M*y)/sinh(M*a); % Numerical issues at high M
% B_temp = cosh(M*y)/sinh(M*a); % Numerical issues at high M
u = u_coeff*(y/a - u_temp);
B_x = B_coeff*(B_temp - 1/tanh(M*a));

disp(['dB0_x_dt = ' num2str(dB0_x_dt)])
disp(['u_coeff  = ' num2str(u_coeff)])
disp(['B_coeff  = ' num2str(B_coeff)])
disp(['M        = ' num2str(M)])
disp(['a        = ' num2str(a)])
disp(' ------------- DIMENSIONLESS PARAMETERS ------------- ')

%% Plot solutions
figure
subplot(2,1,1)
plot(y,u)
title('u vs y')
xlabel('y')
ylabel('u')
subplot(2,1,2)
plot(y,B_x)
title('B_x vs y')
xlabel('y')
ylabel('B_x')

%% Estimate dimensionless parameters
Re_m = max(u)*a*mu_m*sigma;
Re = max(u)*a/nu;
Ha = a*B0_z*sqrt(sigma/mu);
disp(['Re_m  = ' num2str(Re_m)])
disp(['Re  = ' num2str(Re)])
disp(['Ha  = ' num2str(Ha)])


%% Dimensionless solution plot
M_star = 100;
a = 1;
y = linspace(-a,a,N_nodes);
y_star = y/a;
u_coeff = 1;
B_coeff = 1;
u_temp = sinh_a_over_sinh_b_safe(M_star*y,M_star*a);
B_temp = cosh_a_over_sinh_b_safe(M_star*y,M_star*a);
u_star = u_coeff*(y/a - u_temp);
B_star = B_coeff*(B_temp - 1/tanh(M_star*a));

figure
subplot(2,1,1)
plot(y_star,u_star)
title('u vs y')
xlabel('y')
ylabel('u')
subplot(2,1,2)
plot(y_star,B_star)
title('B_x vs y')
xlabel('y')
ylabel('B_x')

if save_profile_to_file
    T = [y_star' u_star' B_star'];
    file = ['PD_solution_M=' num2str(M_star) '.dat'];
    save(file,'T','-ascii')
end

