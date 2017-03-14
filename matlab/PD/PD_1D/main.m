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
Ha = a*B0_z*sqrt(sigma/rho/nu);
U_c = a*dB0_x_dt/B0_z;
B_c = U_c*sqrt(mu*sigma*mu_m^2.0);
y = linspace(-a,a,N_nodes);
u_temp = sinh_a_over_sinh_b_safe(Ha*y/a,Ha);
B_temp = cosh_a_over_sinh_b_safe(Ha*y/a,Ha);
u = U_c*(y/a - u_temp);
B_x = B_c*(B_temp - 1/tanh(Ha));

disp(['dB0_x_dt = ' num2str(dB0_x_dt)])
disp(['U_c      = ' num2str(U_c)])
disp(['B_c      = ' num2str(B_c)])
disp(['Ha       = ' num2str(Ha)])
disp(['a        = ' num2str(a)])
disp(' ------------- DIMENSIONLESS PARAMETERS ------------- ')

%% Plot solutions
figure
subplot(2,2,1)
plot(y,u)
title('Dimensional velocity')
xlabel('y')
ylabel('u')
subplot(2,2,2)
plot(y,B_x)
title('Dimensional magnetic field')
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
u_star = u/U_c;
B_star = B_x/B_c;

subplot(2,2,3)
plot(y_star,u_star)
title('Dimensionless velocity')
xlabel('y')
ylabel('u')
subplot(2,2,4)
plot(y_star,B_star)
title('Dimensionless magnetic field')
xlabel('y')
ylabel('B_x')

if save_profile_to_file
    T = [y_star' u_star' B_star'];
    file = ['PD_solution_M=' num2str(M_star) '.dat'];
    save(file,'T','-ascii')
end

