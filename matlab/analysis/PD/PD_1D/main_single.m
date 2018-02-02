clc;close all;

%% Discretization
N_nodes = 1000;

%% Material properties (SI units)
save_profile_to_file = true;
a = .1;
rho = 9300.0;
K_m = 10^-5;
mu_0 = 1.25663706*10^(-6);
mu_m = mu_0*(1+K_m);
sigma = 7*10^5;
mu = 0.001;
% mu = 10000; % Easier to visualize the profile with
nu = mu/rho;

%% Magnetic field configuration from Mike Ulrickson's data
micro_seconds_to_seconds = 10^6;
B0_z = 5;
dB0_x = -1;
dt = 4500/micro_seconds_to_seconds; % Plasma event time-period
dB0_x_dt_average = dB0_x/dt; % Based on time average
dB0_x_dt_max = -0.000252170601462*micro_seconds_to_seconds; % Based on dB0_x_dt_max

%% Time-varying magnetic field scale
% dB0_x_dt = dB0_x_dt_average;
dB0_x_dt = dB0_x_dt_max;

%% Solution construction
Ha = a*B0_z*sqrt(sigma/rho/nu);
Ha = .1; % For plots

U_c = -a*dB0_x_dt/B0_z;
B_c = U_c*sqrt(mu*sigma*mu_m^2.0);

z = linspace(-a,a,N_nodes);
u_temp = sinh_a_over_sinh_b_safe(Ha*z/a,Ha);
B_temp = cosh_a_over_sinh_b_safe(Ha*z/a,Ha);
u = U_c*(u_temp - z/a);
B_x = B_c*(1/tanh(Ha) - B_temp);

disp(['dB0_x_dt = ' num2str(dB0_x_dt)])
disp(['U_c      = ' num2str(U_c)])
disp(['max(u)   = ' num2str(max(abs(u)))])
disp(['max(B_x) = ' num2str(max(abs(B_x)))])
disp(['B_c      = ' num2str(B_c)])
disp(['Ha       = ' num2str(Ha)])
disp(['a        = ' num2str(a)])
disp(' ------------- DIMENSIONLESS PARAMETERS ------------- ')

%% Plot solutions
figure
subplot(2,2,1)
plot(z,u)
title('Dimensional velocity')
xlabel('z [m]')
ylabel('u [m/s]')
subplot(2,2,2)
plot(z,B_x)
title('Dimensional magnetic field')
xlabel('z [m]')
ylabel('B_x [T]')

%% Estimate dimensionless parameters
Re_m = max(u)*a*mu_m*sigma;
Re = max(u)*a/nu;
disp(['Re_m  = ' num2str(Re_m)])
disp(['Re  = ' num2str(Re)])
disp(['Ha  = ' num2str(Ha)])


%% Dimensionless solution plot
u_star = u/U_c;
B_star = B_x/B_c;
z_star = z/a;

subplot(2,2,3)
plot(z_star,u_star)
title('Dimensionless velocity')
xlabel('z/a')
ylabel('u/U_c')
subplot(2,2,4)
plot(z_star,B_star)
title('Dimensionless magnetic field')
xlabel('z/a')
ylabel('B_x/B_c')

if save_profile_to_file
    file = ['sol/u_PD_solution_Ha=' num2str(Ha) '.dat'];
    T = [z_star' u_star'];
	fmt = '%2.6f';
	header = {};
	header{1} = 'TITLE = "test"\n';
	header{2} = ['VARIABLES = "z", "Ha=' num2str(Ha) '"\n'];
	header{3} = ['ZONE, T ="1", I = ' num2str(length(u_star)) ', DATAPACKING = POINT\n'];
	save_data_for_tec(file,T,header,fmt);

    file = ['sol/B_PD_solution_Ha=' num2str(Ha) '.dat'];
    T = [z_star' B_star'];
	fmt = '%2.6f';
	header = {};
	header{1} = 'TITLE = "test"\n';
	header{2} = ['VARIABLES = "z", "Ha=' num2str(Ha) '"\n'];
	header{3} = ['ZONE, T ="1", I = ' num2str(length(u_star)) ', DATAPACKING = POINT\n'];
	save_data_for_tec(file,T,header,fmt);
end

