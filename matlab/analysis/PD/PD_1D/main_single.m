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
nu = mu/rho;

%% Magnetic field configuration from Mike Ulrickson's data
micro_seconds_to_seconds = 10^6;
B0_z = 5;

%% Time-varying magnetic field scale
dB0_x = -1;
B0_x_max = 1;
dt = 4500/micro_seconds_to_seconds; % Plasma event time-period
dB0_x_dt_average = dB0_x/dt; % Based on time average
dB0_x_dt_max = -0.000252170601462*micro_seconds_to_seconds; % Based on dB0_x_dt_max
dB0_x_dt_linear_regime = -0.1;
dB0_x_dt = dB0_x_dt_average;
% dB0_x_dt = dB0_x_dt_max;
% dB0_x_dt = dB0_x_dt_linear_regime;

%% Characteristic parameters
L = a;
B_c = B0_z;
U_c = a*sqrt(B0_z*abs(dB0_x_dt)*sigma/rho);

%% Dimensionless parameters
Ha = L*B_c*sqrt(sigma/rho/nu);
Re = L*rho*U_c/mu;
Re_m = L*U_c*mu_m*sigma;

%% Solution construction
U_hat = -a*dB0_x_dt/B0_z;
B_hat = U_hat*sqrt(mu*sigma*mu_m^2.0);
J_c = U_hat*mu*sigma*a*dB0_x_dt/B0_z;
F_c = Ha^2/Re*rho*U_hat^2/a;

z = linspace(-a,a,N_nodes);
u_temp = sinh_a_over_sinh_b_safe(Ha*z/a,Ha);
B_temp = cosh_a_over_sinh_b_safe(Ha*z/a,Ha);
J_temp = sinh_a_over_sinh_b_safe(Ha*z/a,Ha);
u = U_hat*(u_temp - z/a);
B_x = B_hat*(1/tanh(Ha) - B_temp);
J_y = B_hat*(-Ha*J_temp);
F_x = -J_y.*B0_z;
F_z = -J_y.*B_x;

disp(' ------------- CHARACTERISTIC PARAMETERS ------------- ')
disp(['U_c      = ' num2str(U_c)])
disp(['B_c      = ' num2str(B_c)])
disp(['Ha       = ' num2str(Ha)])

disp(' ------------- SOLUTION PARAMETERS ------------- ')
disp(['a        = ' num2str(a)])
disp(['dB0_x_dt = ' num2str(dB0_x_dt)])
disp(['U_hat    = ' num2str(U_hat)])
disp(['max(u)   = ' num2str(max(abs(u)))])
disp(['max(B_x) = ' num2str(max(abs(B_x)))])
disp(['B_hat      = ' num2str(B_hat)])
disp(' ------------- DIMENSIONLESS PARAMETERS ------------- ')

%% Plot solutions
figure
subplot(2,4,1)
plot(z,u)
title('Dimensional velocity')
xlabel('z [m]')
ylabel('u [m/s]')
subplot(2,4,2)
plot(z,B_x)
title('Dimensional magnetic field')
xlabel('z [m]')
ylabel('B_x [T]')
subplot(2,4,3)
plot(z,F_x)
title('Dimensional Lorentz force (x-direction)')
xlabel('z [m]')
ylabel('F_x [N]')
subplot(2,4,4)
plot(z,F_z)
title('Dimensional Lorentz force (z-direction)')
xlabel('z [m]')
ylabel('F_z [N]')

%% Estimate dimensionless parameters
Re_m = max(u)*a*mu_m*sigma;
Re = max(u)*a/nu;
disp(['Re_m  = ' num2str(Re_m)])
disp(['Re  = ' num2str(Re)])
disp(['Ha  = ' num2str(Ha)])


%% Dimensionless solution plot
u_star = u/U_hat;
B_star = B_x/B_hat;
z_star = z/a;
F_x_star = F_x/F_c;
F_z_star = F_z/F_c;

subplot(2,4,5)
plot(z_star,u_star)
title('Dimensionless velocity')
xlabel('z/a')
ylabel('u/U_hat')
subplot(2,4,6)
plot(z_star,B_star)
title('Dimensionless magnetic field')
xlabel('z/a')
ylabel('B_x/B_hat')
subplot(2,4,7)
plot(z_star,F_x_star)
title('Dimensionless Lorentz Force (x-direction)')
xlabel('z/a')
ylabel('F_x/F_c')
subplot(2,4,8)
plot(z_star,F_z_star)
title('Dimensionless Lorentz Force (z-direction)')
xlabel('z/a')
ylabel('F_z/F_c')

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

