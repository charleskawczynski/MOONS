clc;close all;

%% Discretization
N_nodes = 1000;
save_profile_to_file = false;
plot_B_dot = false;
plot_solution = false;
plot_surface = true;
plot_surface_separate = false;
save_surfs_to_file = true;
selected_cases = false;
save_surfs_to_file_separate = false;
single_case = false;

%% Material properties (SI units)
a = .1;
rho = 9300.0;
K_m = 10^-5;
mu_0 = 1.25663706*10^(-6);
mu_m = mu_0*(1+K_m);
sigma = 7*10^5;
mu = 0.001;
% mu = 10000; % Easier to visualize the profile with
nu = mu/rho;

%% Package material props
mat.rho = rho;
mat.K_m = K_m;
mat.mu_m = mu_m;
mat.sigma = sigma;
mat.mu = mu;
mat.nu = nu;

%% Magnetic field configuration from Mike Ulrickson's data
mili_seconds_to_seconds = 10^-3;
B0_x = 1;
B0_z = 1;
dB0_x = -1;
dt = 45*mili_seconds_to_seconds; % Plasma event time-period
T = dt;

M = 50;
N = 50;

if selected_cases
	dB0_x_dt_average = dB0_x/dt; % Based on time average
	dB0_x_dt_max = -0.000252170601462*10^6; % Based on dB0_x_dt_max
	%% Time-varying magnetic field scale
	dB0_x_dt = dB0_x_dt_average;
	% dB0_x_dt = dB0_x_dt_max;
	T_period = [5 20 50]*mili_seconds_to_seconds;
	B0_z = [1 5 10];
	[B0_z dT] = meshgrid(B0_z,T_period);
	dB0_x_dt = dB0_x./dT;
else
	B0_z = linspace(1,10,M);
	T_period = linspace(5,50,N)*mili_seconds_to_seconds;
	[B0_z dT] = meshgrid(B0_z,T_period);
	dB0_x_dt = dB0_x./dT;
end

M = length(B0_z);
N = length(dB0_x_dt);
zero_field = zeros(N,M);
u_max = zero_field;
B_max = zero_field;
B_mean = zero_field;
Re = zero_field;
Ha = zero_field;
Re_m = zero_field;
Pr_m = zero_field;
for i=1:M
for j=1:N
	% PD_1D_sol(mat,a,T,B0_x,dB0_x,dT,B0_z,save_profile_to_file,plot_solution,plot_B_dot)
	sol = PD_1D_sol(mat,a,T_period(j),B0_x,dB0_x,dT(j,i),B0_z(j,i),save_profile_to_file,plot_solution,plot_B_dot);
	u_max(j,i) = sol.u_max;
	B_max(j,i) = sol.B_max;
	B_mean(j,i) = sol.B_mean;
	Re(j,i) = sol.Re;
	Ha(j,i) = sol.Ha;
	Re_m(j,i) = sol.Re_m;
	Pr_m(j,i) = sol.Pr_m;
end
end

sol_all.dB0_x_dt = dB0_x_dt';
sol_all.B0_z = B0_z';
sol_all.u_max = u_max';
sol_all.B_max = B_max';
sol_all.B_mean = B_mean';
sol_all.Re = Re';
sol_all.Ha = Ha';
sol_all.Re_m = Re_m';
sol_all.Pr_m = Pr_m';

if plot_surface && ~single_case
	figure
	subplot(2,3,1)
	surf(B0_z,dB0_x_dt,u_max)
	xlabel('B^0_z')
	ylabel('dB^0_{x}/dt')
	title('u_{max}')
	subplot(2,3,2)
	surf(B0_z,dB0_x_dt,B_max)
	xlabel('B^0_z')
	ylabel('dB^0_{x}/dt')
	title('B_{max}')
	subplot(2,3,3)
	surf(B0_z,dB0_x_dt,B_mean)
	xlabel('B^0_z')
	ylabel('dB^0_{x}/dt')
	title('B_{mean}')

	subplot(2,3,4)
	surf(B0_z,dB0_x_dt,Re)
	xlabel('B^0_z')
	ylabel('dB^0_{x}/dt')
	title('Re')
	subplot(2,3,5)
	surf(B0_z,dB0_x_dt,Ha)
	xlabel('B^0_z')
	ylabel('dB^0_{x}/dt')
	title('Ha')
	subplot(2,3,6)
	surf(B0_z,dB0_x_dt,Re_m)
	xlabel('B^0_z')
	ylabel('dB^0_{x}/dt')
	title('Re_m')
end


if plot_surface_separate && ~single_case
	figure
	surf(B0_z,dB0_x_dt,u_max)
	xlabel('B^0_z')
	ylabel('dB^0_{x}/dt')
	title('u_{max}')
	figure
	surf(B0_z,dB0_x_dt,B_max)
	xlabel('B^0_z')
	ylabel('dB^0_{x}/dt')
	title('B_{max}')
	figure
	surf(B0_z,dB0_x_dt,B_mean)
	xlabel('B^0_z')
	ylabel('dB^0_{x}/dt')
	title('B_{mean}')

	figure
	surf(B0_z,dB0_x_dt,Re)
	xlabel('B^0_z')
	ylabel('dB^0_{x}/dt')
	title('Re')
	figure
	surf(B0_z,dB0_x_dt,Ha)
	xlabel('B^0_z')
	ylabel('dB^0_{x}/dt')
	title('Ha')
	figure
	surf(B0_z,dB0_x_dt,Re_m)
	xlabel('B^0_z')
	ylabel('dB^0_{x}/dt')
	title('Re_m')
end

B0_z = B0_z(:);
dB0_x_dt = dB0_x_dt(:);
u_max = u_max(:);
B_max = B_max(:);
Re = Re(:);
Ha = Ha(:);
Re_m = Re_m(:);
Pr_m = Pr_m(:);
% F = [B0_z(:) dB0_x_dt(:) u_max(:) B_max(:) Re(:) Ha(:) Re_m(:)];
F = [B0_z dB0_x_dt u_max B_max Re Ha Re_m Pr_m];
if save_surfs_to_file
    file = 'sol/PD_surf.dat';
	I = num2str(M);
	J = num2str(N);
	h1 = 'TITLE = "test"\n';
	h2 = 'VARIABLES = "B0_z", "dB0_x_dt", "umax", "Bmax", "Re", "Ha", "Rem", "Prm"\n';
	h3 = ['ZONE, T ="1", I = ' I ', J = ' J ' DATAPACKING = POINT\n'];
	fp = fopen(file,'w');
	fprintf(fp,h1);
	fprintf(fp,h2);
	fprintf(fp,h3);
	fclose(fp);
	dlmwrite(file, F, '-append', 'precision', '%.6f', 'delimiter', '\t');
end

h1 = 'TITLE = "test"\n';
h3 = ['ZONE, T ="1", I = ' I ', J = ' J ' DATAPACKING = POINT\n'];
I = num2str(M);
J = num2str(N);
if save_surfs_to_file_separate
	name = 'u_max'; var = [B0_z dB0_x_dt u_max];
    file = ['sol/' name '.dat']; h2 = ['VARIABLES = "B0_z", "dB0_x_dt", "' name '"\n'];
	fp = fopen(file,'w'); fprintf(fp,h1); fprintf(fp,h2); fprintf(fp,h3); fclose(fp);
	dlmwrite(file, var, '-append', 'precision', '%.6f', 'delimiter', '\t');
	name = 'B_max'; var = [B0_z dB0_x_dt B_max];
    file = ['sol/' name '.dat']; h2 = ['VARIABLES = "B0_z", "dB0_x_dt", "' name '"\n'];
	fp = fopen(file,'w'); fprintf(fp,h1); fprintf(fp,h2); fprintf(fp,h3); fclose(fp);
	dlmwrite(file, var, '-append', 'precision', '%.6f', 'delimiter', '\t');
	name = 'Re'; var = [B0_z dB0_x_dt Re];
    file = ['sol/' name '.dat']; h2 = ['VARIABLES = "B0_z", "dB0_x_dt", "' name '"\n'];
	fp = fopen(file,'w'); fprintf(fp,h1); fprintf(fp,h2); fprintf(fp,h3); fclose(fp);
	dlmwrite(file, var, '-append', 'precision', '%.6f', 'delimiter', '\t');
	name = 'Ha'; var = [B0_z dB0_x_dt Ha];;
    file = ['sol/' name '.dat']; h2 = ['VARIABLES = "B0_z", "dB0_x_dt", "' name '"\n'];
	fp = fopen(file,'w'); fprintf(fp,h1); fprintf(fp,h2); fprintf(fp,h3); fclose(fp);
	dlmwrite(file, var, '-append', 'precision', '%.6f', 'delimiter', '\t');
	name = 'Re_m'; var = [B0_z dB0_x_dt Re_m];;
    file = ['sol/' name '.dat']; h2 = ['VARIABLES = "B0_z", "dB0_x_dt", "' name '"\n'];
	fp = fopen(file,'w'); fprintf(fp,h1); fprintf(fp,h2); fprintf(fp,h3); fclose(fp);
	dlmwrite(file, var, '-append', 'precision', '%.6f', 'delimiter', '\t');
	name = 'Pr_m'; var = [B0_z dB0_x_dt Pr_m];;
    file = ['sol/' name '.dat']; h2 = ['VARIABLES = "B0_z", "dB0_x_dt", "' name '"\n'];
	fp = fopen(file,'w'); fprintf(fp,h1); fprintf(fp,h2); fprintf(fp,h3); fclose(fp);
	dlmwrite(file, var, '-append', 'precision', '%.6f', 'delimiter', '\t');
end

