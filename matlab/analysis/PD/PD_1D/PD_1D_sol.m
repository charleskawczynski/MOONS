function sol = PD_1D_sol(mat,a,T,B0_x,dB0_x,dT,B0_z,save_profile_to_file,plot_solution,plot_B_dot)

%% Discretization
N_nodes = 1000;

%% Material properties (SI units)
mu_0 = 1.25663706*10^(-6);
rho = mat.rho;
K_m = mat.K_m;
mu_m = mat.mu_m;
sigma = mat.sigma;
mu = mat.mu;
nu = mat.nu;
dB0_x_dt = dB0_x/dT;

%% Solution construction
Ha = a*B0_z*sqrt(sigma/rho/nu);
U_c = a*dB0_x_dt/B0_z;
B_c = U_c*sqrt(mu*sigma*mu_m^2.0);
y = linspace(-a,a,N_nodes);
u_temp = sinh_a_over_sinh_b_safe(Ha*y/a,Ha);
B_temp = cosh_a_over_sinh_b_safe(Ha*y/a,Ha);
u = U_c*(y/a - u_temp);
B_x = B_c*(B_temp - 1/tanh(Ha));

% disp(['rho   = ' num2str(rho)])
% disp(['K_m   = ' num2str(K_m)])
% disp(['mu_m  = ' num2str(mu_m)])
% disp(['sigma = ' num2str(sigma)])
% disp(['mu    = ' num2str(mu)])
% disp(['nu    = ' num2str(nu)])

% disp(['dB0_x_dt    = ' num2str(dB0_x_dt)])
% disp(['U_c      = ' num2str(U_c)])
% disp(['max(u)   = ' num2str(max(abs(u)))])
% disp(['max(B_x) = ' num2str(max(abs(B_x)))])
% disp(['B_c      = ' num2str(B_c)])
% disp(['Ha       = ' num2str(Ha)])
% disp(['a        = ' num2str(a)])
% disp(' ------------- DIMENSIONLESS PARAMETERS ------------- ')

%% Plot solutions
if plot_solution
	figure
	subplot(2,2,1)
	plot(y,u)
	title('Dimensional velocity')
	xlabel('y [m]')
	ylabel('u [m/s]')
	subplot(2,2,2)
	plot(y,B_x)
	title('Dimensional magnetic field')
	xlabel('y [m]')
	ylabel('B_x [T]')
end

u_max = max(abs(u));
B_max = max(abs(B_x));
B_mean = mean(B_x);

%% Estimate dimensionless parameters
Re_m = u_max*a*mu_m*sigma;
Re = u_max*a/nu;
Ha = a*B0_z*sqrt(sigma/mu);
Pr_m = Re_m/Re;
% disp(['Re_m  = ' num2str(Re_m)])
% disp(['Re  = ' num2str(Re)])
% disp(['Ha  = ' num2str(Ha)])


%% Dimensionless solution plot
u_star = u/U_c;
B_star = B_x/B_c;
y_star = y/a;

if plot_solution
	subplot(2,2,3)
	plot(y_star,u_star)
	title('Dimensionless velocity')
	xlabel('y/a')
	ylabel('u/U_c')
	subplot(2,2,4)
	plot(y_star,B_star)
	title('Dimensionless magnetic field')
	xlabel('y/a')
	ylabel('B_x/B_c')
end

if plot_B_dot
	figure
	t = linspace(0,T);
	temp = ones(size(t));
	B1_x_vs_t = mean(B_x)*temp;
	B1_x_vs_t = mean(B_x)*t;
	B0_x_vs_t = B0_x+t*dB0_x_dt;
	B_x_vs_t = B0_x_vs_t+B1_x_vs_t;
	plot(t,B1_x_vs_t,'r-',t,B0_x_vs_t,'b-',t,B_x_vs_t,'k-')
	title('B vs time')
	xlabel('Time [s]')
	ylabel('Magnetic field [T]')
	legend('B^1_x vs t','B^0_x vs t','B_x vs t')
end

if save_profile_to_file
    T = [y_star' u_star' B_star'];
    file = ['sol/PD_solution_Ha=' num2str(Ha) '.dat'];
    save(file,'T','-ascii')
end

sol.U_c = U_c;
sol.u = u;
sol.u_max = u_max;
sol.B_max = B_max;
sol.B_mean = B_mean;
sol.B = B_x;
sol.y = y;
sol.a = a;
sol.B_c = B_c;
sol.Ha = Ha;
sol.Re = Re;
sol.Re_m = Re_m;
sol.Pr_m = Pr_m;

end