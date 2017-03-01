clc;close all;

N = 10;
Re = 100;
Rem = 100;
Ha = 10;
dB0_z_dt = 0;
dB0_x = 1;

dp_dz = -1;
k = -Re*dp_dz;
A = Ha*dB0_x;
D = k+Ha*dB0_z_dt;
C = k-Ha*dB0_z_dt;
a = 1;
b = 1;
m.x = coordinates(a,40); Nx = m.x.sn;
m.y = coordinates(b,40); Ny = m.y.sn;

x = m.x.hn;
y = m.y.hn;
S_X = zeros(Nx,Ny);
S_Y = zeros(Nx,Ny);

for n=0:N
	lambda = (pi/b*(n+1/2))^2;
	sqrt_lambda = sqrt(lambda);
	gamma_1 = -sqrt(A^2 + 4*lambda)/2;
	gamma_2 = +sqrt(A^2 + 4*lambda)/2;
	% I_n = -16*b^3*(cos(n*pi)^n)/(pi^3*(2*n+1)^3);
	I_n = 2*b*cos(sqrt_lambda*b)/lambda - 2*sin(sqrt_lambda*b)/lambda^(3/2);
	E_1 = exp(-gamma_1*a);
	E_2 = exp(-gamma_2*a);
	F_1 = exp(+gamma_1*a);
	F_2 = exp(+gamma_2*a);
	theta = F_1*E_2 - F_2*E_1;
	if n==0
		coeff = .5;
		coeff = 1;
		I_n = -2/3*b^3;
	else
		coeff = 1;
	end
	alpha_1 = (C*1/b*I_n)/theta * (E_2 - F_2);
	alpha_2 = (C*1/b*I_n)/theta * (F_1 - E_1);
	beta_1  = (D*1/b*I_n)/theta * (E_2 - F_2);
	beta_2  = (D*1/b*I_n)/theta * (F_1 - E_1);
for i=1:Nx
for j=1:Ny
	S_X(i,j) = S_X(i,j) + coeff*(alpha_1*exp(gamma_1*x(i)) + alpha_1*exp(gamma_2*x(i)))*cos(sqrt_lambda*y(j));
	S_Y(i,j) = S_Y(i,j) + coeff*(beta_1 *exp(gamma_1*x(i)) + beta_1 *exp(gamma_2*x(i)))*cos(sqrt_lambda*y(j));
end
end
end
X = S_X;
Y = S_Y;

for j=1:Ny
	X(:,j) = X(:,j) + C*.5*(b^2 - y(j)^2);
	Y(:,j) = Y(:,j) + D*.5*(b^2 - y(j)^2);
end

X = X';
Y = Y';

u_z = .5*(X+Y);
B_z = Rem/Ha*.5*(X-Y);

u_z_f = init_Node(m);
B_z_f = init_Node(m);
X_f = init_Node(m);
Y_f = init_Node(m);

u_z_f.vals = u_z;
B_z_f.vals = B_z;
X_f.vals = X;
Y_f.vals = Y;

plot_field(m,[2 1 1],X_f,'X','X,Y vs x,y')
plot_field(m,[2 1 2],Y_f,'Y','X,Y vs x,y')

plot_field(m,[2 1 1],u_z_f,'u_z','solution vs x,y')
plot_field(m,[2 1 2],B_z_f,'B_z','solution vs x,y')
