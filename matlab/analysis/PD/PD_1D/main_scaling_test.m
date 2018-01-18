clc;close all;

%% Discretization
N_nodes = 1000;

Ha = linspace(0.01,100,1000);
Re_m = 1;
N = length(Ha);
m_u_val = zeros(1,N);
m_B_val = zeros(1,N);
for i=1:N
	z = linspace(-1,1,N_nodes);
	u_temp = sinh_a_over_sinh_b_safe(Ha(i)*z,Ha(i));
	B_temp = cosh_a_over_sinh_b_safe(Ha(i)*z,Ha(i));
	u_temp = (u_temp - z);
	B_temp = Re_m/Ha(i)*(1/tanh(Ha(i)) - B_temp);
	m_u_val(i) = max(abs(u_temp));
	m_B_val(i) = max(abs(B_temp));
end

%% Plot solutions
figure
plot(Ha,m_u_val,Ha,m_B_val)
title('max of distribution vs Ha')
xlabel('Ha []')
ylabel('max of distribution []')
legend('u','B')
