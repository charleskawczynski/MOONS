clc; close all;

B = A;
s=size(A);
for i=2:s(1)
    B(i,2) = B(i,2) - B(end,2);
end
B = B(1:end-1,:);

dt = B(:,1);
e = abs(B(:,2));
x_label = '1/dt';
y_label = 'L_2 error';
fig_title = 'Temporal Convergence Rates';

x = 1./dt;
% x = dt;
y = e;

plot_convergence_rates(x,y,x_label,y_label,fig_title)
