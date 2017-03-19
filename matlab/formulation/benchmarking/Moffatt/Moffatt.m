% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;

%% MOFFATT
% Magnetic Field Generation in Electrically Conducting Fluids H.K. Moffatt
% Page 55, Section 3.8, Effect of plane differential rotation on an
% initially uniform field

% This notation was mixed with
% 1. Salah, N., Soulaimani, A. & Habashi, W. G. A finite element method for
% magnetohydrodynamics. Comput. methods Appl. Mech. 190, (2001).


r0 = 1;
Rem = 1000/(4*pi)
Rem = 10
B0 = 1;
r=linspace(0,2*r0,100);

theta=linspace(0,2*pi,200);
[r,theta]=meshgrid(r,theta);

k0 = sqrt(Rem)/r0;
p = (1-i)*k0/sqrt(2);
J0 = besselj(0,p*r0);
J1 = besselj(1,p*r0);
D = 2/(p*J0);
C = r0*(2*J1 - p*r0*J1)/(p*J0); C = real(C);
J1p  = besselj(1,p*r);
f = (r + C./r).*(r>r0) + (D*J1p).*(r<=r0);
Az = imag(B0*f.*exp(i*theta));
[By Bx] = gradient(Az);
Bmag = sqrt(Bx.^2 + By.^2);

%% VARIABLE TO PLOT
[X,Y,Az] = pol2cart(theta,r,Az);
[X,Y,Bx] = pol2cart(theta,r,Bx);
[X,Y,By] = pol2cart(theta,r,By);
[X,Y,Bmag] = pol2cart(theta,r,Bmag);

%% Edge of Cylinder
d = sqrt(r.^2 - r0^2); tol = r0/100;
radius = 100.*(d<tol) + 0.*(d>=tol);
[X,Y,R] = pol2cart(theta,r,radius);


%% Qualitative Plot
% contour(X,Y,R,L_edge,'LineColor', [0 0 0]); hold on
L_edge = linspace(min(min(R)),max(max(R)),5);

figure
L_var = linspace(min(min(Az)),max(max(Az)),50);
contourf(X,Y,real(Az),L_var)
xlabel('x'); ylabel('y'); title('A_z'); colorbar

figure
L_var = linspace(min(min(Bmag)),max(max(Bmag)),50);
contourf(X,Y,real(Bmag),L_var)
xlabel('x'); ylabel('y'); title('B_{mag}'); colorbar


T = false;
if T
    figure
    L_var = linspace(min(min(Az)),max(max(Az)),100);
    subplot(2,2,1); contour(X,Y,real(Az),L_var)
    xlabel('x'); ylabel('y'); title('A_z'); colorbar

    L_var = linspace(min(min(Bmag)),max(max(Bmag)),100);
    subplot(2,2,2); contourf(X,Y,real(Bmag),L_var)
    xlabel('x'); ylabel('y'); title('B_{mag}'); colorbar

    L_var = linspace(min(min(Bx)),max(max(Bx)),100);
    subplot(2,2,3); contourf(X,Y,real(Bx),L_var)
    xlabel('x'); ylabel('y'); title('B_x'); colorbar

    L_var = linspace(min(min(By)),max(max(By)),100);
    subplot(2,2,4); contourf(X,Y,real(By),L_var)
    xlabel('x'); ylabel('y'); title('B_y'); colorbar
elseif T
    figure
    L_var = linspace(min(min(Az)),max(max(Az)),100);
    subplot(2,1,1); contour(X,Y,real(Az),L_var)
    xlabel('x'); ylabel('y'); title('A_z'); colorbar

    L_var = linspace(min(min(Bmag)),max(max(Bmag)),100);
    subplot(2,1,2); contourf(X,Y,real(Bmag),L_var)
    xlabel('x'); ylabel('y'); title('B_{mag}'); colorbar

    figure
    L_var = linspace(min(min(Bx)),max(max(Bx)),100);
    subplot(2,1,1); contourf(X,Y,real(Bx),L_var)
    xlabel('x'); ylabel('y'); title('B_x'); colorbar

    L_var = linspace(min(min(By)),max(max(By)),100);
    subplot(2,1,2); contourf(X,Y,real(By),L_var)
    xlabel('x'); ylabel('y'); title('B_y'); colorbar
end

%% Quantitative Plot
% figure
% plot(X,Y,real(Z),lines)
% xlabel('x')
% ylabel('A_z')
% title('Potential along y at x = 0')
% 
% figure
% plot(X,Y,real(Z),lines)
% xlabel('x')
% ylabel('A_z')
% title('Potential along x at y = 0')


