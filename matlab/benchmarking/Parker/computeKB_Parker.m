function KB = computeKB_Parker(omegaT,TF)
% Clear workspace and prescribe path
% clear;clc;close all;
% p = mfilename('fullpath');
% [thisDir,name,ext] = fileparts(p);
% chdir(thisDir); myDir.this = thisDir;

%% PARKER
% 1. Parker, R. Reconnexion of Lines of Force in Rotating Spheres and
% Cylinders. (1966).
TFp = TF(1);
TFmag = TF(2);
TFr = TF(3);
TFphi = TF(4);

R = 1; b = R;
Rem = 100; H0 = 1;
r = linspace(0,R,200+1); dr = r(2)-r(1);
phi = linspace(0,2*pi,250+1); dphi = phi(2)-phi(1);
Nr = length(r); Nphi = length(phi);

K = 100; % Size of summation series
j = besselzero(0,K,1); % Get zeros of bessel function

S = zeros(1,length(r));
coeff = j*0;
for n = 1:K
    J1r = bessel(1,j(n)*r/b);
    J1 = bessel(1,j(n));
    et = exp(-i*omegaT - j(n)^2*omegaT/Rem);
    coeff(n) = 4*H0*b/(j(n)^2*(i+j(n)^2/Rem)*J1);
    S = S + (coeff(n)*J1r'*et)';
end
J1r = besselj(1,i^(3/2)*Rem^(1/2)*r/b);
J0  = besselj(0,i^(3/2)*Rem^(1/2));
V = -2*H0*b*i^(-1/2)/Rem^(1/2)*J1r/J0 + S;

Hr = i*(V./r)'*exp(i*phi);
dVdr = derivative(V,1,2);
Hphi = -dVdr'*exp(i*phi);
Hmag = sqrt(Hr.^2+Hphi.^2);

potential = V'*exp(i*phi);

potential = potential';
Hr = Hr';
Hphi = Hphi';
Hmag = Hmag';

[r,phi] = meshgrid(r,phi);
[X,Y,Hr] = pol2cart(phi,r,Hr);
[X,Y,Hphi] = pol2cart(phi,r,Hphi);
[X,Y,potential] = pol2cart(phi,r,potential);
[X,Y,Hmag] = pol2cart(phi,r,Hmag);

KB = (Hmag(:,2:end).^2).*r(:,2:end);
KB = KB*dr*dphi;
KB = sum(sum(KB));

if TFp
    figure
    contourf(X,Y,real(potential))
    xlabel('x'); ylabel('y'); title('potential'); colorbar
end
if TFmag
    figure
    contourf(X,Y,real(Hmag))
    xlabel('x'); ylabel('y'); title('H_{mag}'); colorbar
end

if TFr
    figure
    contourf(X,Y,real(Hr))
    xlabel('x'); ylabel('y'); title('H_r'); colorbar
end
if TFphi
    figure
    contourf(X,Y,real(Hphi))
    xlabel('x'); ylabel('y'); title('H_{\phi}'); colorbar
end

% [V I] = max(Y(:));
% [I_row, I_col] = ind2sub(size(Y),I); %#ok<NASGU>
% y1 = Y(I_row,:); hmag1 = real(Hmag(I_row,:));
% 
% [V I] = min(Y(:));
% [I_row, I_col] = ind2sub(size(Y),I); %#ok<NASGU>
% y2 = Y(I_row,:); hmag2 = real(Hmag(I_row,:));
% 
% figure
% plot(y1,hmag1,y2,hmag2)
% xlabel('y'); ylabel('H_{mag}'); title('H_{mag}'); colorbar


end