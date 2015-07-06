% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;

%% PARKER
% 1. Parker, R. Reconnexion of Lines of Force in Rotating Spheres and
% Cylinders. (1966).

transient = false;

%% TRANSIENT PLOTS (~minutes)
if transient
    t = linspace(0.00001,25.8,60)
    KB = zeros(1,length(t));
    TF = [false false false false];
    for i=1:length(t)
        TF(2) = mod(i,4)==0;
%         KB(i) = computeKB_Parker(t(i),TF,false);
        KB(i) = computeKB_Parker(t(i),TF);
    end
    figure; plot(t,KB);title('KB vs t')
    xlabel('t'); ylabel('KB')
else
    %% SS PLOTS (~seconds)
    R = 1; b = R;
    Rem = 100; H0 = 1;
    % r = linspace(0,R,4*25+S);
    % phi = linspace(0,2*pi,4*25+S);
    r = linspace(0,R,100+1);
    phi = linspace(0,2*pi,100+1);
    % omegaT = 8.88;
    omegaT = 1000;
%     omegaT = 0.0000001;

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
    % coeff
    rstar = ones(size(r));
    J1r = besselj(1,i^(3/2)*Rem^(1/2)*r/b);
    J0  = besselj(0,i^(3/2)*Rem^(1/2));
    V = -2*H0*b*i^(-1/2)/Rem^(1/2)*J1r/J0 + S;

    Hr = i*(V./r)'*exp(i*phi);
    dVdr = derivative(V,1,2);
    Hphi = -dVdr'*exp(i*phi);
    Hmag = sqrt((Hr).^2+(Hphi).^2);

    potential = V'*exp(i*phi);

    % phistar = ones(1,length(phi));
    % Hx = Hr*(cos(phi)'*phistar) + Hphi*(sin(phi)'*phistar);
    % Hy = -Hr*sin(phi)'*phistar + Hphi*(cos(phi)'*phistar);

    Hr = Hr';
    Hphi = Hphi';
    potential = potential';
    Hmag = Hmag';

    [r,phi] = meshgrid(r,phi);
    [X,Y,Hr] = pol2cart(phi,r,Hr);
    [X,Y,Hphi] = pol2cart(phi,r,Hphi);
    [X,Y,potential] = pol2cart(phi,r,potential);

    [X,Y,Hmag] = pol2cart(phi,r,Hmag);

    figure
    contourf(X,Y,real(Hr))
    xlabel('x'); ylabel('y'); title('H_r'); colorbar
    
    figure
    contourf(X,Y,real(Hphi))
    xlabel('x'); ylabel('y'); title('H_{\phi}'); colorbar
    
    figure
    contourf(X,Y,real(potential))
    xlabel('x'); ylabel('y'); title('potential'); colorbar


    figure
    contourf(X,Y,real(Hmag))
    xlabel('x'); ylabel('y'); title('H_{mag}'); colorbar

    [V I] = max(Y(:));
    [I_row, I_col] = ind2sub(size(Y),I); %#ok<NASGU>
    y1 = Y(I_row,:); hmag1 = real(Hmag(I_row,:));
    [V I] = min(Y(:));
    [I_row, I_col] = ind2sub(size(Y),I);
    y2 = Y(I_row,:); hmag2 = real(Hmag(I_row,:));
    figure; plot(y1,hmag1,y2,hmag2)
    xlabel('y'); ylabel('H_{mag}'); title('H_{mag} at x = 0'); colorbar

    [V I] = max(X(:));
    [I_row, I_col] = ind2sub(size(X),I); %#ok<NASGU>
    y1 = X(I_row,:); hmag1 = real(Hmag(I_row,:));
    [V I] = min(X(:));
    [I_row, I_col] = ind2sub(size(X),I);
    y2 = X(I_row,:); hmag2 = real(Hmag(I_row,:));
    figure; plot(y1,hmag1,y2,hmag2)
    xlabel('x'); ylabel('H_{mag}'); title('H_{mag} at y = 0'); colorbar
end