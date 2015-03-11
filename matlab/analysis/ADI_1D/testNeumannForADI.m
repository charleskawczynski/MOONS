%% ***************************************
% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;

%% PARAMS
n = 100; % Number of cells
neumann = true; % (true,false) = (f=cose(), f = sin()) and with appropriate BCs
Nsweeps = 40; % Number of multi-scale sweeps
p = 3; % Fourier Mode, look at p = 1 for neumann: looks bad
nlevels = floor(log2(n));
tol = 1e-6; % stopping tolerance
dt = 0.1;
multiScale = true;

%% GRID GENERATION
x = linspace(0,1,n+1); h = x(2) - x(1); alpha = 2;

%% PREP FORMATTED OUTPUT
st = [];
for i=1:nlevels; if (i~=nlevels);st = [st '%d\t\t,'];else;st = [st '%d']; end; end

%% PROBLEM SETUP
u_solution = cos(p*pi*x)';
if (neumann); u_solution = u_solution - mean(u_solution); end

f = lap(u_solution,alpha,h);
f(1) = alpha*(-u_solution(1)+u_solution(2))/h^2;
f(end) = alpha*(-u_solution(end)+u_solution(end-1))/h^2;

f = f - mean(f);

%% MULTI-SCALE TIME STEP SELECTION
if multiScale; disp(['Number of sweeps = ' num2str(Nsweeps)]); end
if ~multiScale; disp(['Number of iterations = ' num2str(Nsweeps)]); end
if multiScale; disp(['Number of multi-scale time levels = ' num2str(nlevels)]);end

dtj = zeros(1,nlevels);
for j=1:nlevels
    hj = 2^(j-1)*h; dtj(j) = 4*(hj^2)/(alpha*pi^2);
end
if multiScale
disp(sprintf(['dt(j)            = ' st],dtj))
end


%% MULTI-SCALE RELAXATION WITH THOMAS ALGORITHM (ELDREDGE)
un = zeros(size(f));
if (~neumann); un(1) = u_solution(1); un(end) = u_solution(end); end

if multiScale
    for k=1:Nsweeps
        for j=1:nlevels
            if (neumann); unp1 = relax1DNeumann(un,f,alpha,dtj(j),n,h); end
            if (~neumann); unp1 = relax1D(un,f,alpha,dtj(j),n,h); end
            diffNorm = max(abs(un - unp1));
            un = unp1;
        end
        for j=nlevels:-1:1
            if (neumann); unp1 = relax1DNeumann(un,f,alpha,dtj(j),n,h); end
            if (~neumann); unp1 = relax1D(un,f,alpha,dtj(j),n,h); end
            diffNorm = max(abs(un - unp1));
            un = unp1;
        end
    end
else
    for k=1:Nsweeps
        if (neumann); unp1 = relax1DNeumann(un,f,alpha,dt,n,h); end
        if (~neumann); unp1 = relax1D(un,f,alpha,dtj(j),n,h); end
        diffNorm = max(abs(un - unp1));
        un = unp1;
    end
end
if (neumann); un = un - mean(un); end
% if (~neumann); un = un - mean(un); end
u_trisolve = un;

%% ERROR
err_trisolve = max(abs(u_trisolve - u_solution));

disp(['Difference between iterates = ' num2str(diffNorm)])
disp(['Error w.r.t. exact discrete solution = ' num2str(err_trisolve)])

%% VISUALIZATION
figure
subplot(2,1,1)
plot(x,u_trisolve,'b-',x,u_solution,'r-')
title('Solution to \nabla^2u = f')
xlabel('x')
ylabel('u(x)')
legend('u_{trisolve}','u_{solution}')
fact = 1.8;
% axis([-0.1 1.1 min(u_solution)*(fact) max(u_solution)*fact])

subplot(2,1,2)
plot(x,f,'k-')
title('Forcing term in \nabla^2u = f')
xlabel('x')
ylabel('f(x)')
legend('f')
% axis([-0.1 1.1 min(f)*(fact) max(f)*fact])

% u_trisolve
