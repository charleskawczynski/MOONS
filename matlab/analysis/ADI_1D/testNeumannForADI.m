%% ***************************************
% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;

%% PARAMS
n = 1000; % Number of cells
neumann = true; % (true,false) = (f=cose(), f = sin()) and with appropriate BCs
Nsweeps = 6; % Number of multi-scale sweeps
p = 2; % Fourier Mode, look at p = 1 for neumann: looks bad
nlevels = floor(log2(n));

%% GRID GENERATION
x = linspace(0,1,n+1); h = x(2) - x(1); alpha = 1;

%% PREP FORMATTED OUTPUT
st = [];
for i=1:nlevels; if (i~=nlevels);st = [st '%d\t\t,'];else;st = [st '%d']; end; end

%% PROBLEM SETUP
if (neumann); f = cos(p*pi*x)'; end
if (~neumann); f = sin(p*pi*x)'; end
% F = f;
u_solution = -(p*pi)^(-2)*f;
if (neumann); u_solution = u_solution - mean(u_solution); end
f(1) = u_solution(1); f(end) = u_solution(end);

%% MULTI-SCALE TIME STEP SELECTION
disp(['Number of multi-scale time levels = ' num2str(nlevels)])
dtj = zeros(1,nlevels);
for j=1:nlevels
    hj = 2^(j-1)*h; dtj(j) = 4*(hj^2)/(alpha*pi^2);
end
disp(sprintf(['dt(j)            = ' st],dtj))

CDm = zeros(1,nlevels); % Condition number of I-A
CDp = zeros(1,nlevels); % Condition number of I+A


%% MULTI-SCALE RELAXATION WITH THOMAS ALGORITHM (ELDREDGE)
un = zeros(size(f));
if (~neumann); un(1) = u_solution(1); un(end) = u_solution(end); end

for k=1:Nsweeps
    for j=1:nlevels
        unp1 = relax1D(un,f,alpha,dtj(j),n,h);
        if (neumann); unp1(1) = unp1(2); unp1(end) = unp1(end-1); end % O(h) boundary treatment
        un = unp1;
        if (neumann); unp1 = unp1 - mean(unp1); end
    end
    for j=nlevels:-1:1
        unp1 = relax1D(un,f,alpha,dtj(j),n,h);
        if (neumann); unp1(1) = unp1(2); unp1(end) = unp1(end-1); end % O(h) boundary treatment
        if (neumann); unp1 = unp1 - mean(unp1); end
        un = unp1;
    end
end
u_trisolve = un;


%% MATLAB BACKSLASH (\)
un = zeros(size(f));
if (~neumann); un(1) = u_solution(1); un(end) = u_solution(end); end
% f = F;
for k=1:Nsweeps
    for j=1:nlevels
        [loDiag, Diag, upDiag] = setUpSystem(.5*dtj(j)*alpha,h,n);
        Diag(2:end-1) = 1 + Diag(2:end-1);
        A_rhs = diag(Diag) + diag(upDiag,1) + diag(loDiag,-1);
        [loDiag, Diag, upDiag] = setUpSystem(-.5*dtj(j)*alpha,h,n);
        Diag(2:end-1) = 1 + Diag(2:end-1);
        A_lhs = diag(Diag) + diag(upDiag,1) + diag(loDiag,-1);
        unp1 = A_lhs\(A_rhs*un-dtj(j)*f);
%         disp(['Eig(I+A(dt(j))) = ' num2str(eig(A_rhs)')])
%         disp(['Eig(I-A(dt(j))) = ' num2str(eig(A_lhs)')])
        CDm(j) = cond(A_lhs);
        CDp(j) = cond(A_rhs);
        if (neumann); unp1(1) = unp1(2); unp1(end) = unp1(end-1); end % O(h) boundary treatment
        if (neumann); unp1 = unp1 - mean(unp1); end
        un = unp1;
    end
    for j=nlevels:-1:1
        [loDiag, Diag, upDiag] = setUpSystem(.5*dtj(j)*alpha,h,n);
        Diag(2:end-1) = 1 + Diag(2:end-1);
        A_rhs = diag(Diag) + diag(upDiag,1) + diag(loDiag,-1);
        [loDiag, Diag, upDiag] = setUpSystem(-.5*dtj(j)*alpha,h,n);
        Diag(2:end-1) = 1 + Diag(2:end-1);
        A_lhs = diag(Diag) + diag(upDiag,1) + diag(loDiag,-1);
        unp1 = A_lhs\(A_rhs*un-dtj(j)*f);
        if (neumann); unp1(1) = unp1(2); unp1(end) = unp1(end-1); end % O(h) boundary treatment
        if (neumann); unp1 = unp1 - mean(unp1); end
        un = unp1;
    end
end
u_matlab = un;

%% OUTPUT CONDITION NUMBERS
disp(sprintf(['cond(I-A(dt(j))) = ' st],CDm))
disp(sprintf(['cond(I-A(dt(j))) = ' st],CDp))
%% ERROR
err_trisolve = max(abs(u_trisolve - u_solution));
err_matlab = max(abs(u_matlab - u_solution));

disp(['max(abs(u_exact - u_trisolve)) = ' num2str(err_trisolve)])
disp(['max(abs(u_exact - u_matlab)) = ' num2str(err_matlab)])

%% VISUALIZATION
figure
subplot(2,1,1)
plot(x,u_matlab,'kx',x,u_trisolve,'b-',x,u_solution,'ro')
title('Solution to \nabla^2u = f')
xlabel('x')
ylabel('u(x)')
legend('u_{matlab}','u_{trisolve}','u_{solution}')
fact = 1.8;
axis([-0.1 1.1 min(u_solution)*(fact) max(u_solution)*fact])

subplot(2,1,2)
plot(x,f,'k-')
title('Forcing term in \nabla^2u = f')
xlabel('x')
ylabel('f(x)')
legend('f')
axis([-0.1 1.1 min(f)*(fact) max(f)*fact])

