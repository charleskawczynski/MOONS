clc; clear all; close all;
M = 59;
h = 2 / M;

si = [];
sj = [];
sv = [];

f = zeros((M + 1) * (M + 1), 1);

lbc = @(x, y) -x;
rbc = @(x, y)  x;

bbc = @(x, y)  0.5 * y;
tbc = @(x, y) -y;

for i = 2 : M
    for j = 2 : M
        idx = (i - 1) * (M + 1) + j;
        si = [si, idx, idx, idx, idx, idx];
        sj = [sj, idx, idx + 1, idx - 1, idx + (M + 1), idx - (M + 1)];
        sv = [sv, -4, 1, 1, 1, 1];
        f(idx) = 0;
    end
end

for i = 1 : M + 1
    j = 1;
    idx = (i - 1) * (M + 1) + j;
    si = [si, idx, idx];
    sj = [sj, idx, idx + 1];
    sv = [sv, 1, -1];
    f(idx) = h * bbc(h * (i - 1) - 1, h * (j - 1) - 1);
    
    j = M + 1;
    idx = (i - 1) * (M + 1) + j;
    si = [si, idx, idx];
    sj = [sj, idx, idx - 1];
    sv = [sv, 1, -1];
    f(idx) = h * tbc(h * (i - 1) - 1, h * (j - 1) - 1);
end

for j = 2 : M
    i = 1;
    idx = (i - 1) * (M + 1) + j;
    si = [si, idx, idx];
    sj = [sj, idx, idx + (M + 1)];
    sv = [sv, 1, -1];
    f(idx) = h * lbc(h * (i - 1) - 1, h * (j - 1) - 1);
    
    i = M + 1;
    idx = (i - 1) * (M + 1) + j;
    si = [si, idx, idx];
    sj = [sj, idx, idx - (M + 1)];
    sv = [sv, 1, -1];
    f(idx) = h * rbc(h * (i - 1) - 1, h * (j - 1) - 1);
end

A = sparse(si, sj, sv, (M+1) * (M + 1), (M+1) * (M + 1));

icase = 2;

if icase == 1
    %% replacing equation

    ifix = round(M/2);
    jfix = round(M/2);
    fix = (ifix - 1) * (M + 1) + jfix;

    A(fix, fix + (M+1)) = 0;
    A(fix, fix - (M+1)) = 0;
    A(fix, fix + 1) = 0;
    A(fix, fix - 1) = 0;

    u = A \ f;
else
%% QR

[Qtf, QtA, e] = qr(A, f, 'vector');

Rs = QtA(1:end-1, 1:end-1);
zs = Qtf(1:end-1);

fprintf('Residual (alpha) = %e\n', Qtf(end) / norm(Qtf, 'inf'));

uperm = Rs \ zs;
uperm = [uperm; 0];
u = zeros(size(A, 1), 1);
u(e) = uperm;
end

u = reshape(u, [M+1, M+1]);

surf(u)