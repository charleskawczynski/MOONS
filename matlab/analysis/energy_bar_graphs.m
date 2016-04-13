% Energy bar graphs
clc; close all; clear all;

kinetic = true;
Re = 400;
% e = [0.000446342 0.0163517 0.0167981]; % Re = 400
e = [0.000248035 0.00542606 0.00567407]; % Re = 1000


e(1) = 0.000437615;
e(2) = 0.0126841;
e(3) = 0.0131217;

if kinetic
    s = {'advection';'diffusion';'Lorentz'};
%     s = {'advection';'pressure';'diffusion';'Lorentz'};
else
    s = {'advection';'diffusion'};
end
bar(e)
set(gca,'XTickLabel',s)
ylabel('Energy')

if kinetic
    title(['Kinetic Energy Budget for Re = ' int2str(Re)])
else
    title('Magnetic Energy Budget')
end
