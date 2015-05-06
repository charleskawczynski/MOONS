R_ADI = load('R_ADI.dat');
R_SOR = load('R_SOR.dat');
R_MG = load('R_MG.dat');

figure
subplot(2,1,1)
plot(r)
subplot(2,1,2)
loglog(r)

