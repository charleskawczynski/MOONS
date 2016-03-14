clc; clear all; close all;
norms_Jac = load('n_PSE.dat');
n_PSE = 1:size(norms_Jac(:,1));
norms_Jac = load('norms_Jacobi.dat');
n_Jac = 1:size(norms_Jac(:,1));
norms_SOR = load('norms_SOR.dat');
n_SOR = 1:size(norms_SOR(:,1));
norms_ADI = load('norms_ADI.dat');
n_ADI = 1:size(norms_ADI(:,1));
norms_MG = load('norms_MG.dat');
n_MG = 1:size(norms_MG(:,1));
n_MG = n_MG*5*3;

individual = false;
saveToFile = false;
if saveToFile
    T = [n_SOR' norms_Jac(:,3)];
    save('norms_Jac_tecplot.dat' ,'T','-ascii')
    T = [n_SOR' norms_SOR(:,3)];
    save('norms_SOR_tecplot.dat' ,'T','-ascii')
    T = [n_ADI' norms_ADI(:,3)];
    save('norms_ADI_tecplot.dat' ,'T','-ascii')
    T = [n_MG' norms_MG(:,3)];
    save('norms_MG_tecplot.dat' ,'T','-ascii')
end


if individual
    figure
    semilogy(n_Jac,norms_Jac)
    title('Convergence rate of Jacobi')
    xlabel('Iteration')
    ylabel('norm')
    legend('L_1','L_2','L_{\infty}')

    figure
    semilogy(n_SOR,norms_SOR)
    title('Convergence rate of SOR')
    xlabel('Iteration')
    ylabel('norm')
    legend('L_1','L_2','L_{\infty}')

    figure
    semilogy(n_ADI,norms_ADI)
    title('Convergence rate of ADI')
    xlabel('Iteration')
    ylabel('norm')
    legend('L_1','L_2','L_{\infty}')

    figure
    semilogy(n_MG,norms_MG)
    title('Convergence rate of MG')
    xlabel('Iteration')
    ylabel('norm')
    legend('L_1','L_2','L_{\infty}')

else
    figure
    for i=1:3
%         subplot(2,2,i)
        semilogy(n_Jac,norms_Jac(:,i),'m-',n_SOR,norms_SOR(:,i),'r-',n_ADI,norms_ADI(:,i),'b-',n_MG,norms_MG(:,i),'g-')
        title('Convergence Comparison')
        xlabel('Iteration')
        ylabel('norm')
        if i==3
            legend('L_{\infty,Jacobi}','L_{\infty,SOR}','L_{\infty,ADI}','L_{\infty,MG}')
        else
            legend(['L_{' int2str(i) ',Jacobi}'],['L_{' int2str(i) ',SOR}'],['L_{' int2str(i) ',ADI}'],['L_{' int2str(i) ',MG}'])
        end
    end

end