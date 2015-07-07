function [n norms] = plotNorms(name,saveToFile)
norms = load([name.file name.ext]);
n = 1:size(norms(:,1));

if saveToFile
    T = [n' norms(:,3)]; %#ok<NASGU>
    save(['norms ' name.file '.dat'],'T','-ascii');
end

% figure
% semilogy(n,norms)
% title(['Convergence rate of ' name.file])
% xlabel('Iteration')
% ylabel('norm')
% legend('L_1','L_2','L_{\infty}')


end