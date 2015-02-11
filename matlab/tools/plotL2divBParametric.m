function plotL2divBParametric(dir,myPlot)

Niterations = 1;  i=1:Niterations;
maxIterations = 20*(i-1);

with = true;
starti = 3;

for i=1:Niterations
    if i>1 || with
        addpath([dir.working '\i' num2str(i) '\'])
        data = load('Brackbill_Rem.dat');
        rmpath([dir.working '\i' num2str(i) '\'])

        timeStep{i} = data(starti:end,1);
        tstar{i} = data(starti:end,2);
        t_CP{i} = data(starti:end,4);
        L2{i} = data(starti:end,5);
        Linf{i} = data(starti:end,6);
    end
    if i==Niterations
        if ~with
            timeStep{1} = [];
            L2{1} = [];
        end
        T = [timeStep;L2];
    end
end

figure('Position',myPlot.vector);
loglog(T{:})
title('Performance of Cleaning Procedure')
xlabel('Time Step')
ylabel('L_2(0,\nabla \bullet B)')

if with
    for i=1:Niterations
        legend_fcn = @(i)sprintf('SOR iter. = %3.1f',maxIterations(i));
        legend(cellfun(legend_fcn, num2cell(1:Niterations) , 'UniformOutput', false));
    end
else
    for i=2:Niterations
        legend_fcn = @(i)sprintf('SOR iter. = %3.1f',maxIterations(i));
        legend(cellfun(legend_fcn, num2cell(2:Niterations) , 'UniformOutput', false));
    end
end

legend('Location','southeast')
% legend('Location','northwest')

if dir.saveTo
    fh = gcf; temp = [dir.working 'L2divBparametric'];
    saveas(fh,temp,'fig'); print('-depsc',temp); print('-dpng',temp)
end

end