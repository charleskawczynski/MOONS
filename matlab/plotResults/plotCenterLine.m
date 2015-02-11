classdef plotCenterLine
    methods
        function obj = plotCenterLine()
        end
        function plotdata = From3D(obj,plotdata,dir,name,component,plotFinal,myPlot)

            addpath([dir.working name.field])

            [x,y,z,Nx,Ny,Nz,var] = getField(dir,name,component);

            if myPlot.directionPlane == 1
                N = ceil(1 + myPlot.fractionIntoPlane*(Nx - 1));
                var = var(N,:,:); var = reshape(var,Ny,Nz);
                plane = 'x'; cut = x(N);
                if myPlot.directionLine == 2
                    N = ceil(1 + myPlot.linePosition*(Nz - 1));
                    var = var(:,N); cutLine = z(N);
                    xcoord = y; xcoordLabel = 'y'; normalCoordLabel = 'z';
                elseif myPlot.directionLine == 3
                    N = ceil(1 + myPlot.linePosition*(Ny - 1));
                    var = var(N,:); cutLine = y(N);
                    xcoord = z; xcoordLabel = 'z'; normalCoordLabel = 'y';
                else
                    error('Cutplane and line cannot be along same direction')
                end
            elseif myPlot.directionPlane == 2
                N = ceil(1 + myPlot.linePosition*(Ny - 1));
                var = var(:,N,:); var = reshape(var,Nx,Nz);
                plane = 'y'; cut = y(N);
                if myPlot.directionLine == 1
                    N = ceil(1 + myPlot.linePosition*(Nz - 1));
                    var = var(:,N); cutLine = z(N);
                    xcoord = x; xcoordLabel = 'x'; normalCoordLabel = 'z';
                elseif myPlot.directionLine == 3
                    N = ceil(1 + myPlot.linePosition*(Nx - 1));
                    var = var(N,:); cutLine = x(N);
                    xcoord = z; xcoordLabel = 'z'; normalCoordLabel = 'x';
                else
                    error('Cutplane and line cannot be along same direction')
                end
            elseif myPlot.directionPlane == 3
                N = ceil(1 + myPlot.fractionIntoPlane*(Nz - 1));
                var = var(:,:,N); var = reshape(var,Nx,Ny);
                plane = 'z'; cut = z(N);
                if myPlot.directionLine == 1
                    N = ceil(1 + myPlot.linePosition*(Ny - 1));
                    var = var(:,N); cutLine = y(N);
                    xcoord = x; xcoordLabel = 'x'; normalCoordLabel = 'y';
                elseif myPlot.directionLine == 2
                    N = ceil(1 + myPlot.linePosition*(Nx - 1));
                    var = var(N,:); cutLine = x(N);
                    xcoord = y; xcoordLabel = 'y'; normalCoordLabel = 'x';
                else
                    error('Cutplane and line cannot be along same direction')
                end
            end

            rmpath([dir.working name.field '\'])

            plotdata.var{:,plotdata.i} = var;
            plotdata.x{:,plotdata.i} = xcoord;
            plotdata.(['title' num2str(plotdata.i)]) = name.title;
            plotdata.i = plotdata.i + 1;

            if plotFinal

                figure('Position',myPlot.vector);
                T = [plotdata.x;plotdata.var];
                plot(T{:})
                xlabel(xcoordLabel)
                fullTitle = '';
                if isfield(plotdata,'totalTitle')
                    fullTitle = plotdata.totalTitle;
                else
                    if isfield(plotdata,'preTitle')
                        fullTitle = plotdata.preTitle;
                    end
                    for i=1:3
                        if i==3
                            fullTitle = [fullTitle plotdata.(['title' num2str(i)])];
                        else
                            fullTitle = [fullTitle plotdata.(['title' num2str(i)]) ','];
                        end
                    end
                end
                if isfield(plotdata,'totalYLabel')
                    ylabel(plotdata.totalYLabel)
                else
                    ylabel(fullTitle)
                end
                title([fullTitle ' at plane ' plane ' = ' num2str(cut) ' , along ' xcoordLabel ', at ' normalCoordLabel ' = ' num2str(cutLine)])
                for i=1:plotdata.i-1
                    legend_fcn = @(i)sprintf('%s',[plotdata.(['title' num2str(i)])]);
                    legend(cellfun(legend_fcn, num2cell(1:plotdata.i-1) , 'UniformOutput', false));
                end

                if isfield(plotdata,'totalTitle')
                    saveMyPlot(dir,[name.field '\centerLine_' name.field '_' plotdata.totalTitle])
                else
                    saveMyPlot(dir,[name.field '\centerLine_' name.field])
                end

                plotdata = struct;
            end

        end
        function plotdata = From3DNew(obj,plotdata,dir,name,component,plotFinal,myPlot)

            addpath([dir.working name.field])

            [x,y,z,Nx,Ny,Nz,var] = getField(dir,name,component);

            if myPlot.directionPlane == 1
                N = ceil(1 + myPlot.fractionIntoPlane*(Nx - 1));
                var = var(N,:,:); var = reshape(var,Ny,Nz);
                plane = 'x';
                if myPlot.directionLine == 2
                    N = ceil(1 + myPlot.linePosition*(Nz - 1));
                    var = var(:,N);
                    xcoord = y; xcoordLabel = 'y'; normalCoordLabel = 'z';
                elseif myPlot.directionLine == 3
                    N = ceil(1 + myPlot.linePosition*(Ny - 1));
                    var = var(N,:);
                    xcoord = z; xcoordLabel = 'z'; normalCoordLabel = 'y';
                else
                    error('Cutplane and line cannot be along same direction')
                end
            elseif myPlot.directionPlane == 2
                N = ceil(1 + myPlot.linePosition*(Ny - 1));
                var = var(:,N,:); var = reshape(var,Nx,Nz);
                plane = 'y';
                if myPlot.directionLine == 1
                    N = ceil(1 + myPlot.linePosition*(Nz - 1));
                    var = var(:,N);
                    xcoord = x; xcoordLabel = 'x'; normalCoordLabel = 'z';
                elseif myPlot.directionLine == 3
                    N = ceil(1 + myPlot.linePosition*(Nx - 1));
                    var = var(N,:);
                    xcoord = z; xcoordLabel = 'z'; normalCoordLabel = 'x';
                else
                    error('Cutplane and line cannot be along same direction')
                end
            elseif myPlot.directionPlane == 3
                N = ceil(1 + myPlot.fractionIntoPlane*(Nz - 1));
                var = var(:,:,N); var = reshape(var,Nx,Ny);
                plane = 'z';
                if myPlot.directionLine == 1
                    N = ceil(1 + myPlot.linePosition*(Ny - 1));
                    var = var(:,N);
                    xcoord = x; xcoordLabel = 'x'; normalCoordLabel = 'y';
                elseif myPlot.directionLine == 2
                    N = ceil(1 + myPlot.linePosition*(Nx - 1));
                    var = var(N,:);
                    xcoord = y; xcoordLabel = 'y'; normalCoordLabel = 'x';
                else
                    error('Cutplane and line cannot be along same direction')
                end
            end

            rmpath([dir.working name.field '\'])

            plotdata.var{:,plotdata.i} = var;
            plotdata.x{:,plotdata.i} = xcoord;
            plotdata.(['title' num2str(plotdata.i)]) = name.title;
            plotdata.i = plotdata.i + 1;

            if plotFinal

                figure('Position',myPlot.vector);
                T = [plotdata.x;plotdata.var];
                plot(T{:})
                xlabel(xcoordLabel)
                fullTitle = '';
                if isfield(plotdata,'totalTitle')
                    fullTitle = plotdata.totalTitle;
                else
                    if isfield(plotdata,'preTitle')
                        fullTitle = plotdata.preTitle;
                    end
                    for i=1:3
                        if i==3
                            fullTitle = [fullTitle plotdata.(['title' num2str(i)])];
                        else
                            fullTitle = [fullTitle plotdata.(['title' num2str(i)]) ','];
                        end
                    end
                end
                if isfield(plotdata,'totalYLabel')
                    ylabel(plotdata.totalYLabel)
                else
                    ylabel(fullTitle)
                end
                title([fullTitle ' at '  num2str(myPlot.fractionIntoPlane*100) '% into ' plane ' plane, cutline along '...
                    xcoordLabel ', ' num2str(myPlot.linePosition*100) '% into ' normalCoordLabel  ])
                for i=1:plotdata.i-1
                    legend_fcn = @(i)sprintf('%s',[plotdata.(['title' num2str(i)])]);
                    legend(cellfun(legend_fcn, num2cell(1:plotdata.i-1) , 'UniformOutput', false));
                end

                if isfield(plotdata,'totalTitle')
                    saveMyPlot(dir,[name.field '\centerLine_' name.field '_' plotdata.totalTitle])
                else
                    saveMyPlot(dir,[name.field '\centerLine_' name.field])
                end

                plotdata = struct;
            end

        end
        function plotdata = VecFrom3D(obj,plotdata,dir,name,plotFinal,myPlot)

            addpath([dir.working name.field '\'])

            [x,y,z,Nx,Ny,Nz,varx,vary,varz] = getVecField(dir,name,[4 5 6]);
            if myPlot.directionPlane == 1
                N = ceil(1 + myPlot.fractionIntoPlane*(Nx - 1));
                varx = varx(N,:,:); varx = reshape(varx,Ny,Nz);
                vary = vary(N,:,:); vary = reshape(vary,Ny,Nz);
                varz = varz(N,:,:); varz = reshape(varz,Ny,Nz);
                plane = 'x'; cut = x(N);
                if myPlot.directionLine == 2
                    N = ceil(1 + myPlot.linePosition*(Nz - 1)); cutLine = z(N);
                    varx = varx(:,N); vary = vary(:,N); varz = varz(:,N);
                    xcoord = y; xcoordLabel = 'y'; normalCoordLabel = 'z';
                elseif myPlot.directionLine == 3
                    N = ceil(1 + myPlot.linePosition*(Ny - 1)); cutLine = y(N);
                    varx = varx(N,:); vary = vary(N,:); varz = varz(N,:);
                    xcoord = z; xcoordLabel = 'z'; normalCoordLabel = 'y';
                else
                    error('Cutplane and line cannot be along same direction')
                end
            elseif myPlot.directionPlane == 2
                N = ceil(1 + myPlot.linePosition*(Ny - 1));
                varx = varx(:,N,:); varx = reshape(varx,Nx,Nz);
                vary = vary(:,N,:); vary = reshape(vary,Nx,Nz);
                varz = varz(:,N,:); varz = reshape(varz,Nx,Nz);
                plane = 'y'; cut = y(N);
                if myPlot.directionLine == 1
                    N = ceil(1 + myPlot.linePosition*(Nz - 1)); cutLine = z(N);
                    varx = varx(:,N); vary = vary(:,N); varz = varz(:,N);
                    xcoord = x; xcoordLabel = 'x'; normalCoordLabel = 'z';
                elseif myPlot.directionLine == 3
                    N = ceil(1 + myPlot.linePosition*(Nx - 1)); cutLine = x(N);
                    varx = varx(N,:); vary = vary(N,:); varz = varz(N,:);
                    xcoord = z; xcoordLabel = 'z'; normalCoordLabel = 'x';
                else
                    error('Cutplane and line cannot be along same direction')
                end
            elseif myPlot.directionPlane == 3
                N = ceil(1 + myPlot.fractionIntoPlane*(Nz - 1));
                varx = varx(:,:,N); varx = reshape(varx,Nx,Ny);
                vary = vary(:,:,N); vary = reshape(vary,Nx,Ny);
                varz = varz(:,:,N); varz = reshape(varz,Nx,Ny);
                plane = 'z'; cut = z(N);
                if myPlot.directionLine == 1
                    N = ceil(1 + myPlot.linePosition*(Ny - 1)); cutLine = y(N);
                    varx = varx(:,N); vary = vary(:,N); varz = varz(:,N);
                    xcoord = x; xcoordLabel = 'x'; normalCoordLabel = 'y';
                elseif myPlot.directionLine == 2
                    N = ceil(1 + myPlot.linePosition*(Nx - 1)); cutLine = x(N);
                    varx = varx(N,:); vary = vary(N,:); varz = varz(N,:);
                    xcoord = y; xcoordLabel = 'y'; normalCoordLabel = 'x';
                else
                    error('Cutplane and line cannot be along same direction')
                end
            end

            rmpath([dir.working name.field '\'])

            plotdata.var{:,plotdata.i} = varx;
            plotdata.x{:,plotdata.i} = xcoord;
            plotdata.(['title' num2str(plotdata.i)]) = name.titlex;
            plotdata.i = plotdata.i + 1;

            plotdata.var{:,plotdata.i} = vary;
            plotdata.x{:,plotdata.i} = xcoord;
            plotdata.(['title' num2str(plotdata.i)]) = name.titley;
            plotdata.i = plotdata.i + 1;

            plotdata.var{:,plotdata.i} = varz;
            plotdata.x{:,plotdata.i} = xcoord;
            plotdata.(['title' num2str(plotdata.i)]) = name.titlez;
            plotdata.i = plotdata.i + 1;

            for i=1:plotdata.i-1
                x = plotdata.x{:,i}; y = plotdata.var{:,i};
                T = [x(:) y(:)]; %#ok
                save([dir.working name.field '\' 'centerLine_' plotdata.(['title' num2str(i)]) '.dat'] ,'T','-ascii')
            end

            if plotFinal

                figure('Position',myPlot.vector);
                T = [plotdata.x;plotdata.var];
                plot(T{:})
                xlabel(xcoordLabel)
                fullTitle = '';
                if isfield(plotdata,'totalTitle')
                    fullTitle = plotdata.totalTitle;
                else
                    if isfield(plotdata,'preTitle')
                        fullTitle = plotdata.preTitle;
                    end
                    for i=1:3
                        if i==3
                            fullTitle = [fullTitle plotdata.(['title' num2str(i)])];
                        else
                            fullTitle = [fullTitle plotdata.(['title' num2str(i)]) ','];
                        end
                    end
                end
                if isfield(plotdata,'totalYLabel')
                    ylabel(plotdata.totalYLabel)
                else
                    ylabel(fullTitle)
                end
                % This title is old...
                %     title([fullTitle ' at '  num2str(myPlot.fractionIntoPlane*100) '% into ' plane ' plane, cutline along '...
                %         xcoordLabel ', ' num2str(myPlot.linePosition*100) '% into ' normalCoordLabel  ])
                title([fullTitle ' at plane ' plane ' = ' num2str(cut) ' , along ' xcoordLabel ', at ' normalCoordLabel ' = ' num2str(cutLine)])
                for i=1:plotdata.i-1
                    legend_fcn = @(i)sprintf('%s',[plotdata.(['title' num2str(i)])]);
                    legend(cellfun(legend_fcn, num2cell(1:plotdata.i-1) , 'UniformOutput', false));
                end
                if isfield(plotdata,'totalTitle')
                    name.saveFile = ['centerLine_' name.field '_' plotdata.totalTitle];
                else
                    name.saveFile = ['centerLine_' name.field];
                end
                saveMyPlot(dir,name)

                plotdata = struct;
            end

        end
    end
end
