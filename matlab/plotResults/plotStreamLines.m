classdef plotStreamLines
    methods
        function obj = plotStreamLines()
        end
        function plotdata = from3D(plotdata,dir,name,plotFinal,myPlot)

            addpath([dir.working name.field])

            [x,y,z,Nx,Ny,Nz,varx,vary,varz] = getVecField(dir,name,[4 5 6]);

            if myPlot.directionPlane == 1
                N = ceil(1 + myPlot.fractionIntoPlane*(Nx - 1));
                varx = varx(N,:,:); varx = reshape(varx,Ny,Nz);
                vary = vary(N,:,:); vary = reshape(vary,Ny,Nz);
                varz = varz(N,:,:); varz = reshape(varz,Ny,Nz);
                xcoord = y; ycoord = z; cut = x(N);
                xcoordLabel = 'y'; ycoordLabel = 'z'; plane = 'x';
            elseif myPlot.directionPlane == 2
                N = ceil(1 + myPlot.fractionIntoPlane*(Ny - 1));
                varx = varx(:,N,:); varx = reshape(varx,Nx,Nz);
                vary = vary(:,N,:); vary = reshape(vary,Nx,Nz);
                varz = varz(:,N,:); varz = reshape(varz,Nx,Nz);
                xcoord = x; ycoord = z; cut = y(N);
                xcoordLabel = 'x'; ycoordLabel = 'z'; plane = 'y';
            elseif myPlot.directionPlane == 3
                N = ceil(1 + myPlot.fractionIntoPlane*(Nz - 1));
                varx = varx(:,:,N); varx = reshape(varx,Nx,Ny);
                vary = vary(:,:,N); vary = reshape(vary,Nx,Ny);
                varz = varz(:,:,N); varz = reshape(varz,Nx,Ny);
                xcoord = x; ycoord = y; cut = z(N);
                xcoordLabel = 'x'; ycoordLabel = 'y'; plane = 'z';
            end

            rmpath([dir.working name.field '\'])

            plotdata.varx(:,:,plotdata.i) = varx;
            plotdata.x(:,plotdata.i) = xcoord;
            plotdata.y(:,plotdata.i) = ycoord;
            plotdata.(['title' num2str(plotdata.i*3-2)]) = name.titlex;

            plotdata.vary(:,:,plotdata.i) = vary;
            plotdata.x(:,plotdata.i) = xcoord;
            plotdata.y(:,plotdata.i) = ycoord;
            plotdata.(['title' num2str(plotdata.i*3-1)]) = name.titley;

            plotdata.varz(:,:,plotdata.i) = varz;
            plotdata.x(:,plotdata.i) = xcoord;
            plotdata.y(:,plotdata.i) = ycoord;
            plotdata.(['title' num2str(plotdata.i*3)]) = name.titlez;
            plotdata.i = plotdata.i + 1;

            if plotFinal

                figure('Position',myPlot.vector);
                if myPlot.vectorType == 1
                    if myPlot.directionPlane == 1
                        streamslice(plotdata.x(:,1),plotdata.y(:,1),plotdata.vary',plotdata.varz')
                    elseif myPlot.directionPlane == 2
                        streamslice(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.varz')
                    elseif myPlot.directionPlane == 3
                        ntraces = 5;
                        x = linspace(min(plotdata.x(:,1)),max(plotdata.x(:,1)),ntraces);
                        y = linspace(min(plotdata.y(:,1)),max(plotdata.y(:,1)),ntraces);
                        [X,Y] = meshgrid(plotdata.x(:,1),plotdata.y(:,1));
                        [sx,sy] = meshgrid(x,y);
                        XY = stream2(X,Y,plotdata.varx',plotdata.vary',sx,sy);
                        streamline(XY);
                        %             streamslice(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.vary')
                    end
                elseif myPlot.vectorType == 2
                    if myPlot.directionPlane == 1
                        quiver(plotdata.x(:,1),plotdata.y(:,1),plotdata.vary',plotdata.varz',myPlot.arrowSize)
                    elseif myPlot.directionPlane == 2
                        quiver(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.varz',myPlot.arrowSize)
                    elseif myPlot.directionPlane == 3
                        quiver(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.vary',myPlot.arrowSize)
                    end
                elseif myPlot.vectorType == 3
                    if myPlot.directionPlane == 1
                        contour(plotdata.x(:,1),plotdata.y(:,1),plotdata.vary',plotdata.varz'); hold on
                        quiver(plotdata.x(:,1),plotdata.y(:,1),plotdata.vary',plotdata.varz',myPlot.arrowSize)
                    elseif myPlot.directionPlane == 2
                        contour(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.varz'); hold on
                        quiver(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.varz',myPlot.arrowSize)
                    elseif myPlot.directionPlane == 3
                        contour(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.vary'); hold on
                        quiver(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.vary',myPlot.arrowSize)
                    end
                end
                xlabel(xcoordLabel)
                ylabel(ycoordLabel)
                fullTitle = '';
                for i=1:3
                    if i==3
                        fullTitle = [fullTitle plotdata.(['title' num2str(i)])];
                    else
                        fullTitle = [fullTitle plotdata.(['title' num2str(i)]) ','];
                    end
                end
                title([fullTitle ' at plane ' plane ' = ' num2str(cut)])
                for i=1:plotdata.i-1
                    legend_fcn = @(i)sprintf('%s',[fullTitle ' (' num2str(i) ')']);
                    legend(cellfun(legend_fcn, num2cell(1:plotdata.i-1) , 'UniformOutput', false));
                end
                axis([min(y) max(y) min(z) max(z)])
                if myPlot.rotate
                    view(-90,90)
                end
                if myPlot.vectorType == 1
                    name.saveFile = ['\streamLines_' name.field];
                elseif myPlot.vectorType == 2
                    name.saveFile = ['\quiver_' name.field];
                elseif myPlot.vectorType == 3
                    name.saveFile = ['\quiverContour_' name.field];
                end
                saveMyPlot(dir,name)

                plotdata = struct;
            end

        end
        function plotdata = plotStreamLinesFrom3D(plotdata,dir,name,plotFinal,myPlot)

            addpath([dir.working name.field])

            [x,y,z,Nx,Ny,Nz,varx,vary,varz] = getVecField(dir,name,[4 5 6]);

            if myPlot.directionPlane == 1
                N = ceil(1 + myPlot.fractionIntoPlane*(Nx - 1));
                xcoord = y; ycoord = z;  zcoord = x;
                cut = x(N);
                xcoordLabel = 'y'; ycoordLabel = 'z'; plane = 'x';
            elseif myPlot.directionPlane == 2
                N = ceil(1 + myPlot.fractionIntoPlane*(Ny - 1));
                xcoord = x; ycoord = z; zcoord = y;
                cut = y(N);
                xcoordLabel = 'x'; ycoordLabel = 'z'; plane = 'y';
            elseif myPlot.directionPlane == 3
                N = ceil(1 + myPlot.fractionIntoPlane*(Nz - 1));
                xcoord = x; ycoord = y; zcoord = z;
                cut = z(N);
                xcoordLabel = 'x'; ycoordLabel = 'y'; plane = 'z';
            end

            rmpath([dir.working name.field '\'])

            plotdata.varx(:,:,:,plotdata.i) = varx;
            plotdata.(['title' num2str(plotdata.i*3-2)]) = name.titlex;

            plotdata.vary(:,:,:,plotdata.i) = vary;
            plotdata.(['title' num2str(plotdata.i*3-1)]) = name.titley;

            plotdata.varz(:,:,:,plotdata.i) = varz;
            plotdata.(['title' num2str(plotdata.i*3)]) = name.titlez;

            plotdata.x(:,plotdata.i) = xcoord;
            plotdata.y(:,plotdata.i) = ycoord;
            plotdata.z(:,plotdata.i) = zcoord;
            plotdata.i = plotdata.i + 1;

            if plotFinal

                figure('Position',myPlot.vector);
                if myPlot.vectorType == 1
                    if myPlot.directionPlane == 1
                        streamslice(plotdata.x(:,1),plotdata.y(:,1),plotdata.vary',plotdata.varz')
                    elseif myPlot.directionPlane == 2
                        streamslice(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.varz')
                    elseif myPlot.directionPlane == 3
                        % The following seems to work, but tecplot just looks so much
                        % better... Sorry matlab...
                        ntraces = 3;
                        range = N:Nz-1;
                        x = linspace(min(plotdata.x(:,1)),max(plotdata.x(:,1)),ntraces);
                        y = linspace(min(plotdata.y(:,1)),max(plotdata.y(:,1)),ntraces);
                        z = plotdata.z(range,1);
                        [X,Y,Z] = meshgrid(plotdata.x(:,1),plotdata.y(:,1),z);
                        [sx,sy,sz] = meshgrid(x,y,z);
                        plotdata.varx = permute(plotdata.varx,[2 1 3]);
                        plotdata.vary = permute(plotdata.vary,[2 1 3]);
                        plotdata.varz = permute(plotdata.varz,[2 1 3]);
                        streamline(X,Y,Z,plotdata.varx(:,:,range),plotdata.vary(:,:,range),plotdata.varz(:,:,range),sx,sy,sz);
                    end
                elseif myPlot.vectorType == 2
                    if myPlot.directionPlane == 1
                        quiver(plotdata.x(:,1),plotdata.y(:,1),plotdata.vary',plotdata.varz',myPlot.arrowSize)
                    elseif myPlot.directionPlane == 2
                        quiver(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.varz',myPlot.arrowSize)
                    elseif myPlot.directionPlane == 3
                        quiver(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.vary',myPlot.arrowSize)
                    end
                elseif myPlot.vectorType == 3
                    if myPlot.directionPlane == 1
                        contour(plotdata.x(:,1),plotdata.y(:,1),plotdata.vary',plotdata.varz'); hold on
                        quiver(plotdata.x(:,1),plotdata.y(:,1),plotdata.vary',plotdata.varz',myPlot.arrowSize)
                    elseif myPlot.directionPlane == 2
                        contour(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.varz'); hold on
                        quiver(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.varz',myPlot.arrowSize)
                    elseif myPlot.directionPlane == 3
                        contour(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.vary'); hold on
                        quiver(plotdata.x(:,1),plotdata.y(:,1),plotdata.varx',plotdata.vary',myPlot.arrowSize)
                    end
                end
                xlabel(xcoordLabel)
                ylabel(ycoordLabel)
                fullTitle = '';
                for i=1:3
                    if i==3
                        fullTitle = [fullTitle plotdata.(['title' num2str(i)])];
                    else
                        fullTitle = [fullTitle plotdata.(['title' num2str(i)]) ','];
                    end
                end
                title([fullTitle ' at plane ' plane ' = ' num2str(cut)])
                for i=1:plotdata.i-1
                    legend_fcn = @(i)sprintf('%s',[fullTitle ' (' num2str(i) ')']);
                    legend(cellfun(legend_fcn, num2cell(1:plotdata.i-1) , 'UniformOutput', false));
                end
                axis([min(xcoord) max(xcoord) min(ycoord) max(ycoord)])
                if myPlot.rotate
                    view(-90,90)
                end
                if myPlot.vectorType == 1
                    name.saveFile = ['\streamLines_' name.field];
                elseif myPlot.vectorType == 2
                    name.saveFile = ['\quiver_' name.field];
                elseif myPlot.vectorType == 3
                    name.saveFile = ['\quiverContour_' name.field];
                end
                saveMyPlot(dir,name)

                plotdata = struct;
            end

        end
    end
end
