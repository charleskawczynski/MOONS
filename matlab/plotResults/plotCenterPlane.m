classdef plotCenterPlane
    methods
        function obj = plotCenterPlane()
        end
        function From3D(CP,dir,name,component,myPlot)

            addpath([dir.working name.field])

            if component~=4 && component~=5 && component~=6
                error('Please check the prescribed column.')
            end

            [x,y,z,Nx,Ny,Nz,var] = getField(dir,name,component);

            if myPlot.directionPlane == 1
                N = ceil(1 + myPlot.fractionIntoPlane*(Nx - 1));
                var = var(N,:,:); var = reshape(var,Ny,Nz);
                xcoord = y; ycoord = z;
                cut = x(N);
                xcoordLabel = 'y'; ycoordLabel = 'z'; plane = 'x';
            elseif myPlot.directionPlane == 2
                N = ceil(1 + myPlot.fractionIntoPlane*(Ny - 1));
                var = var(:,N,:); var = reshape(var,Nx,Nz);
                cut = y(N);
                xcoord = x; ycoord = z;
                xcoordLabel = 'x'; ycoordLabel = 'z'; plane = 'y';
            elseif myPlot.directionPlane == 3
                N = ceil(1 + myPlot.fractionIntoPlane*(Nz - 1));
                var = var(:,:,N); var = reshape(var,Nx,Ny);
                cut = z(N);
                xcoord = x; ycoord = y;
                xcoordLabel = 'x'; ycoordLabel = 'y'; plane = 'z';
            end

            if max(max(var))==0 && min(min(var))==0
                [msg, id] = lastwarn;
                try
                    warning('off', id)
                catch
                end
            end

            if isfield(myPlot,'subPlotNum')
                if myPlot.subPlotNum==1
                    figure('Position',myPlot.vector);
                end
                subplot(myPlot.m,myPlot.n,myPlot.subPlotNum)
            else
                figure('Position',myPlot.vector);
            end
            if myPlot.scalarType==1
                if myPlot.specifyLevels
                    contour(xcoord,ycoord,var',myPlot.nlevels)
                else
                    contour(xcoord,ycoord,var')
                end
                view(2)
            elseif myPlot.scalarType==2
                if myPlot.specifyLevels
                    contourf(xcoord,ycoord,var',myPlot.nlevels)
                else
                    contourf(xcoord,ycoord,var')
                end
                view(2)
            elseif myPlot.scalarType == 3
                surf(xcoord,ycoord,var')
            elseif myPlot.scalarType == 4
                if myPlot.specifyLevels
                    meshc(xcoord,ycoord,var',myPlot.nlevels)
                else
                    meshc(xcoord,ycoord,var')
                end
            end
            if myPlot.rotate
                view(-90,90)
            end
            colorbar
            xlabel(xcoordLabel)
            ylabel(ycoordLabel)
            title([name.title ' at ' plane ' = ' num2str(cut)])
            axis square

            saveMyPlot(dir,name)

%             rmpath([dir.working name.field])

        end
        function From3DGivenData(CP,dir,name,myPlot,x,y,z,Nx,Ny,Nz,var)

            if myPlot.directionPlane == 1
                N = ceil(1 + myPlot.fractionIntoPlane*(Nx - 1));
                var = var(N,:,:); var = reshape(var,Ny,Nz);
                xcoord = y; ycoord = z;
                cut = x(N);
                xcoordLabel = 'y'; ycoordLabel = 'z'; plane = 'x';
            elseif myPlot.directionPlane == 2
                N = ceil(1 + myPlot.fractionIntoPlane*(Ny - 1));
                var = var(:,N,:); var = reshape(var,Nx,Nz);
                cut = y(N);
                xcoord = x; ycoord = z;
                xcoordLabel = 'x'; ycoordLabel = 'z'; plane = 'y';
            elseif myPlot.directionPlane == 3
                N = ceil(1 + myPlot.fractionIntoPlane*(Nz - 1));
%                 N = myPlot.NIntoPlane;
                var = var(:,:,N); var = reshape(var,Nx,Ny);
                cut = z(N);
                xcoord = x; ycoord = y;
                xcoordLabel = 'x'; ycoordLabel = 'y'; plane = 'z';
            end

            if max(max(var))==0 && min(min(var))==0
                [msg, id] = lastwarn;
                %warning('off', id)
            end

            if isfield(myPlot,'subPlotNum')
                if myPlot.subPlotNum==1
                    figure('Position',myPlot.vector);
                end
                subplot(2,2,myPlot.subPlotNum)
            else
                figure('Position',myPlot.vector);
            end
            if myPlot.scalarType==1
                if myPlot.specifyLevels
                    contour(xcoord,ycoord,var',myPlot.nlevels)
                else
                    contour(xcoord,ycoord,var')
                end
                view(2)
            elseif myPlot.scalarType==2
                if myPlot.specifyLevels
                    contourf(xcoord,ycoord,var',myPlot.nlevels)
                else
                    contourf(xcoord,ycoord,var')
                end
                view(2)
            elseif myPlot.scalarType == 3
                surf(xcoord,ycoord,var')
            elseif myPlot.scalarType == 4
                if myPlot.specifyLevels
                    meshc(xcoord,ycoord,var',myPlot.nlevels)
                else
                    meshc(xcoord,ycoord,var')
                end
            end
            colorbar
            xlabel(xcoordLabel)
            ylabel(ycoordLabel)
            title([name.title ' at plane ' plane ' = ' num2str(cut)])
            axis square
            if myPlot.rotate
                view(-90,90)
            end

            temp = name;
            if findstr('\',name.title)
                name.file = ['\centerPlane_' name.file];
                saveMyPlot(dir,name)
            else
                name.file = ['\centerPlane_' name.titlex ',' name.titley ',' name.titlez];
                saveMyPlot(dir,name)
            end
            name = temp; clear temp

        end
        function VecFrom3D(CP,dir,name,components,myPlot)
            %% VecFrom3D plots the xyz components of the vector field
            % specified by the name of the file in name.

            addpath([dir.working name.field '\'])

            [x,y,z,Nx,Ny,Nz,varx,vary,varz] = getVecField(dir,name,[components(1) components(2) components(3)]);

            name.title = name.titlex;
            myPlot.subPlotTF = true;
            myPlot.subPlotNum = 1;
            dir.saveTo = false;
            CP.From3DGivenData(dir,name,myPlot,x,y,z,Nx,Ny,Nz,varx)
            name.title = name.titley;
            myPlot.subPlotNum = 2;
            CP.From3DGivenData(dir,name,myPlot,x,y,z,Nx,Ny,Nz,vary)
            name.title = name.titlez;
            myPlot.subPlotNum = 3;
            % dir.saveTo = true;
            CP.From3DGivenData(dir,name,myPlot,x,y,z,Nx,Ny,Nz,varz)

            rmpath([dir.working name.field '\'])

        end
    end
end
