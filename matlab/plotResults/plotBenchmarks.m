classdef plotBenchmarks
    methods
        function obj = plotBenchmarks()
        end

        function benchmarkMOONS(obj,myDir,myPlot,name)
            %% benchmarkMOONS
            data = struct();
            benchmarkCase = 106;
            switch (benchmarkCase)
                case 0 % Symmetry test: LDC, w should = 0 at center plane
                    data = obj.benchmarkCase0(myDir,myPlot,name);
                case 100 % Guj & Stella: LDC, Re = 400, Ha = 0
                    data = obj.benchmarkCase100(myDir,myPlot,name,1);
                    % data = obj.benchmarkCase100(myDir,myPlot,name,2)
                case 101 % Guj & Stella: LDC, Re = 1000, Ha = 0
                    data = obj.benchmarkCase101(myDir,myPlot,name,1);
                    % data = obj.benchmarkCase2(myDir,myPlot,name,2)
                case 102 % Sergey: LDC, Re = 100, Ha = 10
                    data = obj.benchmarkCase102(myDir,myPlot,name);
                case 103 % Sergey: LDC, Re = 1000, Ha = 100
                    data = obj.benchmarkCase103(myDir,myPlot,name);
                case 104 % Sergey: LDC, Re = 1000, Ha = 1000
                    data = obj.benchmarkCase104(myDir,myPlot,name);
                case 105
                    data = obj.benchmarkCase105(myDir,myPlot,name);
                case 106
                    data = obj.benchmarkCase106(myDir,myPlot,name);
            end
            obj.plotMOONSSource(data,myDir,myPlot,name)

        end
        function plotMOONSSource(obj,data,myDir,myPlot,name) %#ok
            %% Plot settings

            myPlot.directionPlane = 3;
            myPlot.fractionIntoPlane = 0.5; myPlot.NIntoPlane = 20;
            myPlot.linePosition = 0.5; myPlot.NlinePosition = 5;
            myPlot.rotate = true;
            name.field = 'Ufield';
            name.varx = 'u';
            name.vary = 'v';
            name.varz = 'w';
            name.file = [name.varx 'ni,' name.vary 'ni,' name.varz 'ni'];
            name.titlex = [name.varx 'ni']; name.titley = [name.vary 'ni']; name.titlez = [name.varz 'ni'];
            myDir.working = myDir.source;
            myDir.saveTo = true;
            CL = plotCenterLine();
            CL.VecFrom3D(data,myDir,name,true,myPlot);
        end

        function data = benchmarkCase0(obj,myDir,myPlot,name) %#ok
            %% Only MOONS is required for this benchmark
        end
        function data = benchmarkCase100(obj,myDir,myPlot,name,velocity) %#ok
            %% Guj & Stella: LDC, Re = 400, Ha = 0

            smoothness = 1;

            % Get benchmarking data
            myDir.working = 'C:\Users\Charlie\Desktop\benchmark\Guj and Stella\data\';
            addpath(myDir.working)

            gd.xmin = -0.4; gd.xmax = 1; gd.ymin = 0; gd.ymax = 1;
            name.file = 'Re400_uvsy'; name.ext = '.png';
            [y1 u1] = graphDigitizer(myDir,name,gd,smoothness,2);
            T = [y1 u1']; %#ok
            save('Re400_uvsy_GujStella.dat','T','-ascii')

            gd.xmin = 0; gd.xmax = 1; gd.ymin = -0.5; gd.ymax = 0.3;
            name.file = 'Re400_vvsx'; name.ext = '.png';
            [x1 v1] = graphDigitizer(myDir,name,gd,smoothness,1);
            T = [x1 v1']; %#ok
            save('Re400_vvsx_GujStella.dat','T','-ascii')

            %% uvsy
            if (velocity==1)
                myPlot.directionLine = 2;
                data.i = 1;
                data.x{:,data.i} = u1;
                data.var{:,data.i} = y1';
                data.(['title' num2str(data.i)]) = 'u - Guj and Stella';
                data.i = data.i+1;
            elseif velocity==2
                %% vvsx
                myPlot.directionLine = 1;
                data.i = 1;
                data.x{:,data.i} = x1;
                data.var{:,data.i} = v1';
                data.(['title' num2str(data.i)]) = 'v - Guj and Stella';
                data.i = data.i+1;
            else
                error('Only u vs y and v vs x data exist. Choose velocity = 1 or 2')
            end

            rmpath(myDir.working)
        end
        function data = benchmarkCase101(obj,myDir,myPlot,name,velocity) %#ok
            %% Guj & Stella: LDC, Re = 1000, Ha = 0

            smoothness = 1;

            % Get benchmarking data
            myDir.working = 'C:\Users\Charlie\Desktop\benchmark\Guj and Stella\data\';
            addpath(myDir.working)

            gd.xmin = -0.4; gd.xmax = 1; gd.ymin = 0; gd.ymax = 1;
            name.file = 'Re1000_uvsy'; name.ext = '.png';
            [y1 u1] = graphDigitizer(myDir,name,gd,smoothness,2);
            T = [y1 u1']; %#ok
            save('Re1000_uvsy_GujStella.dat','T','-ascii')

            gd.xmin = 0; gd.xmax = 1; gd.ymin = -0.5; gd.ymax = 0.4;
            name.file = 'Re1000_vvsx'; name.ext = '.png';
            [x1 v1] = graphDigitizer(myDir,name,gd,smoothness,1);
            T = [x1' v1]; %#ok
            save('Re1000_vvsx_GujStella.dat','T','-ascii')

            %% uvsy
            if (velocity==1)
                myPlot.directionLine = 2;
                data.i = 1;
                data.x{:,data.i} = u1;
                data.var{:,data.i} = y1';
                data.(['title' num2str(data.i)]) = 'u - Guj and Stella';
                data.i = data.i+1;
            elseif velocity==2
                %% vvsx
                myPlot.directionLine = 1;
                data.i = 1;
                data.x{:,data.i} = x1;
                data.var{:,data.i} = v1';
                data.(['title' num2str(data.i)]) = 'v - Guj and Stella';
                data.i = data.i+1;
            else
                error('Only u vs y and v vs x data exist. Choose velocity = 1 or 2')
            end

            rmpath(myDir.working)


        end
        function data = benchmarkCase102(obj,myDir,myPlot,name) %#ok
            %% Sergey: LDC, Re=100, Ha=10
            myDir.working = 'C:\Users\Charlie\Desktop\benchmark\MOONS\finished\uniformGrid\LDCP_Re100Ha10\sergey\';
            T = load([myDir.working 'U&V1D.dat']);
            x = T(:,1); 
            
            y = T(:,2);
            data.i = 1;
            data.x{:,data.i} = x;
            data.var{:,data.i} = y';
            name.title = 'u_{sergey}';
            data.(['title' num2str(data.i)]) = name.title;
            data.i = data.i+1;
            
            y = T(:,3);
            data.x{:,data.i} = x;
            data.var{:,data.i} = y';
            name.title = 'v_{sergey}';
            data.(['title' num2str(data.i)]) = name.title;
            data.i = data.i+1;
        end
        function data = benchmarkCase103(obj,myDir,myPlot,name) %#ok
            %% Sergey: LDC, Re=1000, Ha=100
            myDir.working = 'C:\Users\Charlie\Dropbox\UCLA\FUSION SHARED\CHARLIES RESEARCH\MHD\LDC\out\LDC\sergey\';
            T = load([myDir.working 'U&V1D.dat']);
            x = T(:,1); 
            
            y = T(:,2);
            data.i = 1;
            data.x{:,data.i} = x;
            data.var{:,data.i} = y';
            name.title = 'u_{sergey}';
            data.(['title' num2str(data.i)]) = name.title;
            data.i = data.i+1;
            
            y = T(:,3);
            data.x{:,data.i} = x;
            data.var{:,data.i} = y';
            name.title = 'v_{sergey}';
            data.(['title' num2str(data.i)]) = name.title;
            data.i = data.i+1;
        end
        function data = benchmarkCase103Old(obj,myDir,myPlot,name) %#ok
            %% Sergey: LDC, Re=1000, Ha=100
            myDir.working = 'C:\Users\Charlie\Desktop\benchmark\MOONS\finished\uniformGrid\LDCP_Re100Ha10\sergey\';
            data.i = 1;
            gd.xmin = -1.2; gd.xmax = 1.2;
            gd.ymin = -0.4; gd.ymax = 1.2;
            name.file = 'u(0,y,0)'; name.ext = '.png';
            name.field = ''; name.title = 'u_{sergey}';
            name.saveFile = 'Re1000Ha100.dat';
            data = obj.digitizeData(data,myDir,gd,name,1);
        end
        function data = benchmarkCase104(obj,myDir,myPlot,name) %#ok
            %% Sergey: LDC, Re=1000, Ha=1000
            data.i = 1;
        end
        function data = benchmarkCase105(obj,myDir,myPlot,name) %#ok
        end
        function data = benchmarkCase106(obj,myDir,myPlot,name) %#ok
            %% Sergey: LDC, Re=1000, Ha=100
            myDir.working = 'C:\Users\Charlie\Desktop\MOONS - BMCs\BMC_106 - sergey and peter\sergey\';
            T = load([myDir.working 'U&V1D.dat']);
            x = T(:,1); 
            
            y = T(:,2);
            data.i = 1;
            data.x{:,data.i} = x;
            data.var{:,data.i} = y';
            name.title = 'u_{sergey}';
            data.(['title' num2str(data.i)]) = name.title;
            data.i = data.i+1;
            
            y = T(:,3);
            data.x{:,data.i} = x;
            data.var{:,data.i} = y';
            name.title = 'v_{sergey}';
            data.(['title' num2str(data.i)]) = name.title;
            data.i = data.i+1;
        end

        function data = digitizeData(obj,data,myDir,gd,name,directionSweep) %#ok

            smoothness = 1;

            [x y] = graphDigitizer(myDir,name,gd,smoothness,directionSweep);
            T = [x y']; %#ok
            save(name.saveFile,'T','-ascii')

            data.x{:,data.i} = x;
            data.var{:,data.i} = y';
            data.(['title' num2str(data.i)]) = name.title;
            data.i = data.i+1;

        end
        function data = loadData(obj,data,myDir,name,myPlot) %#ok
            CL = plotCenterLine();
            data = CL.VecFrom3D(data,myDir,name,true,myPlot);

            myPlot.directionLine = directionLine;
            data.x{:,data.i} = x;
            data.var{:,data.i} = y';
            data.(['title' num2str(data.i)]) = name.title;
            data.i = data.i+1;
        end

        function data = loadData2(obj,data,myDir,name,myPlot) %#ok
            CL = plotCenterLine();
            data = CL.VecFrom3D(data,myDir,name,true,myPlot);

            data.x{:,data.i} = x;
            data.var{:,data.i} = y';
            data.(['title' num2str(data.i)]) = name.title;
            data.i = data.i+1;
        end


        %% OLD functions:
        function benchmarkMOONSOld(obj,myDir,myPlot,includePlot,name)
            %% ***************************************
            % Clear workspace and prescribe path
            % clear;clc;close all;
            % p = mfilename('fullpath');
            % [thisDir] = fileparts(p);
            % chdir(thisDir); myDir.this = thisDir;
            % ***************************************
            addpath('C:\Users\Charlie\Dropbox\UCLA\FUSION SHARED\CHARLIES RESEARCH\MATLAB\myPlotTools')

            varx = name.varx;
            vary = name.vary;
            varz = name.varz;

            %% Purely hydrodynamic cases
            includePlot.GujStella = true;

            %% Coupled MHD cases
            includePlot.Sergey = false;

            %% Simulation Parameters
            Re = 1000; Ha = 0;

            %% Plot settings
            myPlot.directionPlane = 3;
            myPlot.fractionIntoPlane = 0.5; myPlot.NIntoPlane = 20;
            myPlot.linePosition = 0.5; myPlot.NlinePosition = 5;
            myPlot.rotate = true;
            CL = plotCenterLine();

            if includePlot.GujStella
                data.i = 1;
                myPlot.directionLine = 2;
                data = obj.benchmarkGujStella(data,Re,1);
                name.field = 'Ufield';
                name.file = [varx 'n,' vary 'n,' varz 'n'];
                name.titlex = [varx 'n']; name.titley = [vary 'n']; name.titlez = [varz 'n'];
                data = CL.VecFrom3D(data,myDir,name,true,myPlot);

                data.i = 1;
                myPlot.directionLine = 1;
                data = obj.benchmarkGujStella(data,Re,2);
                name.field = 'Ufield';
                name.file = [varx 'n,' vary 'n,' varz 'n'];
                name.titlex = [varx 'n']; name.titley = [vary 'n']; name.titlez = [varz 'n'];
                data = CL.From3D(data,myDir,name,true,myPlot);
            end

            if includePlot.Sergey
                data.i = 1;
                data = obj.benchmarkSergey(data,myPlot,Re,Ha);
                name.titlex = [varx 'n MOONS']; name.titley = [vary 'n MOONS']; name.titlez = [varz 'n MOONS'];
                CL.VecFrom3D(data,myDir,name,true,myPlot);
            end

        end
        function data = benchmarkSergey(obj,data,myPlot,Re,Ha) %#ok
            %% benchmarkGujStella returns the data for Sergey's simulations

            %% Directory settings
            benPath = 'C:\Users\Charlie\Desktop\benchmark\';
            % sergeyPath = [benPath 'sergey\nonUniformGrid\LDC_Re' num2str(Re) 'Ha' num2str(Ha)];
            sergeyPath = [benPath 'sergey\uniformGrid\LDC_Re' num2str(Re) 'Ha' num2str(Ha)];
            addpath(sergeyPath)
            rawData = false;

            myDir.working = sergeyPath;
            name.field = '';
            name.file = '3D_VELOCITY';
            name.titlex = 'u_{sergey}'; name.titley = 'v_{sergey}'; name.titlez = 'w_{sergey}';
            data = plotCenterLineVecFrom3D(data,myDir,name,false,myPlot);

            if rawData
                temp = name.field;
                name.field = 'Ufield';
                name.file = 'VELOCITY';
                name.titlex = 'u_{sergey}'; name.titley = 'v_{sergey}'; name.titlez = 'w_{sergey}';
                plotCenterPlaneVecFrom3D(myDir,name,[4 5 6],myPlot)
                name.field = temp; clear temp

                % collocated
                name.titlex = 'u_{sergey}'; name.titley = 'v_{sergey}'; name.titlez = 'w_{sergey}';
                name.file = '3D_VELOCITY';
                plotCenterPlaneVecFrom3D(myDir,name,[4 5 6],myPlot)

                myPlot.subPlotNum = 1;
                name.file = 'u_sergey'; name.title = 'u_{sergey}';
                plotCenterPlaneFrom3D(myDir,name,4,myPlot)
                myPlot.subPlotNum = 2;
                name.file = 'v_sergey'; name.title = 'v_{sergey}';
                plotCenterPlaneFrom3D(myDir,name,4,myPlot)
                myPlot.subPlotNum = 3;
                name.file = 'w_sergey'; name.title = 'w_{sergey}';
                plotCenterPlaneFrom3D(myDir,name,4,myPlot)
            end
            % rmpath(sergeyPath)

        end
        function data = benchmarkMOONSGeneral(obj,data,myDir,gd,Re,Ha,name,directionLine) %#ok

            smoothness = 1;
            addpath('C:\Users\Charlie\Dropbox\UCLA\FUSION SHARED\CHARLIES RESEARCH\MATLAB\myPlotTools')
            addpath('C:\Users\Charlie\Dropbox\UCLA\FUSION SHARED\CHARLIES RESEARCH\MATLAB\DATA PROCESSING')

            [x y] = graphDigitizer(myDir,name,gd,smoothness,1);
            T = [x y']; %#ok
            save(['Re' num2str(Re) 'Ha' num2str(Ha) '_' name.file '.dat'],'T','-ascii')

            myPlot.directionLine = directionLine;
            data.x{:,data.i} = x;
            data.var{:,data.i} = y';
            data.(['title' num2str(data.i)]) = name.title;
            data.i = data.i+1;

        end
        function compareMOONS(obj,myDir,myPlot,name)
            %% ***************************************
            % Clear workspace and prescribe path
            % clear;clc;close all;
            % p = mfilename('fullpath');
            % [thisDir] = fileparts(p);
            % chdir(thisDir); myDir.this = thisDir;
            % ***************************************
            addpath('C:\Users\Charlie\Dropbox\UCLA\FUSION SHARED\CHARLIES RESEARCH\MATLAB\myPlotTools')

            %% Simulation Parameters
            Re = 100; Ha = 20;
            addpath(myDir.source)

            %% Plot settings
            myPlot.directionPlane = 3;
            myPlot.fractionIntoPlane = 0.5; myPlot.NIntoPlane = 20;
            myPlot.linePosition = 0.5; myPlot.NlinePosition = 5;
            myPlot.rotate = true;


            %% Re100 Ha10
            if Re==100 && Ha==10
                myDir.working = 'C:\Users\Charlie\Desktop\benchmark\MOONS\finished\uniformGrid\LDCP_Re100Ha10\sergey\';
                data.i = 1;
                gd.xmin = -1.2; gd.xmax = 1.2;
                gd.ymin = -0.4; gd.ymax = 1.2;
                name.file = 'u(0,y,0)'; name.ext = '.png';
                name.field = ''; name.title = 'u_{sergey}';

                data = obj.benchmarkMOONSGeneral(data,myDir,gd,Re,Ha,name,1);
                CL = plotCenterLine();

                name.field = 'Ufield';
                name.varx = 'u';
                name.vary = 'v';
                name.varz = 'w';
                name.file = [name.varx 'ni,' name.vary 'ni,' name.varz 'ni'];
                name.titlex = [name.varx 'ni']; name.titley = [name.vary 'ni']; name.titlez = [name.varz 'ni'];
                myDir.working = myDir.source;
                myDir.saveTo = true;
                data = CL.VecFrom3D(data,myDir,name,true,myPlot);
            end

            %% Re1000 Ha100
            if Re==1000 && Ha==100
                myDir.working = 'C:\Users\Charlie\Desktop\benchmark\MOONS\pending\nonUniformGrid\LDCP_Re1000Ha100\sergey\';
                data.i = 1;
                gd.xmin = -1.2; gd.xmax = 1.2;
                gd.ymin = -0.4; gd.ymax = 1.2;
                name.file = 'u(0,y,0)'; name.ext = '.png';
                name.field = ''; name.title = 'u_{sergey}';

                data = obj.benchmarkMOONSGeneral(data,myDir,gd,Re,Ha,name,1);

                name.field = 'Ufield';
                name.varx = 'u';
                name.vary = 'v';
                name.varz = 'w';
                name.file = [name.varx 'ni,' name.vary 'ni,' name.varz 'ni'];
                name.titlex = [name.varx 'ni']; name.titley = [name.vary 'ni']; name.titlez = [name.varz 'ni'];
                myDir.working = myDir.source;
                myDir.saveTo = true;
                data = CL.VecFrom3D(data,myDir,name,true,myPlot);
            end

            %% Re1000 Ha100
            if Re==1000 && Ha==1000
                myDir.working = 'C:\Users\Charlie\Desktop\benchmark\MOONS\pending\nonUniformGrid\LDCP_Re1000Ha1000\sergey\';
                data.i = 1;
                gd.xmin = -1.2; gd.xmax = 1.2;
                gd.ymin = -0.4; gd.ymax = 1.2;
                name.file = 'u(0,y,0)'; name.ext = '.png';
                name.field = ''; name.title = 'u_{sergey}';

                data = obj.benchmarkMOONSGeneral(data,myDir,gd,Re,Ha,name,1);

                name.field = 'Ufield';
                name.varx = 'u';
                name.vary = 'v';
                name.varz = 'w';
                name.file = [name.varx 'ni,' name.vary 'ni,' name.varz 'ni'];
                name.titlex = [name.varx 'ni']; name.titley = [name.vary 'ni']; name.titlez = [name.varz 'ni'];
                myDir.working = myDir.source;
                myDir.saveTo = true;
                CL.VecFrom3D(data,myDir,name,true,myPlot);
            end


        end
        function data = benchmarkGujStella(obj,data,Re,velocity) %#ok
            %% benchmarkGujStella returns the data for Guj and Stella eta al
            % to compare with centerLine plots.
            % INPUTS:
            %        velocity = (1,2) = (u vs y,v vs x)

            smoothness = 1;

            %% Directory settings
            addpath('C:\Users\Charlie\Dropbox\UCLA\FUSION SHARED\CHARLIES RESEARCH\MATLAB\myPlotTools')
            addpath('C:\Users\Charlie\Dropbox\UCLA\FUSION SHARED\CHARLIES RESEARCH\MATLAB\DATA PROCESSING')

            % Get benchmarking data
            %% 1. Guj and Stella
            benPath = 'C:\Users\Charlie\Desktop\benchmark\';
            dir.working = [benPath 'Guj and Stella\data\'];
            addpath(dir.working)

            if Re == 400
                gd.xmin = -0.4; gd.xmax = 1; gd.ymin = 0; gd.ymax = 1;
                name.file = 'Re400_uvsy'; name.ext = '.png';
                [y1 u1] = graphDigitizer(dir,name,gd,smoothness,2);
                T = [y1 u1']; %#ok
                save('Re400_uvsy_GujStella.dat','T','-ascii')

                gd.xmin = 0; gd.xmax = 1; gd.ymin = -0.5; gd.ymax = 0.3;
                name.file = 'Re400_vvsx'; name.ext = '.png';
                [x1 v1] = graphDigitizer(dir,name,gd,smoothness,1);
                T = [x1 v1']; %#ok
                save('Re400_vvsx_GujStella.dat','T','-ascii')
            elseif Re == 1000
                gd.xmin = -0.4; gd.xmax = 1; gd.ymin = 0; gd.ymax = 1;
                name.file = 'Re1000_uvsy'; name.ext = '.png';
                [y1 u1] = graphDigitizer(dir,name,gd,smoothness,2);
                T = [y1 u1']; %#ok
                save('Re1000_uvsy_GujStella.dat','T','-ascii')

                gd.xmin = 0; gd.xmax = 1; gd.ymin = -0.5; gd.ymax = 0.4;
                name.file = 'Re1000_vvsx'; name.ext = '.png';
                [x1 v1] = graphDigitizer(dir,name,gd,smoothness,1);
                T = [x1' v1]; %#ok
                save('Re1000_vvsx_GujStella.dat','T','-ascii')
            else
                error('Benchmarking for Guj and Stella are for Re=400,1000 only')
            end

            %% uvsy
            if (velocity==1)
                myPlot.directionLine = 2;
                data.i = 1;
                data.x{:,data.i} = u1;
                data.var{:,data.i} = y1';
                data.(['title' num2str(data.i)]) = 'u - Guj and Stella';
                data.i = data.i+1;
            elseif velocity==2
                %% vvsx
                myPlot.directionLine = 1;
                data.i = 1;
                data.x{:,data.i} = x1;
                data.var{:,data.i} = v1';
                data.(['title' num2str(data.i)]) = 'v - Guj and Stella';
                data.i = data.i+1;
            else
                error('Only u vs y and v vs x data exist. Choose velocity = 1 or 2')
            end

            rmpath(dir.working)


        end
    end
end