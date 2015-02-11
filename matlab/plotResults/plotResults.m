classdef plotResults
    methods
        function obj = plotResults(obj)
        end
        function U(obj,myDir,myPlot,includePlot,name)
            %% ***************************************
            % Clear workspace and prescribe path
            % clear;clc;close all;
            p = mfilename('fullpath');
            [thisDir] = fileparts(p);
            chdir(thisDir); myDir.this = thisDir;
            % ***************************************
            % addpath(myDir.MOONSdir)
            addpath([myDir.MOONSdir 'matlab\plotResults'])
            CP = plotCenterPlane();

            name.field = 'Ufield';
            name.varx = 'u';name.vary = 'v';name.varz = 'w';
            myPlot.m = 2; myPlot.n = 2;

            %% Set working directory
            myDir.working = myDir.source;
            myDir.saveTo = true;
            myDir.saveExt = 'figs';

            %% CUT PLANES

            % FACE DATA
            switch includePlot.location
                case 1 % face
                    if includePlot.interior
                        myDir.saveTo = false;
                        name.file = [name.varx 'fi']; name.title = [name.varx '_{fi}']; myPlot.subPlotNum = 1;
                        CP.From3D(myDir,name,4,myPlot)
                        name.file = [name.vary 'fi']; name.title = [name.vary '_{fi}']; myPlot.subPlotNum = 2;
                        CP.From3D(myDir,name,4,myPlot)
                        name.file = [name.varz 'fi']; name.title = [name.varz '_{fi}']; myPlot.subPlotNum = 3;
                        CP.From3D(myDir,name,4,myPlot)
                        myPlot.subPlotNum = 4;
                        myDir.saveTo = true;
                        name.saveFile = ['centerPlane_' name.field(1) 'fi_div' name.field(1) ];
                        name.file = ['div' name.field(1) 'ci']; name.title = ['\nabla \bullet ' name.field(1) ' (from interior face)'];
                        CP.From3D(myDir,name,4,myPlot)
                    else
                        myDir.saveTo = false;
                        name.file = [name.varx 'ft']; name.title = [name.varx '_{ft}']; myPlot.subPlotNum = 1;
                        CP.From3D(myDir,name,4,myPlot)
                        name.file = [name.vary 'ft']; name.title = [name.vary '_{ft}']; myPlot.subPlotNum = 2;
                        CP.From3D(myDir,name,4,myPlot)
                        name.file = [name.varz 'ft']; name.title = [name.varz '_{ft}']; myPlot.subPlotNum = 3;
                        CP.From3D(myDir,name,4,myPlot)
                        myPlot.subPlotNum = 4;
                        myDir.saveTo = true;
                        name.saveFile = ['centerPlane_' name.field(1) 'ft_div' name.field(1)];
                        name.file = ['div' name.field(1) 'ct']; name.title = ['\nabla \bullet ' name.field(1) ' (from total face)'];
                        CP.From3D(myDir,name,4,myPlot)
                    end
                case 2 % cc
                    if includePlot.interior
                        name.titlex = [name.varx '_{ci}']; name.titley = [name.vary '_{ci}']; name.titlez = [name.varz '_{ci}'];
                        name.file = [name.varx 'ci,' name.vary 'ci,' name.varz 'ci'];
                        CP.VecFrom3D(myDir,name,[4 5 6],myPlot)
                        myPlot.subPlotNum = 4;
                        name.saveFile = ['centerPlane_' name.field(1) 'ci_div' name.field(1)];
                        name.file = ['div' name.field(1) 'ci']; name.title = ['\nabla \bullet ' name.field(1) ' (cci)'];
                        CP.From3D(myDir,name,4,myPlot)
                    else
                        name.titlex = [name.varx '_{ct}']; name.titley = [name.vary '_{ct}']; name.titlez = [name.varz '_{ct}'];
                        name.file = [name.varx 'ct,' name.vary 'ct,' name.varz 'ct'];
                        CP.VecFrom3D(myDir,name,[4 5 6],myPlot)
                        myPlot.subPlotNum = 4;
                        name.saveFile = ['centerPlane_' name.field(1) 'ct_div' name.field(1)];
                        name.file = ['div' name.field(1) 'ct']; name.title = ['\nabla \bullet ' name.field(1) ' (cct)'];
                        CP.From3D(myDir,name,4,myPlot)
                    end
                case 3 % nodes
                    if includePlot.interior
                        name.titlex = [name.varx '_{ni}']; name.titley = [name.vary '_{ni}']; name.titlez = [name.varz '_{ni}'];
                        name.file = [name.varx 'ni,' name.vary 'ni,' name.varz 'ni'];
                        CP.VecFrom3D(myDir,name,[4 5 6],myPlot)
                        myPlot.subPlotNum = 4;
                        name.saveFile = ['centerPlane_' name.field(1) 'ni_div' name.field(1)];
                        name.file = ['div' name.field(1) 'ni']; name.title = ['\nabla \bullet ' name.field(1) ' (ni)'];
                        CP.From3D(myDir,name,4,myPlot)
                    else
                        name.titlex = [name.varx '_{nt}']; name.titley = [name.vary '_{nt}']; name.titlez = [name.varz '_{nt}'];
                        name.file = [name.varx 'nt,' name.vary 'nt,' name.varz 'nt'];
                        CP.VecFrom3D(myDir,name,[4 5 6],myPlot)
                        myPlot.subPlotNum = 4;
                        name.saveFile = ['centerPlane_' name.field(1) 'nt_div' name.field(1)];
                        name.file = ['div' name.field(1) 'nt']; name.title = ['\nabla \bullet ' name.field(1) ' (nt)'];
                        CP.From3D(myDir,name,4,myPlot)
                    end
                case 4 % edges, not sure if the following makes sense...
                    if includePlot.interior
                        name.titlex = [name.varx '_{ei}']; name.titley = [name.vary '_{ei}']; name.titlez = [name.varz '_{ei}'];
                        name.file = [name.varx 'ei,' name.vary 'ei,' name.varz 'ei'];
                        CP.VecFrom3D(myDir,name,[4 5 6],myPlot)
                        myPlot.subPlotNum = 4;
                        name.saveFile = ['centerPlane_' name.field(1) 'ei_div' name.field(1)];
                        name.file = ['div' name.field(1) 'ei']; name.title = ['\nabla \bullet ' name.field(1) ' (ei)'];
                        CP.From3D(myDir,name,4,myPlot)
                    else
                        name.titlex = [name.varx '_{et}']; name.titley = [name.vary '_{et}']; name.titlez = [name.varz '_{et}'];
                        name.file = [name.varx 'et,' name.vary 'et,' name.varz 'et'];
                        CP.VecFrom3D(myDir,name,[4 5 6],myPlot)
                        myPlot.subPlotNum = 4;
                        name.saveFile = ['centerPlane_' name.field(1) 'et_div' name.field(1)];
                        name.file = ['div' name.field(1) 'et']; name.title = ['\nabla \bullet ' name.field(1) ' (et)'];
                        CP.From3D(myDir,name,4,myPlot)
                    end
            end

        end
        function B(obj,myDir,myPlot,includePlot,name)
            %% ***************************************
            % Clear workspace and prescribe path
            % clear;clc;close all;
            p = mfilename('fullpath');
            [thisDir] = fileparts(p);
            chdir(thisDir); myDir.this = thisDir;
            % ***************************************
            CP = plotCenterPlane();

            name.field = 'Bfield';
            name.varx = 'Bx';name.vary = 'By';name.varz = 'Bz';
            myPlot.m = 2; myPlot.n = 2;

            %% Set working directory
            myDir.working = myDir.source;
            myDir.saveTo = true;
            myDir.saveExt = 'figs';
            myDir.working = myDir.source;

            %% CUT PLANES

            % FACE DATA
            switch includePlot.location
                case 1 % face
                    if includePlot.interior
                        myDir.saveTo = false;
                        name.file = [name.varx 'fi']; name.title = [name.varx '_{fi}']; myPlot.subPlotNum = 1;
                        CP.From3D(myDir,name,4,myPlot)
                        name.file = [name.vary 'fi']; name.title = [name.vary '_{fi}']; myPlot.subPlotNum = 2;
                        CP.From3D(myDir,name,4,myPlot)
                        name.file = [name.varz 'fi']; name.title = [name.varz '_{fi}']; myPlot.subPlotNum = 3;
                        CP.From3D(myDir,name,4,myPlot)
                        myPlot.subPlotNum = 4;
                        myDir.saveTo = true;
                        name.saveFile = ['centerPlane_' name.field(1) 'fi_div' name.field(1) ];
                        name.file = ['div' name.field(1) 'ci']; name.title = ['\nabla \bullet ' name.field(1) ' (from interior face)'];
                        CP.From3D(myDir,name,4,myPlot)
                    else
                        myDir.saveTo = false;
                        name.file = [name.varx 'ft']; name.title = [name.varx '_{ft}']; myPlot.subPlotNum = 1;
                        CP.From3D(myDir,name,4,myPlot)
                        name.file = [name.vary 'ft']; name.title = [name.vary '_{ft}']; myPlot.subPlotNum = 2;
                        CP.From3D(myDir,name,4,myPlot)
                        name.file = [name.varz 'ft']; name.title = [name.varz '_{ft}']; myPlot.subPlotNum = 3;
                        CP.From3D(myDir,name,4,myPlot)
                        myPlot.subPlotNum = 4;
                        myDir.saveTo = true;
                        name.saveFile = ['centerPlane_' name.field(1) 'ft_div' name.field(1)];
                        name.file = ['div' name.field(1) 'ct']; name.title = ['\nabla \bullet ' name.field(1) ' (from total face)'];
                        CP.From3D(myDir,name,4,myPlot)
                    end
                case 2 % cc
                    if includePlot.interior
                        name.titlex = [name.varx '_{ci}']; name.titley = [name.vary '_{ci}']; name.titlez = [name.varz '_{ci}'];
                        name.file = [name.varx 'ci,' name.vary 'ci,' name.varz 'ci'];
                        CP.VecFrom3D(myDir,name,[4 5 6],myPlot)
                        myPlot.subPlotNum = 4;
                        name.saveFile = ['centerPlane_' name.field(1) 'ci_div' name.field(1)];
                        name.file = ['div' name.field(1) 'ci']; name.title = ['\nabla \bullet ' name.field(1) ' (cci)'];
                        CP.From3D(myDir,name,4,myPlot)
                    else
                        name.titlex = [name.varx '_{ct}']; name.titley = [name.vary '_{ct}']; name.titlez = [name.varz '_{ct}'];
                        name.file = [name.varx 'ct,' name.vary 'ct,' name.varz 'ct'];
                        CP.VecFrom3D(myDir,name,[4 5 6],myPlot)
                        myPlot.subPlotNum = 4;
                        name.saveFile = ['centerPlane_' name.field(1) 'ct_div' name.field(1)];
                        name.file = ['div' name.field(1) 'ct']; name.title = ['\nabla \bullet ' name.field(1) ' (cct)'];
                        CP.From3D(myDir,name,4,myPlot)
                    end
                case 3 % nodes
                    if includePlot.interior
                        name.titlex = [name.varx '_{ni}']; name.titley = [name.vary '_{ni}']; name.titlez = [name.varz '_{ni}'];
                        name.file = [name.varx 'ni,' name.vary 'ni,' name.varz 'ni'];
                        CP.VecFrom3D(myDir,name,[4 5 6],myPlot)
                        myPlot.subPlotNum = 4;
                        name.saveFile = ['centerPlane_' name.field(1) 'ni_div' name.field(1)];
                        name.file = ['div' name.field(1) 'ni']; name.title = ['\nabla \bullet ' name.field(1) ' (ni)'];
                        CP.From3D(myDir,name,4,myPlot)
                    else
                        name.titlex = [name.varx '_{nt}']; name.titley = [name.vary '_{nt}']; name.titlez = [name.varz '_{nt}'];
                        name.file = [name.varx 'nt,' name.vary 'nt,' name.varz 'nt'];
                        CP.VecFrom3D(myDir,name,[4 5 6],myPlot)
                        myPlot.subPlotNum = 4;
                        name.saveFile = ['centerPlane_' name.field(1) 'nt_div' name.field(1)];
                        name.file = ['div' name.field(1) 'nt']; name.title = ['\nabla \bullet ' name.field(1) ' (nt)'];
                        CP.From3D(myDir,name,4,myPlot)
                    end
                case 4 % edges, not sure if the following makes sense...
                    if includePlot.interior
                        name.titlex = [name.varx '_{ei}']; name.titley = [name.vary '_{ei}']; name.titlez = [name.varz '_{ei}'];
                        name.file = [name.varx 'ei,' name.vary 'ei,' name.varz 'ei'];
                        CP.VecFrom3D(myDir,name,[4 5 6],myPlot)
                        myPlot.subPlotNum = 4;
                        name.saveFile = ['centerPlane_' name.field(1) 'ei_div' name.field(1)];
                        name.file = ['div' name.field(1) 'ei']; name.title = ['\nabla \bullet ' name.field(1) ' (ei)'];
                        CP.From3D(myDir,name,4,myPlot)
                    else
                        name.titlex = [name.varx '_{et}']; name.titley = [name.vary '_{et}']; name.titlez = [name.varz '_{et}'];
                        name.file = [name.varx 'et,' name.vary 'et,' name.varz 'et'];
                        CP.VecFrom3D(myDir,name,[4 5 6],myPlot)
                        myPlot.subPlotNum = 4;
                        name.saveFile = ['centerPlane_' name.field(1) 'et_div' name.field(1)];
                        name.file = ['div' name.field(1) 'et']; name.title = ['\nabla \bullet ' name.field(1) ' (et)'];
                        CP.From3D(myDir,name,4,myPlot)
                    end
            end

        end
        function P(obj,myDir,myPlot,includePlot,name)
            %% ***************************************
            % Clear workspace and prescribe path
            % clear;clc;close all;
            p = mfilename('fullpath');
            [thisDir] = fileparts(p);
            chdir(thisDir); myDir.this = thisDir;
            % ***************************************
            CP = plotCenterPlane();

            name.field = 'Ufield';
            name.varx = 'u';name.vary = 'v';name.varz = 'w';
            myPlot.m = 1; myPlot.n = 1;

            %% Set working directory
            myDir.working = myDir.source;
            myDir.saveTo = true;
            myDir.saveExt = 'figs';
            myDir.working = [myDir.source 'i1\'];

            % PRESSURE
            myPlot.subPlotNum = 1;
            name.file = 'pci'; name.title = 'Pressure';
            CP.From3D(myDir,name,4,myPlot)

        end
        function sigmaMu(obj,myDir,myPlot,includePlot,name)
            %% ***************************************
            % Clear workspace and prescribe path
            % clear;clc;close all;
            p = mfilename('fullpath');
            [thisDir] = fileparts(p);
            chdir(thisDir); myDir.this = thisDir;
            % ***************************************
            CP = plotCenterPlane();

            name.field = 'Bfield';
            name.varx = 'Bx';name.vary = 'By';name.varz = 'Bz';
            myPlot.m = 2; myPlot.n = 2;

            %% Set working directory
            myDir.working = myDir.source;
            myDir.saveTo = true;

            %% MATERIAL DATA
            % CC DATA
            switch includePlot.location % (1,2,3,4) = (face,cc,node,edge)
                case 2
                    temp = name.field;
                    name.field = 'material';

                    myPlot.subPlotNum = 1;
                    name.file = 'sigmac'; name.title = '\sigma_{cc}';
                    CP.From3D(myDir,name,4,myPlot)

                    myPlot.subPlotNum = 2;
                    name.file = 'muc'; name.title = '\mu_{cc}';
                    CP.From3D(myDir,name,4,myPlot)

                    name.field = temp; clear temp
                case 3
                    temp = name.field;
                    name.field = 'material';

                    myPlot.subPlotNum = 1;
                    name.file = 'sigman'; name.title = '\sigma_{n}';
                    CP.From3D(myDir,name,4,myPlot)

                    myPlot.subPlotNum = 2;
                    name.file = 'mun'; name.title = '\mu_{n}';
                    CP.From3D(myDir,name,4,myPlot)

                    name.field = temp; clear temp
            end

        end
    end
end