%% ***************************************
% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); dir.this = thisDir;
% ***************************************
addpath('C:\Users\Charlie\Dropbox\UCLA\FUSION SHARED\CHARLIES RESEARCH\MATLAB\myPlotTools')

%% Plot Parameters
myPlot.parametric = false;
myPlot.scalarType = 2; % (1,2,3) = (contour,contourF,surf)
myPlot.vectorType = 2; % (1,2,3) = (streamline,quiver,quiver with contour (takes a while...))
myPlot.vectorScale = 5;
myPlot.specifyLevels = true; myPlot.nlevels = 20;
myPlot.directionPlane = 3;
myPlot.fractionIntoPlane = 0.5;
myPlot.directionLine = 1;
myPlot.linePosition = 0.5;
myPlot.rotate = true;

% myPlot.size = 850; % For work
myPlot.size = 600; % For home
myPlot.vector = [10 50 myPlot.size*1.06 myPlot.size]; % For work computer


%% Plots to include
includePlot.divU = false;
includePlot.U = false;
includePlot.B = false;
includePlot.J = false;
includePlot.JcrossB = false;
includePlot.centerLineU = true;
includePlot.omegaPsi = false;
includePlot.streamLinesU = false;
includePlot.streamLinesB = false;

%% Directory settings
dir.source = [dir.this '\..\..\MHD\LidDrivenCavity\out\LDCP\'];
addpath([dir.this '\..\DATA PROCESSING\'])

%% Saved directories
Re = 400;
% if Re == 400
%     dir.source = [dir.this '\..\..\MHD\LidDrivenCavity\out\SAVED\benchmarking\LDCP_Re400\'];
% elseif Re == 1000
%     dir.source = [dir.this '\..\..\MHD\LidDrivenCavity\out\SAVED\benchmarking\LDCP_Re1000\'];
% end

%% Set working directory
dir.working = dir.source;
dir.saveTo = true;

% Get benchmarking data
%% 1. Guj and Stella
dir.working = [dir.this '\..\..\MHD\LidDrivenCavity\out\SAVED\benchmarking\Guj and Stella\data\'];
if Re == 400
    gd.xmin = -0.4; gd.xmax = 1; gd.ymin = 0; gd.ymax = 1;
    name.file = 'Re400_uvsy'; name.ext = '.png';
    [y1 u1] = graphDigitizer(dir,name,gd,3,2);
    T = [y1 u1'];
    save('Re400_uvsy_GujStella.dat','T','-ascii')

    gd.xmin = 0; gd.xmax = 1; gd.ymin = -0.5; gd.ymax = 0.3;
    name.file = 'Re400_vvsx'; name.ext = '.png';
    [x1 v1] = graphDigitizer(dir,name,gd,3,1);
    T = [x1 v1'];
    save('Re400_vvsx_GujStella.dat','T','-ascii')
elseif Re == 1000
    gd.xmin = -0.4; gd.xmax = 1; gd.ymin = 0; gd.ymax = 1;
    name.file = 'Re1000_uvsy'; name.ext = '.png';
    [y1 u1] = graphDigitizer(dir,name,gd,5,2);
    T = [y1 u1'];
    save('Re1000_uvsy_GujStella.dat','T','-ascii')

    gd.xmin = 0; gd.xmax = 1; gd.ymin = -0.5; gd.ymax = 0.4;
    name.file = 'Re1000_vvsx'; name.ext = '.png';
    [x1 v1] = graphDigitizer(dir,name,gd,5,1);
    T = [x1' v1];
    save('Re1000_vvsx_GujStella.dat','T','-ascii')
end
%% 2. Wong and Baker
dir.working = [dir.this '\..\..\MHD\LidDrivenCavity\out\SAVED\benchmarking\Wong and Baker\data\'];
if Re == 400
    gd.xmin = -0.4; gd.xmax = 1; gd.ymin = 0; gd.ymax = 1;
    name.file = 'Re400_uvsy'; name.ext = '.png';
    [y2 u2] = graphDigitizer(dir,name,gd,3,2);
    T = [y2 u2'];
    save('Re400_uvsy_wongBaker.dat','T','-ascii')
elseif Re == 1000
    gd.xmin = -0.4; gd.xmax = 1; gd.ymin = 0; gd.ymax = 1;
    name.file = 'Re1000_uvsy'; name.ext = '.png';
    [y2 u2] = graphDigitizer(dir,name,gd,3,2);
    T = [y2 u2'];
    save('Re1000_uvsy_wongBaker.dat','T','-ascii')
end

%% Reset working directory
dir.working = dir.source;

%% Get Nx,Ny,Nz
dir.working = [dir.source 'i1\'];
name.field = 'Ufield'; name.file = 'divU';
addpath([dir.working name.field])
name.title = '\nabla \bullet U';
[x,y,z,Nx,Ny,Nz,var] = getField(dir,name,4);

%% Check cutplane indexes
if myPlot.directionPlane == 1
    N = ceil(1 + myPlot.fractionIntoPlane*(Nx - 1))
elseif myPlot.directionPlane == 2
    N = ceil(1 + myPlot.fractionIntoPlane*(Ny - 1))
elseif myPlot.directionPlane == 3
    N = ceil(1 + myPlot.fractionIntoPlane*(Nz - 1))
end


%% PLOT INDIVIDUAL CASES
if ~myPlot.parametric
    %% CONTOUR PLOTS
    dir.working = [dir.source 'i1\'];

    %% CENTERLINE PLOTS
    dir.working = [dir.source 'i1\'];
    if includePlot.centerLineU
        %% Guj and Stell, uvsy
        myPlot.directionLine = 2;
        data.i = 1;
        data.x{:,data.i} = u1;
        data.var{:,data.i} = y1';
        data.(['title' num2str(data.i)]) = 'u - Guj and Stella';
        data.i = data.i+1;

        %% Wong and Baker, uvsy
        data.x{:,data.i} = u2;
        data.var{:,data.i} = y2';
        data.(['title' num2str(data.i)]) = 'u - Wong and Baker';
        data.i = data.i+1;

        name.field = 'Ufield'; name.file = 'un,vn,wn';
        name.titlex = 'u'; name.titley = 'v'; name.titlez = 'w';
        data.totalTitle = ['u comparison at Re=' num2str(Re)]; data.totalYLabel = 'velocity';
        data = plotCenterLineVecFrom3D(data,dir,name,true,myPlot);
    end

    if includePlot.centerLineU
        myPlot.directionLine = 1;
        data.i = 1;

        %% Guj and Stella, vvsx
        data.x{:,data.i} = x1;
        data.var{:,data.i} = v1';
        data.(['title' num2str(data.i)]) = 'v - Guj and Stella';
        data.i = data.i+1;

        name.field = 'Ufield'; name.file = 'un,vn,wn';
        name.titlex = 'u'; name.titley = 'v'; name.titlez = 'w';
        data.totalTitle = ['v comparison at Re=' num2str(Re)]; data.totalYLabel = 'velocity';
        data = plotCenterLineVecFrom3D(data,dir,name,true,myPlot);
    end



    %% STREAMLINE PLOTS
    if includePlot.streamLinesU
        data.i = 1;
        name.field = 'Ufield'; name.file = 'u,v,w';
        name.titlex = 'u'; name.titley = 'v'; name.titlez = 'w';
        data = plotStreamLinesFrom3D(data,dir,name,true,myPlot);
    end

end


