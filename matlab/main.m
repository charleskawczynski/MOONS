%% ***************************************
% Clear workspace and prescribe path
clear;clc;close all;
p = mfilename('fullpath');
[thisDir,name,ext] = fileparts(p);
chdir(thisDir); myDir.this = thisDir;
% ***************************************
%% DIRECTORIES
% Root and plot tools
%myDir.MOONSdir = 'C:\Users\Charlie\Dropbox\UCLA\FUSION_SHARED\CHARLIES_RESEARCH\FORTRAN_LIB\';
myDir.MOONSdir = 'C:\Users\Charlie\Documents\GitHub\MOONS\';
myDir.deskTop = 'C:\Users\Charlie\Desktop';
addpath([myDir.MOONSdir 'matlab\tools'])
addpath([myDir.MOONSdir 'matlab\plotResults'])

%% Active directory
myDir.source = 'C:\Users\Charlie\Desktop\MOONS\out\LDC\';
% myDir.source = 'C:\Users\Charlie\Desktop\MOONS\out\LDC\N_32\';
% myDir.source = 'C:\Users\Charlie\Desktop\MOONS\out\LDC\N_64\';
% myDir.source = 'C:\Users\Charlie\Desktop\MOONS\out\LDC\N_128\';
% myDir.source = 'C:\Users\Charlie\Desktop\MOONS\out\LDC\N_256\';
% myDir.source = 'C:\Users\Charlie\Desktop\MOONS - BMCs\BMC_1003 Ha100,Rem100,Re100\';

% myDir.source = 'C:\Users\Charlie\Desktop\MOONS2\out\LDC\';
% myDir.source = 'C:\Users\Charlie\Desktop\benchmark\MOONS\BMC_102_12.17.14\';
% myDir.source = [myDir.MOONSdir '..\MHD\LDC\out\LDC\'];
% myDir.source = [myDir.MOONSdir '\..\MHD\LidDrivenCavity\out\LDCP\'];
%% Nearly perfect and perfect symmetry cases are from
% 9/3/2014 7:55pm
% myDir.source = 'C:\Users\Charlie\Desktop\benchmark\MOONS\finished\uniformGrid\LDC_Re1000Ha0\'; % VERY GOOD SYMMETRY
% myDir.source = 'C:\Users\Charlie\Desktop\benchmark\MOONS\finished\uniformGrid\LDC_Re400Ha0\']; % perfect symmetry (probably bad IO)

% myDir.source = 'C:\Users\Charlie\Desktop\benchmark\MOONS\finished\uniformGrid\LDC_symmetry_cc\LDC\';
% myDir.source = 'C:\Users\Charlie\Desktop\benchmark\MOONS\finished\uniformGrid\LDC_symmetry_n\LDC\';
% myDir.source = 'C:\Users\Charlie\Desktop\benchmark\MOONS\finished\uniformGrid\LDCP_Re100Ha10\';

% myDir.source = [myDir.deskTop '\temp\parallel\LDC\'];
% myDir.source = ['C:\Users\Charlie\Desktop\temp\LDC\'];
% myDir.source = [myDir.deskTop '\benchmark\MOONS\finished\uniformGrid\LDC_Re1000Ha0\'];
% myDir.source = [myDir.deskTop '\benchmark\MOONS\finished\uniformGrid\LDC_Re100Ha0\'];



%% Archived directories
% myDir.source = [myDir.deskTop '\benchmark\MOONS\finished\nonUniformGrid\LDC_Re1000Ha0_beta1.01\'];
% myDir.source = 'C:\Users\Charlie\Desktop\MOONS - BMCs\BMC_102 - good match with sergey\';
% myDir.source = 'C:\Users\Charlie\Desktop\MOONS - BMCs\BMC_104 - good geometry\';
% myDir.source = 'C:\Users\Charlie\Desktop\MOONS - BMCs\BMC_102 - pseudo time step, very low divB\';
% myDir.source = 'C:\Users\Charlie\Desktop\MOONS - BMCs\BMC_106 - sergey and peter\';

myDir.MOONS = myDir.source;

%% Plot Parameters
myPlot = myPlotSettings();

%% Plot Parameters
myPlot.parametric = false;
myPlot.scalarType = 2; % (1,2,3,4) = (contour,contourF,surf,meshc)
myPlot.vectorType = 1; % (1,2,3) = (streamline,quiver,quiver with contour (takes a while...))
myPlot.arrowSize = 3;
myPlot.specifyLevels = true; myPlot.nlevels = 20;
myPlot.directionPlane = 3;
myPlot.fractionIntoPlane = 0.5; myPlot.NIntoPlane = 26;
myPlot.directionLine = 2;
myPlot.linePosition = 0.5; myPlot.NlinePosition = 5;
myPlot.rotate = false;

%% Plots to Include
name = struct;

%% VERY END
% includePlot.parametric = false;

% %% RESULTS
% includePlot.centerLine = false;
includePlot.transientOnly = true;
% includePlot.transientOnly = false;

% %% DEBUGGING/DEVELOPMENT
% includePlot.beforeAfter = false;
% includePlot.sourceB = false;
% includePlot.interpolations = false;
% includePlot.centerLineCheck = false;

% %% VISUALIZATION
% includePlot.streamLinesN = false;
% includePlot.streamLinesC = false;
% includePlot.streamFunction = false;
% includePlot.vorticity = false;

%% TRANSIENT U,B,J DATA
PT = plotTransient();
PT.U(myDir,myPlot,includePlot,name)
% PT.B(myDir,myPlot,includePlot,name)
% PT.J(myDir,myPlot,includePlot,name)

PR = plotResults();

if ~includePlot.transientOnly
    %% U-FIELD
    myPlot.directionPlane = 3;
    includePlot.interior = true; % (T/F) = (fluid domain / total domain)
    includePlot.location = 1; % (1,2,3,4) = (face,cc,node,edge)
    PR.U(myDir,myPlot,includePlot,name)

%     myPlot.directionPlane = 3;
%     includePlot.interior = false; % (T/F) = (fluid domain / total domain)
%     includePlot.location = 3; % (1,2,3,4) = (face,cc,node,edge)
%     PR.U(myDir,myPlot,includePlot,name)

    myPlot.directionPlane = 3;
    includePlot.interior = true; % (T/F) = (fluid domain / total domain)
    includePlot.location = 3; % (1,2,3,4) = (face,cc,node,edge)
    PR.U(myDir,myPlot,includePlot,name)

%     myPlot.directionPlane = 3;
%     includePlot.interior = true; % (T/F) = (fluid domain / total domain)
%     includePlot.location = 3; % (1,2,3,4) = (face,cc,node,edge)
%     PR.U(myDir,myPlot,includePlot,name)

%     myPlot.directionPlane = 1;
%     includePlot.interior = true; % (T/F) = (fluid domain / total domain)
%     includePlot.location = 3; % (1,2,3,4) = (face,cc,node,edge)
%     PR.U(myDir,myPlot,includePlot,name)

%     myPlot.directionPlane = 3;
%     includePlot.interior = true; % (T/F) = (fluid domain / total domain)
%     includePlot.location = 2; % (1,2,3,4) = (face,cc,node,edge)
%     PR.U(myDir,myPlot,includePlot,name)
    
    %% P-FIELD
    % plotMOONS_P(myDir,myPlot,includePlot,name)

    %% B-FIELD
    myPlot.directionPlane = 1; myPlot.rotate = true;
    includePlot.interior = false; % (T/F) = (fluid domain / total domain)
    includePlot.location = 2; % (1,2,3,4) = (face,cc,node,edge)
    PR.B(myDir,myPlot,includePlot,name)


%% SIGMA/MU-FIELD
%     includePlot.location = 3; % (1,2,3,4) = (face,cc,node,edge)
%     PR.sigmaMu(myDir,myPlot,includePlot,name)

    %% BENCHMARK
    % benchmarkMOONS(myDir,myPlot,includePlot,name)
end

BM = plotBenchmarks();
%% QUICK COMPARE
% BM.benchmarkMOONS(myDir,myPlot,name)
% BM.compareMOONS(myDir,myPlot,name)

chdir(thisDir)
