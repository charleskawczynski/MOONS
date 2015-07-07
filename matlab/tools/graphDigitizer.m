function [x,y] = graphDigitizer(dir,name,griddata,smoothness,direction)
% graphDigitizer digitizes the figure given by the filename
% file in directory dir with extension ext.
%
% The plot must be positioned in the corner with the origin at
% the pixel location (0,0) with one, and ONLY one line.

% name = name.file (string)
%        name.ext  (string)
% dir  = dir.working

% griddata = griddata.xmin
%            griddata.xmax
%            griddata.ymin
%            griddata.ymax
% 
% smoothness > 1
% direction = 1: traverse in x
%             2: traverse in y

% samplingFrequency > 1
% darknessTol = ?
% 
% Example:
% gd = struct; name = struct;
% gd.xmin = -15;   gd.xmax = 10;
% gd.ymin = -0.01; gd.ymax = 0.06;
% name.file = 'square duct - cleaned - ANL 3D Code'; name.ext = '.png';
% [x_ANL3D dP_ANL3D] = graphDigitizer(myDir,name,gd,1,1);

samplingFrequency = 1;
darknessTol = 0.5;

%% DEFINE AXIS RANGE
xmin = griddata.xmin;
xmax = griddata.xmax;
ymin = griddata.ymin;
ymax = griddata.ymax;
logX = false;
logY = false;
eX = true;
eY = true;

%% LOAD IMAGE
A = imread([dir.working name.file name.ext]);

%% ADD IMAGE LAYERS
f = rgb2gray(A);
f = f./max(max(f));
%% ASSIGN VARIABLES
s = size(f);
t = 1;

%% Traverse updown
if direction == 1
    for j = 1:samplingFrequency:s(2)
        coord = find(f(:,j) < darknessTol);
        if (isempty(coord) == 0)
            xf(t) = j;
            yf_up(t) = s(1) - coord(1);
            yf_down(t) = s(1) - coord(end);
            t = t+1;
        end
    end
    for t = 1:length(yf_up)
        yf(t) = (yf_up(t) + yf_down(t))/2;
    end
elseif direction == 2
    for i = 1:samplingFrequency:s(1)
        coord = find(f(i,:) < darknessTol);
        if (isempty(coord)==0)
            xf_up(t) = coord(1);
            xf_down(t) = coord(end);
            yf(t) = s(1)-i;
            t = t+1;
        end
    end
    for t = 1:length(xf_up)
        xf(t) = (xf_up(t) + xf_down(t))/2;
    end
else
    error('A transverse direction was not correctly specified.')
end

%% Normalize coordinates by image size
x = xmin + xf/s(2)*(xmax-xmin);
y = ymin + yf/s(1)*(ymax-ymin);

%% Smooth data
if direction == 1
    y = smooth(y,smoothness);
elseif direction == 2
    x = smooth(x,smoothness);
end
%% Allow for logscale
if logX
    x = log(x);
end
if logY
    y = log(y);
end
% if eX
%     x = exp(x);
% end
% if eY
%     y = exp(y);
% end

%% Save digitized data
if direction == 1
    T = [x' y];
elseif direction == 2
    T = [x y'];
end
save([dir.working 'dataFile_' name.file '.dat'] ,'T','-ascii')


end