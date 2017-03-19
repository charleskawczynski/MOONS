function [f,x,y] = graph_digitizer(dir,name,griddata,ind,props)
% graphDigitizer digitizes the figure given by the filename
% file in directory dir with extension ext.
%
% The plot must be positioned in the corner with the origin at
% the pixel location (0,0) with one, and ONLY one line.

% smoothness = ss.smoothness;
% direction = ss.direction;
% samplingFrequency = ss.samplingFrequency;
% darknessTol = ss.darknessTol;

% smoothness = ss.smoothness;
% direction = ss.direction;
samplingFrequency = props.samplingFrequency;
darknessTol = props.darknessTol;
smoothness = props.smoothness;
direction = props.direction;

i_start = ind.i_start;
i_end   = ind.i_end;
j_start = ind.j_start;
j_end   = ind.j_end;

%% DEFINE AXIS RANGE
xmin = griddata.xmin;
xmax = griddata.xmax;
ymin = griddata.ymin;
ymax = griddata.ymax;
logX = false;
logY = false;

%% LOAD IMAGE
A = imread([dir.working name.file name.ext]);
%% ADD IMAGE LAYERS
% f = 0;
% for j = 1:3
%     f = f + A(:,:,j);
% end
f = rgb2gray(A);
% f = f./max(max(f));
f = f(j_start:end+1-j_end,i_start:end+1-i_end);

%% ASSIGN VARIABLES
r = length(f(:,1));
c = length(f(1,:));
s = size(f);
t = 1;
% xf = [];
% yf = [];
% yf_up = [];
% yf_down = [];

%% Traverse updown
if direction == 1
    for j = 1:samplingFrequency:s(2)
        coord = find(f(:,j) < darknessTol);
        if (isempty(coord) == 0)
            xf(t) = j;
            yf_up(t) = r - coord(1);
            yf_down(t) = r - coord(end);
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
            xf(t) = coord(1);
            yf(t) = r-i;
            t = t+1;
        end
    end
else
    error('A transverse direction was not correctly specified.')
end

%% Normalize coordinates by image size
x = xmin + xf/c*(xmax-xmin);
y = ymin + yf/r*(ymax-ymin);

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

%% Save digitized data
if direction == 1
    T = [x' y];
elseif direction == 2
    T = [x y'];
end
save([dir.working 'dataFile_' name.file '.dat'] ,'T','-ascii')

f_plot = f(end:-1:1,:);
figure
subplot(2,1,1)
contour(f_plot)
title('Contour of image input')
xlabel('x')
xlabel('y')

y_plot = y;
x_plot = x;
subplot(2,1,2)
plot(x_plot,y_plot)
axis([xmin xmax ymin ymax])
title('Digitized data')
xlabel('x')
xlabel('y')

end