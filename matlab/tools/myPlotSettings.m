function myPlot = myPlotSettings()

% work = 1; loc = 1; % For work (right screen)
% work = 1; loc = 2; % For work (left screen)
work = 2; loc = 2; % For home (??)
% work = 2; loc = 1; % For home (good)

% figure([x-position y-position x-size y-size]

switch work
    case 1
        myPlot.size = 850; % For work
        switch loc
            case 1
                % RIGHT SCREEN, BOTTOM RIGHT
                myPlot.size = 895; % For work
                myPlot.vector = [9 48 myPlot.size*1.06 myPlot.size];
            case 2
                % LEFT SCREEN, BOTTOM LEFT
                myPlot.size = 960; % For work
                myPlot.vector = [-1673 -15 myPlot.size*1.06 myPlot.size];
            case 3
                % LEFT SCREEN, BOTTOM LEFT (WIDE SCREEN FOR subplot(1,2))
                myPlot.size = 960; % For work
                myPlot.vector = [-1673 -15 myPlot.size*1.06*2 myPlot.size];
            otherwise
        end
    case 2
        switch loc
            case 1
                myPlot.size = 600; % For home
                myPlot.vector = [9 273 myPlot.size*1.06 myPlot.size];
            case 2
                myPlot.size = 600; % For home
                myPlot.vector = [9 50 myPlot.size*1.06 myPlot.size];
            case 3
                myPlot.size = 600; % For home
                myPlot.vector = [9 50 myPlot.size*2.06 myPlot.size];
            otherwise
        end
end

end