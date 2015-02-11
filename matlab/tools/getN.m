function [Nx Ny Nz] = getN(data,nheaderLines)

% The following procedure is used to determine the dimensions of the
% incoming data. It has not yet been tested.
s = size(data);
HIMAG_output = false;

if HIMAG_output
    % NOT YET WORKING...
    if s(2) > 3 % 3D field
        x = data(:,1);
        i = find(x == x(1));
        Nx = i(2)-2;
        temp = diff(data(:,2));
        i = find(temp);
        y = data(i,2);
        j = find(y == y(1));
        Ny = j(2)-2;
        Nz = s(1)/Nx/Ny;
    elseif s(2) == 3 % 2D scalar field
        x = data(:,1);
        i = find(x == x(1));
        Nx = i(2)-1;
        temp = diff(data(:,2));
        i = find(temp);
        Ny = i(1);
        Nz = 0;
    end
else
    if s(2) > 3 % 3D field
        x = data(:,1);
        i = find(x == x(1));
        Nx = i(2)-1;
        temp = diff(data(:,2));
        i = find(temp);
        y = data(i,2);
        j = find(y == y(1));
        Ny = j(2)-1;
        Nz = s(1)/Nx/Ny;
    elseif s(2) == 3 % 2D scalar field
        x = data(:,1);
        i = find(x == x(1));
        Nx = i(2)-1;
        temp = diff(data(:,2));
        i = find(temp);
        Ny = i(1);
        Nz = 0;
    end

end
end
