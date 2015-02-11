classdef myImporter
    methods
        
        function [data nheaderLines] = myImport(name)

            delimiter = ' ';
            try
                s = fileread([name.file '.dat']);
            catch
                error(['Error opening ' name.file '.dat'])
            end

            if ~isempty(regexp(s,'NaN','match'))
                error(['Problem loading ' name.file '. NaN exists.'])
            end
            if ~isempty(regexp(s,'Infinity','match'))
                error(['Problem loading ' name.file '. Infinity exists.'])
            end

            [data delimiterOut nheaderLines] = importdata([name.file '.dat'],delimiter);

            try
                temp = data.data;
                var = temp(:,1); % This line triggers the error.
                if size(var)==1
                    var = temp(:,2); % This line triggers the error.
                end
            catch err
                try
                    % This section is used when the data is stored in data.textdata
                    % when letters appear in the .dat file (e.g. 5.23E-2.0).
                    % A different approach needs to be used to extract the data.

                    % starti must be the first row where the data starts.
                    t = sprintf('%s*', data.textdata{nheaderLines:end,:});
                    N = sscanf(t, '%f*');
                    s = size(data.textdata(nheaderLines:end,:));
                    temp = reshape(N,s);
                catch err
                    % This takes into account the possibility that no headers
                    % exist and the import function automatically imports without
                    % any furthur reshaping etc.
                    temp = data;
                end
            end
            data = temp;

        end
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
    end
end
