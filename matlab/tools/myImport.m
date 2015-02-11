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