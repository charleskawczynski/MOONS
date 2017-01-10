function checkBCs(BCs,forceType)
if strcmp(forceType,'cos')
elseif strcmp(forceType,'zero')
elseif strcmp(forceType,'sin')
elseif strcmp(forceType,'exp')
else
    error('Bad forceType input')
end
end