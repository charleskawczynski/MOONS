function x = switchToDual(x)
if x.is_CC
    x.is_CC = false;
    x.is_N = true;
elseif x.is_N
    x.is_N = false;
    x.is_CC = true;
end
end