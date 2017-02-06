function x = switchToDual(x)
if x.DL.is_CC
    x.DL.is_CC = false;
    x.DL.is_N = true;
elseif x.DL.is_N
    x.DL.is_N = false;
    x.DL.is_CC = true;
end
end