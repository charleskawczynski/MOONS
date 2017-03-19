function x = save_x_BC(x)
if x.DL.is_CC
    x.vals = save_i_from_ends(x.vals,0);
elseif x.DL.is_N
    x.vals = save_i_from_ends(x.vals,1);
end