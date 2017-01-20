function x = save_x_BC(x)
if x.is_CC
    x.vals = save_i_from_ends(x.vals,0);
elseif x.is_N
    x.vals = save_i_from_ends(x.vals,1);
end