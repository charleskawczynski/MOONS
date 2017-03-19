function f = save_Ax_BC(f,x)
if x.DL.is_CC
	f = save_i_from_ends(f,1);
elseif x.DL.is_N
	f = save_i_from_ends(f,2);
else
    error('bad input to save_Ax_BC')
end
end