function f = save_Ax_BC(f,x)
if x.is_CC
	f = save_i_from_ends(f,1);
elseif x.is_N
	f = save_i_from_ends(f,2);
else
    error('bad input to save_Ax_BC')
end
end