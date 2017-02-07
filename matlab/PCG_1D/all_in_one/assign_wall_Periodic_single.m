function u = assign_wall_Periodic_single(u,val,BCs)
if u.DL.is_N
	if BCs.bc1.type.Periodic && BCs.bc2.type.Periodic
		u.vals(end-1) = val;
		u.vals(end) = val;
		u.vals(1) = val;
	end
end
end