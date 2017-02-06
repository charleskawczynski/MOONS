function u = assign_single_periodic_wall(u,val,x)
if u.DL.is_N
	if x.BCs.bc1.type.Periodic && x.BCs.bc2.type.Periodic
		u.vals(end-1) = val;
	end
end
end