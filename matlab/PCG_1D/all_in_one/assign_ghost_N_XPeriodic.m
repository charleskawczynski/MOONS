function u = assign_ghost_N_XPeriodic(u,val)
if (u.is_N)
	u.vals(1) = val;
	u.vals(end) = val;
end
end