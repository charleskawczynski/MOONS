function u = assign_wall_Dirichlet(u,val,x)
if x.DL.is_N
	if x.BCs.bc1.type.Dirichlet
		u.vals(2) = val;
	end
	if x.BCs.bc2.type.Dirichlet
		u.vals(end-1) = val;
	end
end
end