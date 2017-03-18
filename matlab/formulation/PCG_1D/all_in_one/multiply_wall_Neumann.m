function u = multiply_wall_Neumann(u,val,x)
if u.DL.is_N
	if x.BCs.bc1.type.Neumann
		u.vals(2) = u.vals(2)*val;
	end
	if x.BCs.bc2.type.Neumann
		u.vals(end-1) = u.vals(end-1)*val;
	end
end
end