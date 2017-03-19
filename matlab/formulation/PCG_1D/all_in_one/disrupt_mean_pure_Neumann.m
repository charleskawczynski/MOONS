function b = disrupt_mean_pure_Neumann(b,BCs)
if (BCs.bc1.type.Neumann && BCs.bc2.type.Neumann) ||...
   (BCs.bc1.type.Periodic && BCs.bc2.type.Periodic)
	b.c.vals = b.c.vals+1;
	b.n.vals = b.n.vals+1;
end
end