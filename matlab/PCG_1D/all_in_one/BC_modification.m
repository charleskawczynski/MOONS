function b = BC_modification(b,x,A,c)
Ax_BC = compute_Ax_BC(A,x,c);
b = assign_wall_Dirichlet(b,0,x);
if x.BCs.bc1.type.Dirichlet && x.is_N
    b.vals(2) = 0;
end
if x.BCs.bc2.type.Dirichlet && x.is_N
    b.vals(end-1) = 0;
end
b.vals = zeroghostpoints(b.vals);
b.vals = b.vals - Ax_BC.vals;
b = multiply_wall_Neumann(b,0.5,x);
end