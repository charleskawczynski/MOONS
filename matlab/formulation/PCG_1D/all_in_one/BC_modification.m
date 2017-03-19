function b = BC_modification(b,x,A,c,name)
Ax_BC = compute_Ax_BC(A,x,c);
b = assign_wall_Dirichlet(b,0,x);
b.vals = zeroghostpoints(b.vals);
b.vals = b.vals - Ax_BC.vals;
disp(['mean(BC_modification(' name ')) = ' num2str(mean(Ax_BC.vals))])
b = multiply_wall_Neumann(b,0.5,x);
end