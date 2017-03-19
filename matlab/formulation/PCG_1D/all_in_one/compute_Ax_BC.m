function Ax_BC = compute_Ax_BC(A,x,c)
x_BC = compute_x_BC(x,c);
Ax_BC.vals = A.explicit.vals*x_BC.vals;
Ax_BC.vals = save_Ax_BC(Ax_BC.vals,x);
end