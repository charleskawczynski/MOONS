function Ax_BC = compute_Ax_BC_MF(operator_explicit,x,c)
x_BC = compute_x_BC(x,c);
Ax_BC = operator_explicit(x_BC,c);
Ax_BC.vals = save_Ax_BC(Ax_BC.vals,x);
end