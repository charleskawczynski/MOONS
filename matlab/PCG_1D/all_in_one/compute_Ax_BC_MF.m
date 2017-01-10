function Ax_BC = compute_Ax_BC_MF(operator,x,c)
x_BC = compute_x_BC(x,c);
Ax_BC = operator(x_BC,c);
Ax_BC.vals = save_Ax_BC(Ax_BC.vals,x);
end