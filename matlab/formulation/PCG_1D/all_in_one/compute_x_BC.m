function x_BC = compute_x_BC(x,c)
x_BC = x;
x_BC.vals(:) = 0;
x_BC = apply_BCs(x_BC,c);
x_BC = save_x_BC(x_BC);
end