function Ax = Laplacian(x,c)
temp = x;
temp = apply_BCs_implicit(temp,c);
Ax = Laplacian_explicit(temp,c);
Ax = multiply_wall_Neumann(Ax,0.5,x);
Ax = assign_single_periodic_wall(Ax,0.5,x);
end