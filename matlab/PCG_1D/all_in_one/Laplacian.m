function Ax = Laplacian(x,c,check_result)
temp = x;
temp = apply_BCs_implicit(temp,c);
Ax = Laplacian_explicit(temp,c);
Ax = multiply_wall_Neumann(Ax,0.5,x);
end