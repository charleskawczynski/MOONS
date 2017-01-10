function u = multiply_i_from_ends(u,F,i)
u(1+i) = u(1+i)*F;
u(end-i) = u(end-i)*F;
end