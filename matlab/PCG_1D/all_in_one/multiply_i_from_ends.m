function x = multiply_i_from_ends(x,F,i)
x(1+i) = x(1+i)*F;
x(end-i) = x(end-i)*F;
end