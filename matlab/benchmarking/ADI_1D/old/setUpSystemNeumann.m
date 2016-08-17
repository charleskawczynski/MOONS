function [a b c] = setUpSystemNeumann(alpha,h,n)
a = alpha*ones(1,n)/h^2;
b = alpha*(-2*ones(1,n+1)/h^2);
c = alpha*ones(1,n)/h^2;
end