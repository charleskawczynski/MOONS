function [a b c] = setUpSystem(alpha,h,n)
a = [alpha*ones(1,n-1)/h^2 0];
b = [1 -2*alpha*ones(1,n-1)/h^2 1];
c = [0 alpha*ones(1,n-1)/h^2];
end