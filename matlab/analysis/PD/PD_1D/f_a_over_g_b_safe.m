function T = f_a_over_g_b_safe(f,g,a,b)
if length(b)>1
	b
	error('b must be of size 1')
end
N = 100;
tol = 100;
L = length(a);
a = reshape(a,[1 L]);
i_small_enough = find(abs(a)<=tol);
i_too_large = find(abs(a)>tol);
i_small_enough = 1:length(a);
i_small_enough = [i_small_enough i_too_large];
a_too_large = a(i_too_large);
a_small_enough = a(i_small_enough);

if b>tol || max(abs(a))>tol
    T = zeros(size(a));
    T(i_small_enough) = f(a_small_enough)/g(b);
    T(i_too_large)    = 1;
else
    T = sinh(a)/cosh(b);
end
end