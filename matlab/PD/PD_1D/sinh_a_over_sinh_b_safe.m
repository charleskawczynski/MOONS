function T = sinh_a_over_sinh_b_safe(a,b)
if length(b)>1
	b
	error('b must be of size 1')
end
tol = 10^-1;
[i_small_enough, i_too_large_left, i_too_large_right] = get_index_extreme(a,tol);

a_small_enough    = a(i_small_enough);
a_too_large_left  = -a(i_too_large_left);
a_too_large_right = a(i_too_large_right);

a_minus_b_right = a_too_large_right - b;
b_minus_a_left  = a_too_large_left  - b;

if ~isempty(i_too_large_left) || ~isempty(i_too_large_right)
    T = zeros(size(a));
    T(i_small_enough) = sinh(a_small_enough)/sinh(b);
    T(i_too_large_right) = exp(a_minus_b_right) - exp(-a_too_large_right)/(exp(b)-exp(-b));
    T(i_too_large_left ) =-exp(b_minus_a_left ) - exp(-a_too_large_left )/(exp(b)-exp(-b));
else
    T = sinh(a)/sinh(b);
end
end