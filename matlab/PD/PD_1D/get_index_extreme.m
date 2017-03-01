function [i_small_enough, i_too_large_left, i_too_large_right] = get_index_extreme(a,tol)
i_all = 1:length(a);
i_too_large_right = find(exp(-a)<tol);
i_too_large_left  = find(exp(+a)<tol);
i_small_enough = setdiff(i_all,[i_too_large_left i_too_large_right]);
end