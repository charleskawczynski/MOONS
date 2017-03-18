function f = saveOnlyAub(f,u)
if u.is_CC
	f = save_i_from_ends(f,2);
    % f(1+2:end-2) = 0;
    % f(1) = 0;
    % f(end) = 0;
elseif u.is_N
	f = save_i_from_ends(f,3);
    % f(1+3:end-3) = 0;
    % f(1:2) = 0;
    % f(end-1:end) = 0;
else
    error('bad input to saveOnlyAub')
end
end