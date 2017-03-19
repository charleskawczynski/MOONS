function u = saveub(u)
if u.is_CC
    % u.vals(2:end-1) = 0;
    u.vals = save_i_from_ends(u.vals,0);
elseif u.is_N
	% Leaves u(2) and u(end-1) untouched
    u.vals = save_i_from_ends(u.vals,1);
    % u.vals(3:end-2) = 0;
    % u.vals(1) = 0;
    % u.vals(end) = 0;
end