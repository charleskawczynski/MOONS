function u = assign_ghost_XPeriodic(u,val)
u.vals(1) = val;
u.vals(end) = val;
end