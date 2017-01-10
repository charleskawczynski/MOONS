function f = modify_forcing(f,u,Aubc)
if u.BCs.bc1.type.Neumann && u.is_N
    f(2) = f(2)*0.5;
end
if u.BCs.bc2.type.Neumann && u.is_N
    f(end-1) = f(end-1)*0.5;
end
if u.BCs.bc1.type.Dirichlet && u.is_N
    f(2) = 0;
end
if u.BCs.bc2.type.Dirichlet && u.is_N
    f(end-1) = 0;
end
f = zeroghostpoints(f);
f = f - Aubc;
end