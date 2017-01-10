function u = apply_BCs_implicit(u,c)
f = u.vals;
ub1 = 0;
ub2 = 0;
if u.is_N
    if (u.BCs.bc1.type.Dirichlet); f(2) = ub1; f(1) = 2*f(2) - f(3);
    elseif (u.BCs.bc1.type.Neumann); f(1) = f(3) - 2*ub1*c.dh;
    else; error('Bad bc1 for N in apply_BCs')
    end
    if (u.BCs.bc2.type.Dirichlet); f(end-1) = ub2; f(end) = 2*f(end-1) - f(end-2);
    elseif (u.BCs.bc2.type.Neumann); f(end) = f(end-2) + 2*ub2*c.dh;
    else; error('Bad bc2 for N in apply_BCs')
    end
elseif u.is_CC
    if (u.BCs.bc1.type.Dirichlet); f(1) = 2*ub1 - f(2);
    elseif (u.BCs.bc1.type.Neumann); f(1) = f(2) - ub1*c.dh;
    else; error('Bad bc1 for CC in apply_BCs')
    end
    if (u.BCs.bc2.type.Dirichlet); f(end) = 2*ub2 - f(end-1);
    elseif (u.BCs.bc2.type.Neumann); f(end) = f(end-1) + ub2*c.dh;
    else; error('Bad bc2 for CC in apply_BCs')
    end
else; error('Bad data type input into apply_BCs')
end
u.vals = f;
end