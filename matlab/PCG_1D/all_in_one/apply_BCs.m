function u = apply_BCs(u,c)
f = u.vals;
ub1 = u.BCs.bc1.val;
ub2 = u.BCs.bc2.val;
if u.DL.is_N
    if (u.BCs.bc1.type.Dirichlet); f(2) = ub1; f(1) = 2*f(2) - f(3);
    elseif (u.BCs.bc1.type.Neumann); f(1) = f(3) - 2*ub1*c.dh;
    elseif (u.BCs.bc1.type.Periodic && ~u.BCs.bc2.type.Periodic); f(1) = f(end-2);
    else
    end
    if (u.BCs.bc2.type.Dirichlet); f(end-1) = ub2; f(end) = 2*f(end-1) - f(end-2);
    elseif (u.BCs.bc2.type.Neumann); f(end) = f(end-2) + 2*ub2*c.dh;
    elseif (u.BCs.bc2.type.Periodic && ~u.BCs.bc1.type.Periodic); f(end) = f(3);
    else
    end
    if (u.BCs.bc1.type.Periodic && u.BCs.bc2.type.Periodic)
        f(1) = f(end-2);
        f(end-1) = f(2);
        f(end) = 0;
    end
elseif u.DL.is_CC
    if (u.BCs.bc1.type.Dirichlet); f(1) = 2*ub1 - f(2);
    elseif (u.BCs.bc1.type.Neumann); f(1) = f(2) - ub1*c.dh;
    elseif (u.BCs.bc1.type.Periodic); f(1) = f(end-1);
    else; error('Bad bc1 for CC in apply_BCs')
    end
    if (u.BCs.bc2.type.Dirichlet); f(end) = 2*ub2 - f(end-1);
    elseif (u.BCs.bc2.type.Neumann); f(end) = f(end-1) + ub2*c.dh;
    elseif (u.BCs.bc2.type.Periodic); f(end) = f(2);
    else; error('Bad bc2 for CC in apply_BCs')
    end
else; error('Bad data type input into apply_BCs')
end
u.vals = f;
end