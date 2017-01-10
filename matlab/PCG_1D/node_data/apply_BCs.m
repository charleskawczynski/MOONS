function u = apply_BCs(u,c,explicit,centered)
f = u.vals;
ub1 = u.BCs.bc1.val;
ub2 = u.BCs.bc2.val;
if u.is_N
    if (u.BCs.bc1.type.Dirichlet)
        if centered
            % Un-modified stencil
        elseif explicit
            f(2) = ub1;
            f(1) = 2*f(2) - f(3);
        else
            f(2) = 0; f(1) = 2*f(2) - f(3);
        end
    elseif (u.BCs.bc1.type.Neumann)
%         f(1) = f(3) - 2*ub1*c.dh; % Without dividing by 2
        f(1) = f(2) - 2*ub1*c.dh; % Pure Neumann
    else
        error('Bad bc1 for N in apply_BCs')
    end
    if (u.BCs.bc2.type.Dirichlet)
        if centered
            % Un-modified stencil
        elseif explicit
            f(end-1) = ub2;
            f(end) = 2*f(end-1) - f(end-2);
        else
            f(end-1) = 0; f(end) = 2*f(end-1) - f(end-2);
        end
    elseif (u.BCs.bc2.type.Neumann)
%         f(end) = f(end-2) + 2*ub2*c.dh; % Without dividing by 2
        f(end) = f(end-1) + 2*ub2*c.dh;
    else
        error('Bad bc2 for N in apply_BCs')
    end
elseif u.is_CC
    if (u.BCs.bc1.type.Dirichlet)
        if centered
            % Un-modified stencil
        elseif explicit
            f(1) = 2*ub1 - f(2);
        else
            f(1) = - f(2);
        end
    elseif (u.BCs.bc1.type.Neumann)
        f(1) = f(2) - ub1*c.dh;
    else
        error('Bad bc1 for CC in apply_BCs')
    end
    if (u.BCs.bc2.type.Dirichlet)
        if centered
            % Un-modified stencil
        elseif explicit
            f(end) = 2*ub2 - f(end-1);
        else
            f(end) = - f(end-1);
        end
    elseif (u.BCs.bc2.type.Neumann)
        f(end) = f(end-1) + ub2*c.dh;
    else
        error('Bad bc2 for CC in apply_BCs')
    end
else
    error('Bad data type input into apply_BCs')
end
u.vals = f;
end