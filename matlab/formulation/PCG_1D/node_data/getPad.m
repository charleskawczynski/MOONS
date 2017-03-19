function pad = getPad(BCs)
pad = zeros(2,1);
if BCs.bc1.type.Dirichlet
    pad(1) = 2;
elseif BCs.bc1.type.Neumann
    pad(1) = 1;
else
    error('bad bctype in getPad')
end
if BCs.bc2.type.Dirichlet
    pad(2) = 2;
elseif BCs.bc2.type.Neumann
    pad(2) = 1;
else
    error('bad bctype in getPad')
end
end