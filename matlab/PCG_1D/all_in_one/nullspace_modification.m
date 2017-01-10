function b = nullspace_modification(b,BCs,name)
if BCs.bc1.type.Neumann && BCs.bc2.type.Neumann
    b.c.vals = subtract_mean(b.c.vals);
    b.n.vals = subtract_mean(b.n.vals);
    disp(['mean( ' name ' ) = ' num2str(mean(b.c.vals))])
    disp(['mean( ' name ' ) = ' num2str(mean(b.n.vals))])
end
end