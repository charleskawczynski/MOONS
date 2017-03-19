function b = nullspace_modification(b,BCs,name)
if BCs.bc1.type.Neumann && BCs.bc2.type.Neumann
    b.c.vals = subtract_mean(b.c.vals);
    b.n.vals = subtract_mean(b.n.vals);
    temp = mean(b.c.vals);
    disp(['mean( ' name ' ,c) = ' num2str(temp)])
    temp = mean(b.n.vals);
    disp(['mean( ' name ' ,n) = ' num2str(temp)])
end

if BCs.bc1.type.Periodic && BCs.bc2.type.Periodic
    b.c.vals = subtract_mean(b.c.vals);
    b.n.vals(1:end-1) = subtract_mean(b.n.vals(1:end-1));
    temp = mean(b.c.vals);
    disp(['mean( ' name ' ,c) = ' num2str(temp)])
    temp = mean(b.n.vals);
    disp(['mean( ' name ' ,n) = ' num2str(temp)])
end
end