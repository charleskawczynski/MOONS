function print_symmetry(A)

Ac = A.c.vals;
An = A.n.vals;
symmetry = max(max(abs(Ac - Ac'))); disp(['symmetry (c) = ' num2str(symmetry)])
symmetry = max(max(abs(An - An'))); disp(['symmetry (n) = ' num2str(symmetry)])

end