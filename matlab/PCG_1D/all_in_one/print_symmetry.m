function print_symmetry(A)

Ac = A.c.vals;
An = A.n.vals;
symmetry_c = max(max(abs(Ac - Ac')));
symmetry_n = max(max(abs(An - An')));
disp(['symmetry (c,n) = ' num2str(symmetry_c) ' , ' num2str(symmetry_n)])

end