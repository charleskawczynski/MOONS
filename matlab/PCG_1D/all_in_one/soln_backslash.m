function BS = soln_backslash(BS,b,A,c)

p = getPad(BS.x.c.BCs);
s.c = size(BS.x.c.vals,1); z.c = 2:s.c-1;
s.n = size(BS.x.n.vals,1); z.n = 1+p(1):s.n-p(2);

bc = b.c.vals;
bn = b.n.vals;
Ac = A.c.vals;
An = A.n.vals;
BS.x.c.vals = zeros(size(bc));
BS.x.n.vals = zeros(size(bn));
BS.r.c.vals = zeros(size(bc));
BS.r.n.vals = zeros(size(bn));

cond_c = cond(Ac(z.c,z.c));
cond_n = cond(An(z.n,z.n));
disp(['cond     (c,n) = ' num2str(cond_c) ' , ' num2str(cond_n)])

BS.x.c.vals(z.c) = Ac(z.c,z.c)\bc(z.c);
BS.x.n.vals(z.n) = An(z.n,z.n)\bn(z.n);

BS.x.c = apply_BCs(BS.x.c,c,BS.x.c);
BS.x.n = apply_BCs(BS.x.n,c,BS.x.n);

BS.r.c.vals(z.c) = Ac(z.c,z.c)*BS.x.c.vals(z.c)-bc(z.c);
BS.r.n.vals(z.n) = An(z.n,z.n)*BS.x.n.vals(z.n)-bn(z.n);

norms = Ln_norms(BS.r.c.vals);
BS.norms.c.L1 = norms.L1;
BS.norms.c.L2 = norms.L2;
BS.norms.c.Linf = norms.Linf;
norms = Ln_norms(BS.r.n.vals);
BS.norms.n.L1 = norms.L1;
BS.norms.n.L2 = norms.L2;
BS.norms.n.Linf = norms.Linf;

end