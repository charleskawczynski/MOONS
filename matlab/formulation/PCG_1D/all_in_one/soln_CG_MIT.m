function CG = soln_CG_MIT(CG,b,N_inner,BCs)

p = getPad(BCs);
s.c = size(CG.x.c.vals,1); z.c = 2:s.c-1;
s.n = size(CG.x.n.vals,1); z.n = 1+p(1):s.n-p(2);

%% solve CC
x = CG.x.c.vals(z.c);
A = CG.A.c.vals(z.c,z.c);
B = b.c.vals(z.c);
[x r L1 L2 Linf] = CG_MIT_raw(x,A,B,N_inner);
CG.x.c.vals(z.c) = x;
CG.r.c.vals(z.c) = r;
CG.norms.c.L1 = [CG.norms.c.L1; L1];
CG.norms.c.L2 = [CG.norms.c.L2; L2];
CG.norms.c.Linf = [CG.norms.c.Linf; Linf];

%% solve N
x = CG.x.n.vals(z.n);
A = CG.A.n.vals(z.n,z.n);
B = b.n.vals(z.n);
[x r L1 L2 Linf] = CG_MIT_raw(x,A,B,N_inner);
CG.x.n.vals(z.n) = x;
CG.r.n.vals(z.n) = r;
CG.norms.n.L1 = [CG.norms.n.L1; L1];
CG.norms.n.L2 = [CG.norms.n.L2; L2];
CG.norms.n.Linf = [CG.norms.n.Linf; Linf];

end