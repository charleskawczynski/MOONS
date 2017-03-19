function CG = soln_CG_CK(CG,operator,operator_explicit,b,N_inner,c)

%% solve CC
x = CG.x.c;
p = CG.p.c;
B = b.c;
vol = c.vol.c;
[x r L1 L2 Linf] = CG_CK_raw(operator,operator_explicit,x,B,p,N_inner,vol,c);
CG.x.c = x;
CG.r.c = r;
CG.p.c = p;
CG.norms.c.L1 = [CG.norms.c.L1; L1];
CG.norms.c.L2 = [CG.norms.c.L2; L2];
CG.norms.c.Linf = [CG.norms.c.Linf; Linf];

%% solve N
x = CG.x.n;
p = CG.p.n;
B = b.n;
vol = c.vol.n;
[x r L1 L2 Linf] = CG_CK_raw(operator,operator_explicit,x,B,p,N_inner,vol,c);
CG.x.n = x;
CG.r.n = r;
CG.p.n = p;
CG.norms.n.L1 = [CG.norms.n.L1; L1];
CG.norms.n.L2 = [CG.norms.n.L2; L2];
CG.norms.n.Linf = [CG.norms.n.Linf; Linf];

end