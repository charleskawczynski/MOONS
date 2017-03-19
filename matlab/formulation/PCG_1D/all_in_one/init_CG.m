function CG = init_CG(x,A)
CG.x.c = x.c;
CG.p.c = x.c;
CG.r.c.vals = x.c.vals*0;
CG.A.c.vals = A.c.vals;
CG.r.c.DL = x.c.DL;

CG.norms.c.L1 = [];
CG.norms.c.L2 = [];
CG.norms.c.Linf = [];


CG.x.n = x.n;
CG.p.n = x.n;
CG.r.n.vals = x.n.vals*0;
CG.A.n.vals = A.n.vals;
CG.r.n.DL = x.n.DL;

CG.x_initial = x;

CG.norms.n.L1 = [];
CG.norms.n.L2 = [];
CG.norms.n.Linf = [];

end