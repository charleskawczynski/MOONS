function CG = init_CG(x,A)
CG.x.c = x.c;
CG.r.c.vals = x.c.vals*0;
CG.A.c.vals = A.c.vals;
CG.r.c.is_C = x.c.is_CC;

CG.norms.c.L1 = [];
CG.norms.c.L2 = [];
CG.norms.c.Linf = [];


CG.x.n = x.n;
CG.r.n.vals = x.n.vals*0;
CG.A.n.vals = A.n.vals;
CG.r.n.is_C = x.n.is_CC;

CG.norms.n.L1 = [];
CG.norms.n.L2 = [];
CG.norms.n.Linf = [];

end